open! Core
module Highlight = Language_highlighting.Highlight_snapshot

module Item = struct
  type origin =
    | Command
    | Keyword
    | Accepted_binding
    | Session_binding
  [@@deriving equal, sexp_of]

  type t =
    { label : string
    ; origin : origin
    }

  let label t = t.label
  let origin t = t.origin
end

type t =
  { source : string
  ; cursor_byte : int
  ; start_byte : int
  ; end_byte : int
  ; items : Item.t list
  ; selected_index : int
  }

let max_source_bytes = 65_536
let max_items = 16

let lexical_limits =
  Highlight.Limits.create
    ~max_source_bytes
    ~max_identity_bytes:256
    ~max_spans:16_384
    ~max_issues:32
    ~max_work_units:1_048_576
    ~max_nesting:128
  |> Or_error.ok_exn
;;

let source_id =
  Chatml_source.Id.create ~package_namespace:"repl-terminal" ~relative_path:"draft.chatml"
  |> Or_error.ok_exn
;;

let ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

let valid_name name =
  let bytes = String.length name in
  bytes > 0
  && bytes <= 128
  && ident_start name.[0]
  && String.for_all name ~f:ident_continue
;;

let keywords =
  [ "and"
  ; "do"
  ; "done"
  ; "else"
  ; "end"
  ; "false"
  ; "fun"
  ; "if"
  ; "in"
  ; "let"
  ; "match"
  ; "module"
  ; "open"
  ; "rec"
  ; "ref"
  ; "sig"
  ; "struct"
  ; "then"
  ; "true"
  ; "type"
  ; "val"
  ; "where"
  ; "while"
  ; "with"
  ]
;;

let in_code snapshot cursor =
  let scanned =
    match Highlight.status snapshot with
    | Reached_eof -> true
    | Truncated { next_byte; reason = _ } -> cursor < next_byte
  in
  scanned
  && (not
        (List.exists (Highlight.spans snapshot) ~f:(fun span ->
           let range = Highlight.Span.range span in
           Highlight.Range.start_byte range < cursor
           && cursor < Highlight.Range.end_byte range
           &&
           match Highlight.Span.role span with
           | Comment | String_literal | Number_literal | Type_variable | Variant_tag ->
             true
           | Tag_name
           | Attribute_name
           | Attribute_value
           | Keyword
           | Identifier
           | Boolean_literal
           | Raw_delimiter
           | Raw_text
           | Script_body
           | Operator
           | Punctuation -> false)))
  && not
       (List.exists (Highlight.issues snapshot) ~f:(fun issue ->
          Int.equal cursor (Highlight.Range.end_byte (Highlight.Issue.range issue))
          &&
          match Highlight.Issue.kind issue with
          | Open_comment | Open_string -> true
          | Open_raw_block | Open_tag | Open_script -> false))
;;

let lexical_context source cursor_byte =
  match
    Chatml_source.create
      source_id
      ~contents:source
      ~expected_digest:None
      ~max_bytes:(Some max_source_bytes)
      ~checkpoint:Fn.id
  with
  | Error _ -> false
  | Ok captured ->
    (match Highlight.scan_chatml captured ~limits:lexical_limits ~checkpoint:Fn.id with
     | Error _ -> false
     | Ok snapshot -> in_code snapshot cursor_byte)
;;

let catalog labels origin = List.map labels ~f:(fun label -> { Item.label; origin })

let skip_whitespace_before source byte =
  let byte = ref byte in
  while !byte > 0 && Char.is_whitespace source.[!byte - 1] do
    decr byte
  done;
  !byte
;;

let opaque_identifier_prefix source first =
  first > 0 && (Char.equal source.[first - 1] '\'' || Char.equal source.[first - 1] '`')
;;

let query_with_session_names ~source ~cursor_byte ~binding_names ~session_names =
  let source_bytes = String.length source in
  if
    source_bytes > max_source_bytes
    || cursor_byte < 0
    || cursor_byte > source_bytes
    || not (lexical_context source cursor_byte)
  then None
  else (
    let first = ref cursor_byte in
    while !first > 0 && ident_continue source.[!first - 1] do
      decr first
    done;
    let last = ref cursor_byte in
    while !last < source_bytes && ident_continue source.[!last] do
      incr last
    done;
    let line_start =
      match
        String.rfindi (String.prefix source !first) ~f:(fun _ c -> Char.equal c '\n')
      with
      | None -> 0
      | Some index -> index + 1
    in
    let leading = String.sub source ~pos:line_start ~len:(!first - line_start) in
    let start_byte, choices =
      if opaque_identifier_prefix source !first
      then !first, []
      else if
        String.equal (String.strip leading) ":"
        && !first > 0
        && Char.equal source.[!first - 1] ':'
      then !first - 1, catalog Help.command_names Command
      else if String.equal (String.strip leading) ":help"
      then !first, catalog Help.topics Command
      else if
        let before = skip_whitespace_before source !first in
        before > 0 && Char.equal source.[before - 1] '.'
      then
        ( !first
        , catalog
            (List.take session_names 16 |> List.filter ~f:valid_name)
            Session_binding )
      else
        ( !first
        , catalog
            (List.take session_names 16 |> List.filter ~f:valid_name)
            Session_binding
          @ catalog
              (List.take binding_names 128 |> List.filter ~f:valid_name)
              Accepted_binding
          @ catalog keywords Keyword )
    in
    let prefix = String.sub source ~pos:start_byte ~len:(cursor_byte - start_byte) in
    let seen = String.Hash_set.create () in
    let items =
      if String.is_empty prefix && List.is_empty session_names
      then []
      else
        choices
        |> List.filter ~f:(fun item -> String.is_prefix item.Item.label ~prefix)
        |> List.filter ~f:(fun item ->
          if Hash_set.mem seen item.Item.label
          then false
          else (
            Hash_set.add seen item.Item.label;
            true))
        |> fun items -> List.take items max_items
    in
    if List.is_empty items
    then None
    else
      Some
        { source; cursor_byte; start_byte; end_byte = !last; items; selected_index = 0 })
;;

let query ~source ~cursor_byte ~binding_names =
  query_with_session_names ~source ~cursor_byte ~binding_names ~session_names:[]
;;

module Context = struct
  type t =
    { source : string
    ; cursor_byte : int
    ; path : string list
    ; prefix : string
    }

  let path t = t.path
  let prefix t = t.prefix

  let equal left right =
    Int.equal left.cursor_byte right.cursor_byte
    && String.equal left.source right.source
    && List.equal String.equal left.path right.path
    && String.equal left.prefix right.prefix
  ;;
end

let context ~source ~cursor_byte =
  let bytes = String.length source in
  if
    bytes > max_source_bytes
    || cursor_byte < 0
    || cursor_byte > bytes
    || not (lexical_context source cursor_byte)
  then None
  else (
    let first = ref cursor_byte in
    while !first > 0 && ident_continue source.[!first - 1] do
      decr first
    done;
    let prefix = String.sub source ~pos:!first ~len:(cursor_byte - !first) in
    let line_start =
      match
        String.rfindi (String.prefix source !first) ~f:(fun _ c -> Char.equal c '\n')
      with
      | None -> 0
      | Some index -> index + 1
    in
    let leading =
      String.sub source ~pos:line_start ~len:(!first - line_start) |> String.strip
    in
    if
      opaque_identifier_prefix source !first
      || String.is_prefix leading ~prefix:":"
      || ((not (String.is_empty prefix)) && not (valid_name prefix))
    then None
    else (
      let rec path before depth names =
        let before = skip_whitespace_before source before in
        if before > 0 && Char.equal source.[before - 1] '.'
        then (
          let last = skip_whitespace_before source (before - 1) in
          let first = ref last in
          while !first > 0 && ident_continue source.[!first - 1] do
            decr first
          done;
          let name = String.sub source ~pos:!first ~len:(last - !first) in
          if depth >= 16 || not (valid_name name)
          then None
          else path !first (depth + 1) (name :: names))
        else Some names
      in
      match path !first 0 [] with
      | None -> None
      | Some [] when String.is_empty prefix -> None
      | Some path -> Some { Context.source; cursor_byte; path; prefix }))
;;

let items t = t.items
let selected_index t = t.selected_index
let selected t = List.nth_exn t.items t.selected_index
let next t = { t with selected_index = (t.selected_index + 1) % List.length t.items }

let previous t =
  { t with
    selected_index = (t.selected_index + List.length t.items - 1) % List.length t.items
  }
;;

let accept t ~source ~cursor_byte =
  if not (String.equal t.source source && Int.equal t.cursor_byte cursor_byte)
  then None
  else (
    let insertion = Item.label (selected t) in
    Some
      ( String.prefix source t.start_byte
        ^ insertion
        ^ String.drop_prefix source t.end_byte
      , t.start_byte + String.length insertion ))
;;
