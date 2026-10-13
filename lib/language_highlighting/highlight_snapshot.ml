open Core

module Role = struct
  type t =
    | Tag_name
    | Attribute_name
    | Attribute_value
    | Keyword
    | Identifier
    | Type_variable
    | Variant_tag
    | String_literal
    | Number_literal
    | Boolean_literal
    | Comment
    | Raw_delimiter
    | Raw_text
    | Script_body
    | Operator
    | Punctuation
  [@@deriving compare, equal, sexp_of]
end

module Source_owner = struct
  module Chatmd_view = struct
    type t =
      | Authored
      | Effective
    [@@deriving compare, equal, sexp_of]
  end

  type t =
    | Chatml of
        { source_id : Chatml_source.Id.t
        ; source_digest : Chatml_source.Digest.t
        }
    | Chatmd of
        { input_identity : string
        ; view : Chatmd_view.t
        ; view_sha256 : string
        }
  [@@deriving sexp_of]

  let equal a b =
    match a, b with
    | Chatml a, Chatml b ->
      Chatml_source.Id.equal a.source_id b.source_id
      && Chatml_source.Digest.equal a.source_digest b.source_digest
    | Chatmd a, Chatmd b ->
      String.equal a.input_identity b.input_identity
      && Chatmd_view.equal a.view b.view
      && String.equal a.view_sha256 b.view_sha256
    | Chatml _, Chatmd _ | Chatmd _, Chatml _ -> false
  ;;
end

module Range = struct
  type t =
    { owner : Source_owner.t
    ; start_byte : int
    ; end_byte : int
    }

  let owner t = t.owner
  let start_byte t = t.start_byte
  let end_byte t = t.end_byte
end

module Span = struct
  type t =
    { range : Range.t
    ; role : Role.t
    ; snapshot_token : unit ref
    }

  let range t = t.range
  let role t = t.role
end

module Limits = struct
  type t =
    { max_source_bytes : int
    ; max_identity_bytes : int
    ; max_spans : int
    ; max_issues : int
    ; max_work_units : int
    ; max_nesting : int
    }

  let create
        ~max_source_bytes
        ~max_identity_bytes
        ~max_spans
        ~max_issues
        ~max_work_units
        ~max_nesting
    =
    if
      List.for_all
        [ max_source_bytes
        ; max_identity_bytes
        ; max_spans
        ; max_issues
        ; max_work_units
        ; max_nesting
        ]
        ~f:(fun n -> n > 0)
    then
      Ok
        { max_source_bytes
        ; max_identity_bytes
        ; max_spans
        ; max_issues
        ; max_work_units
        ; max_nesting
        }
    else Or_error.error_string "highlight limits must be positive"
  ;;
end

module Status = struct
  type reason =
    | Span_limit
    | Issue_limit
    | Work_limit
    | Nesting_limit
  [@@deriving compare, equal, sexp_of]

  type t =
    | Reached_eof
    | Truncated of
        { next_byte : int
        ; reason : reason
        }
  [@@deriving compare, equal, sexp_of]
end

module Issue = struct
  type kind =
    | Open_comment
    | Open_string
    | Open_raw_block
    | Open_tag
    | Open_script
  [@@deriving compare, equal, sexp_of]

  type t =
    { kind : kind
    ; range : Range.t
    }

  let kind t = t.kind
  let range t = t.range
end

module Error = struct
  type t =
    | Source_too_large
    | Identity_too_large
  [@@deriving compare, equal, sexp_of]
end

type t =
  { owner : Source_owner.t
  ; source : string
  ; source_byte_length : int
  ; spans : Span.t list
  ; issues : Issue.t list
  ; status : Status.t
  ; snapshot_token : unit ref
  }

let version _ = 1
let source_owner t = t.owner
let source_byte_length t = t.source_byte_length
let source_contents t = t.source
let spans t = t.spans
let issues t = t.issues
let status t = t.status
let contains_span t span = phys_equal t.snapshot_token span.Span.snapshot_token

exception Stop of Status.reason

type builder =
  { source : string
  ; owner : Source_owner.t
  ; limits : Limits.t
  ; checkpoint : unit -> unit
  ; token : unit ref
  ; mutable work : int
  ; mutable completed : int
  ; mutable span_count : int
  ; mutable spans_rev : Span.t list
  ; mutable issue_count : int
  ; mutable issues_rev : Issue.t list
  }

let charge b units =
  if units > b.limits.max_work_units - b.work then raise (Stop Work_limit);
  b.work <- b.work + units;
  if b.work mod 4096 = 0 then b.checkpoint ()
;;

let char b i =
  charge b 1;
  b.source.[i]
;;

let starts b i needle =
  let source_len = String.length b.source in
  let needle_len = String.length needle in
  if needle_len > source_len - i
  then false
  else (
    let matched = ref true in
    let j = ref 0 in
    while !matched && !j < needle_len do
      matched := Char.equal (char b (i + !j)) needle.[!j];
      Int.incr j
    done;
    !matched)
;;

let range b start_byte end_byte =
  assert (start_byte >= 0 && end_byte > start_byte && end_byte <= String.length b.source);
  { Range.owner = b.owner; start_byte; end_byte }
;;

let emit b role start_byte end_byte =
  if start_byte < end_byte
  then (
    if b.span_count >= b.limits.max_spans then raise (Stop Span_limit);
    charge b 1;
    b.spans_rev
    <- { Span.range = range b start_byte end_byte; role; snapshot_token = b.token }
       :: b.spans_rev;
    b.span_count <- b.span_count + 1)
;;

let issue b kind start_byte end_byte =
  if start_byte < end_byte
  then (
    if b.issue_count >= b.limits.max_issues then raise (Stop Issue_limit);
    charge b 1;
    b.issues_rev <- { Issue.kind; range = range b start_byte end_byte } :: b.issues_rev;
    b.issue_count <- b.issue_count + 1)
;;

let finish b status =
  { owner = b.owner
  ; source = b.source
  ; source_byte_length = String.length b.source
  ; spans = List.rev b.spans_rev
  ; issues = List.rev b.issues_rev
  ; status
  ; snapshot_token = b.token
  }
;;

let run source owner limits ~checkpoint scan =
  checkpoint ();
  let b =
    { source
    ; owner
    ; limits
    ; checkpoint
    ; token = ref ()
    ; work = 0
    ; completed = 0
    ; span_count = 0
    ; spans_rev = []
    ; issue_count = 0
    ; issues_rev = []
    }
  in
  let status =
    try
      scan b;
      Status.Reached_eof
    with
    | Stop reason ->
      b.spans_rev
      <- List.drop_while b.spans_rev ~f:(fun span ->
           span.Span.range.Range.end_byte > b.completed);
      b.issues_rev
      <- List.filter b.issues_rev ~f:(fun issue ->
           issue.Issue.range.Range.end_byte <= b.completed);
      Truncated { next_byte = b.completed; reason }
  in
  finish b status
;;

let ascii_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let ident_start = function
  | '_' -> true
  | c -> ascii_alpha c
;;

let ident_continue c = ident_start c || digit c

let lower_ident_start = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let chatml_keyword = function
  | "fun"
  | "if"
  | "then"
  | "else"
  | "while"
  | "do"
  | "done"
  | "let"
  | "let*"
  | "let+"
  | "in"
  | "match"
  | "with"
  | "module"
  | "struct"
  | "end"
  | "open"
  | "ref"
  | "rec"
  | "and"
  | "type"
  | "sig"
  | "val"
  | "where" -> true
  | _ -> false
;;

let scan_chatml_region b ~start_byte ~end_byte =
  let len = end_byte in
  let i = ref start_byte in
  let starts_here offset needle =
    String.length needle <= len - offset && starts b offset needle
  in
  while !i < len do
    let start = !i in
    let current = char b !i in
    if starts_here !i "(*"
    then (
      i := !i + 2;
      let depth = ref 1 in
      while !i < len && !depth > 0 do
        if starts_here !i "(*"
        then (
          if !depth >= b.limits.max_nesting then raise (Stop Nesting_limit);
          Int.incr depth;
          i := !i + 2)
        else if starts_here !i "*)"
        then (
          Int.decr depth;
          i := !i + 2)
        else Int.incr i
      done;
      emit b Role.Comment start !i;
      if !depth > 0 then issue b Issue.Open_comment start !i)
    else if Char.equal current '"'
    then (
      Int.incr i;
      let closed = ref false in
      while !i < len && not !closed do
        let c = char b !i in
        if Char.equal c '\\' && !i + 1 < len
        then i := !i + 2
        else if Char.equal c '"'
        then (
          Int.incr i;
          closed := true)
        else Int.incr i
      done;
      emit b Role.String_literal start !i;
      if not !closed then issue b Issue.Open_string start !i)
    else if Char.equal current '`' && !i + 1 < len && ident_continue (char b (!i + 1))
    then (
      i := !i + 2;
      while !i < len && ident_continue (char b !i) do
        Int.incr i
      done;
      emit b Role.Variant_tag start !i)
    else if Char.equal current '\'' && !i + 1 < len && lower_ident_start (char b (!i + 1))
    then (
      i := !i + 2;
      while !i < len && ident_continue (char b !i) do
        Int.incr i
      done;
      emit b Role.Type_variable start !i)
    else if ident_start current
    then (
      Int.incr i;
      while !i < len && ident_continue (char b !i) do
        Int.incr i
      done;
      let width = !i - start in
      let role =
        if width <= 8
        then (
          let word = String.sub b.source ~pos:start ~len:width in
          if
            String.equal word "let"
            && !i < len
            && (Char.equal (char b !i) '*' || Char.equal (char b !i) '+')
          then (
            Int.incr i;
            Role.Keyword)
          else if String.equal word "true" || String.equal word "false"
          then Role.Boolean_literal
          else if chatml_keyword word
          then Role.Keyword
          else if String.equal word "_"
          then Role.Punctuation
          else Role.Identifier)
        else Role.Identifier
      in
      emit b role start !i)
    else if digit current
    then (
      Int.incr i;
      while !i < len && digit (char b !i) do
        Int.incr i
      done;
      if !i + 1 < len && Char.equal (char b !i) '.' && digit (char b (!i + 1))
      then (
        Int.incr i;
        while !i < len && digit (char b !i) do
          Int.incr i
        done);
      emit b Role.Number_literal start !i)
    else (
      let multi =
        List.find
          [ "<=."
          ; ">=."
          ; "->"
          ; "<-"
          ; ":="
          ; "!="
          ; "=="
          ; "<="
          ; ">="
          ; "<."
          ; ">."
          ; "++"
          ; "+."
          ; "-."
          ; "*."
          ; "/."
          ; ";;"
          ; "(|"
          ; "|)"
          ]
          ~f:(starts_here !i)
      in
      let width = Option.value_map multi ~default:1 ~f:String.length in
      i := !i + width;
      let role =
        match current with
        | '+' | '-' | '*' | '/' | '<' | '>' | '=' | ':' | '!' -> Some Role.Operator
        | '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',' | '.' | '|' | '_' ->
          Some Role.Punctuation
        | _ -> None
      in
      Option.iter role ~f:(fun role -> emit b role start !i));
    b.completed <- !i
  done
;;

let scan_chatml source ~limits ~checkpoint =
  let bytes = Chatml_source.contents source in
  if String.length bytes > limits.Limits.max_source_bytes
  then Error Error.Source_too_large
  else (
    let id = Chatml_source.id source in
    if
      String.length (Chatml_source.Id.package_namespace id)
      > limits.max_identity_bytes - String.length (Chatml_source.Id.relative_path id)
    then Error Error.Identity_too_large
    else
      Ok
        (run
           bytes
           (Source_owner.Chatml
              { source_id = id; source_digest = Chatml_source.digest source })
           limits
           ~checkpoint
           (fun b ->
              scan_chatml_region b ~start_byte:0 ~end_byte:(String.length b.source))))
;;

let scan_chatmd_bytes b =
  let len = String.length b.source in
  let i = ref 0 in
  let script_start = ref None in
  let script_chatml = ref false in
  let script_close_at offset =
    if not (starts b offset "</script")
    then false
    else (
      let after_name = offset + String.length "</script" in
      let cursor = ref after_name in
      while !cursor < len && Char.is_whitespace (char b !cursor) do
        Int.incr cursor
      done;
      !cursor < len && Char.equal (char b !cursor) '>')
  in
  let scan_until ~stops =
    let start = !i in
    while !i < len && not (stops !i) do
      Int.incr i
    done;
    start, !i
  in
  while !i < len do
    let start = !i in
    if starts b !i "RAW|"
    then (
      emit b Role.Raw_delimiter !i (!i + 4);
      i := !i + 4;
      let left, right = scan_until ~stops:(fun offset -> starts b offset "|RAW") in
      emit b Role.Raw_text left right;
      if !i < len
      then (
        emit b Role.Raw_delimiter !i (!i + 4);
        i := !i + 4)
      else issue b Issue.Open_raw_block start len)
    else if Option.is_some !script_start && not (script_close_at !i)
    then (
      let left, right =
        scan_until ~stops:(fun offset -> starts b offset "RAW|" || script_close_at offset)
      in
      if !script_chatml
      then scan_chatml_region b ~start_byte:left ~end_byte:right
      else emit b Role.Script_body left right)
    else if starts b !i "<!--"
    then (
      i := !i + 4;
      while !i < len && not (starts b !i "-->") do
        Int.incr i
      done;
      if !i < len then i := !i + 3;
      emit b Role.Comment start !i;
      if !i >= len && not (starts b (Int.max 0 (!i - 3)) "-->")
      then issue b Issue.Open_comment start !i)
    else if Char.equal (char b !i) '<'
    then (
      let closing = starts b !i "</" in
      let name_start = !i + if closing then 2 else 1 in
      if name_start < len && ident_start (char b name_start)
      then (
        let name_end = ref (name_start + 1) in
        while !name_end < len && ident_continue (char b !name_end) do
          Int.incr name_end
        done;
        let j = ref !name_end in
        let quote = ref None in
        while !j < len && (Option.is_some !quote || not (Char.equal (char b !j) '>')) do
          let c = char b !j in
          (match !quote with
           | Some q when Char.equal c '\\' && !j + 1 < len -> Int.incr j
           | Some q when Char.equal c q -> quote := None
           | Some _ -> ()
           | None when Char.equal c '\'' || Char.equal c '"' -> quote := Some c
           | None -> ());
          Int.incr j
        done;
        if !j >= len
        then (
          i := len;
          issue b Issue.Open_tag start len)
        else (
          let tag_end = !j + 1 in
          let width = !name_end - name_start in
          let name =
            if width <= 32
            then Some (String.sub b.source ~pos:name_start ~len:width)
            else None
          in
          let known =
            Option.value_map name ~default:false ~f:(fun name ->
              Option.is_some (Chatmd_ast.tag_of_string_opt name))
          in
          let selected_chatml = ref false in
          let external_source = ref false in
          if known
          then (
            emit b Role.Punctuation start name_start;
            emit b Role.Tag_name name_start !name_end;
            let attr = ref !name_end in
            while !attr < !j do
              let c = char b !attr in
              if Char.is_whitespace c || Char.equal c '/'
              then Int.incr attr
              else if Char.equal c '='
              then Int.incr attr
              else (
                let attr_start = !attr in
                while
                  !attr < !j
                  && not
                       (let c = char b !attr in
                        Char.is_whitespace c || Char.equal c '=' || Char.equal c '/')
                do
                  Int.incr attr
                done;
                let attr_name =
                  if !attr - attr_start <= 8
                  then
                    Some (String.sub b.source ~pos:attr_start ~len:(!attr - attr_start))
                  else None
                in
                if Option.value_map attr_name ~default:false ~f:(String.equal "src")
                then external_source := true;
                emit b Role.Attribute_name attr_start !attr;
                while !attr < !j && Char.is_whitespace (char b !attr) do
                  Int.incr attr
                done;
                if !attr < !j && Char.equal (char b !attr) '='
                then (
                  Int.incr attr;
                  while !attr < !j && Char.is_whitespace (char b !attr) do
                    Int.incr attr
                  done;
                  if !attr < !j
                  then (
                    let value_start = !attr in
                    let q = char b !attr in
                    if Char.equal q '\'' || Char.equal q '"'
                    then (
                      Int.incr attr;
                      while !attr < !j && not (Char.equal (char b !attr) q) do
                        if Char.equal (char b !attr) '\\' && !attr + 1 < !j
                        then Int.incr attr;
                        Int.incr attr
                      done;
                      if !attr < !j then Int.incr attr)
                    else
                      while
                        !attr < !j
                        && not
                             (let c = char b !attr in
                              Char.is_whitespace c || Char.equal c '/')
                      do
                        Int.incr attr
                      done;
                    if
                      Option.value_map
                        attr_name
                        ~default:false
                        ~f:(String.equal "language")
                    then (
                      let value =
                        if !attr - value_start <= 8
                        then
                          Some
                            (String.sub
                               b.source
                               ~pos:value_start
                               ~len:(!attr - value_start))
                        else None
                      in
                      selected_chatml
                      := Option.value_map value ~default:false ~f:(fun value ->
                           String.equal value "chatml"
                           || String.equal value "\"chatml\""
                           || String.equal value "'chatml'"));
                    emit b Role.Attribute_value value_start !attr)))
            done;
            emit b Role.Punctuation !j tag_end);
          i := tag_end;
          match name with
          | Some name when String.equal name "script" && known ->
            if closing
            then (
              script_start := None;
              script_chatml := false)
            else if !j > start && not (Char.equal b.source.[!j - 1] '/')
            then (
              script_start := Some start;
              script_chatml := !selected_chatml && not !external_source)
          | _ -> ()))
      else Int.incr i)
    else Int.incr i;
    b.completed <- !i
  done;
  Option.iter !script_start ~f:(fun opened -> issue b Issue.Open_script opened len)
;;

let scan_chatmd input ~view ~limits ~checkpoint =
  let source, sha256 =
    match view with
    | Source_owner.Chatmd_view.Authored ->
      Chatmd_parser_input.authored input, Chatmd_parser_input.authored_sha256 input
    | Effective ->
      Chatmd_parser_input.effective input, Chatmd_parser_input.effective_sha256 input
  in
  if
    String.length (Chatmd_parser_input.authored input) > limits.Limits.max_source_bytes
    || String.length (Chatmd_parser_input.effective input) > limits.max_source_bytes
  then Error Error.Source_too_large
  else if String.length (Chatmd_parser_input.path input) > limits.max_identity_bytes
  then Error Error.Identity_too_large
  else
    Ok
      (run
         source
         (Source_owner.Chatmd
            { input_identity = Chatmd_parser_input.identity_digest input ~checkpoint
            ; view
            ; view_sha256 = sha256
            })
         limits
         ~checkpoint
         scan_chatmd_bytes)
;;
