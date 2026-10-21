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

module Cursor = struct
  module Raw_slice = struct
    type t =
      { owner : Source_owner.t
      ; start_byte : int
      ; end_byte : int
      ; contents : string
      }

    let owner t = t.owner
    let start_byte t = t.start_byte
    let end_byte t = t.end_byte
    let contents t = t.contents
  end

  module Completed_attribute = struct
    type t =
      { raw_name : Raw_slice.t
      ; raw_value : Raw_slice.t option
      ; decoded : Chatmd_ast.attribute
      }

    let raw_name t = t.raw_name
    let raw_value t = t.raw_value
    let decoded t = t.decoded
  end

  module Value_state = struct
    type t =
      { quote : Chatmd_attribute_lex.Quote.t option
      ; trailing_matching_quote_escape : bool
      ; open_entity : bool
      }

    let quote t = t.quote
    let trailing_matching_quote_escape t = t.trailing_matching_quote_escape
    let open_entity t = t.open_entity
  end

  module Kind = struct
    type t =
      | Tag_name of { closing : bool }
      | Attribute_name of { tag : Chatmd_ast.tag }
      | Attribute_value of
          { tag : Chatmd_ast.tag
          ; attribute : string
          ; value_state : Value_state.t
          }
      | Text
      | Script_body
      | Opaque
  end

  type t =
    { owner : Source_owner.t
    ; cursor_byte : int
    ; raw_prefix : Raw_slice.t
    ; kind : Kind.t
    ; scope : Chatmd_tag_scope.t
    ; completed_attributes : Completed_attribute.t list
    ; uncertain_discriminator : bool
    }

  let owner t = t.owner
  let cursor_byte t = t.cursor_byte
  let raw_prefix t = t.raw_prefix
  let kind t = t.kind
  let scope t = t.scope
  let completed_attributes t = t.completed_attributes
  let uncertain_discriminator t = t.uncertain_discriminator

  module Unavailable = struct
    type t =
      | Invalid_cursor
      | Beyond_scanned_prefix of
          { next_byte : int
          ; reason : Status.reason
          }
    [@@deriving equal, sexp_of]
  end

  type result =
    | Available of t
    | Unavailable of Unavailable.t
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

let raw_slice b start_byte end_byte =
  assert (start_byte >= 0 && end_byte >= start_byte && end_byte <= String.length b.source);
  let remaining = ref (end_byte - start_byte) in
  while !remaining > 0 do
    let step = Int.min 4096 !remaining in
    charge b step;
    b.checkpoint ();
    remaining := !remaining - step
  done;
  { Cursor.Raw_slice.owner = b.owner
  ; start_byte
  ; end_byte
  ; contents = String.sub b.source ~pos:start_byte ~len:(end_byte - start_byte)
  }
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

let cursor_attributes b attributes =
  List.map attributes ~f:(fun attribute ->
    charge b 1;
    let raw_name = Chatmd_attribute_lex.Attribute.raw_name attribute in
    let raw_value = Chatmd_attribute_lex.Attribute.raw_value attribute in
    let slice raw =
      raw_slice
        b
        (Chatmd_attribute_lex.Byte_range.start_byte raw)
        (Chatmd_attribute_lex.Byte_range.end_byte raw)
    in
    { Cursor.Completed_attribute.raw_name = slice raw_name
    ; raw_value = Option.map raw_value ~f:slice
    ; decoded = Chatmd_attribute_lex.Attribute.decoded attribute
    })
;;

let classify_cursor_tag b ~scope ~closing ~name_start ~name_end ~cursor_byte =
  let open Chatmd_attribute_lex.Pending in
  if Int.equal name_start cursor_byte || Int.equal name_end cursor_byte
  then Cursor.Kind.Tag_name { closing }, name_start, [], true
  else if closing
  then Cursor.Kind.Opaque, cursor_byte, [], true
  else (
    let name = String.sub b.source ~pos:name_start ~len:(name_end - name_start) in
    match Chatmd_tag_scope.recognize scope ~name with
    | None -> Cursor.Kind.Text, cursor_byte, [], false
    | Some tag ->
      let parsed =
        match
          Chatmd_attribute_lex.prefix
            ~source:b.source
            ~start_byte:name_end
            ~end_byte:cursor_byte
            ~checkpoint:(fun () -> charge b 1)
        with
        | Ok parsed -> parsed
        | Error _ -> assert false
      in
      let completed =
        cursor_attributes b (Chatmd_attribute_lex.Prefix.completed parsed)
      in
      let pending = Chatmd_attribute_lex.Prefix.pending parsed in
      let uncertain = Chatmd_attribute_lex.Pending.uncertain_discriminator pending in
      let kind, prefix_start =
        match pending with
        | Between_attributes | After_name _ | Before_value _ ->
          (match pending with
           | Before_value { name; _ } ->
             ( Cursor.Kind.Attribute_value
                 { tag
                 ; attribute = name
                 ; value_state =
                     { Cursor.Value_state.quote = None
                     ; trailing_matching_quote_escape = false
                     ; open_entity = false
                     }
                 }
             , cursor_byte )
           | Between_attributes | After_name _ ->
             Cursor.Kind.Attribute_name { tag }, cursor_byte
           | Attribute_name _ | Attribute_value _ | Opaque -> assert false)
        | Attribute_name raw ->
          ( Cursor.Kind.Attribute_name { tag }
          , Chatmd_attribute_lex.Byte_range.start_byte raw )
        | Attribute_value
            { name; raw_value; quote; trailing_matching_quote_escape; open_entity } ->
          let prefix_start =
            Chatmd_attribute_lex.Byte_range.start_byte raw_value
            + if Option.is_some quote then 1 else 0
          in
          ( Cursor.Kind.Attribute_value
              { tag
              ; attribute = name
              ; value_state =
                  { Cursor.Value_state.quote
                  ; trailing_matching_quote_escape
                  ; open_entity
                  }
              }
          , prefix_start )
        | Opaque -> Cursor.Kind.Opaque, cursor_byte
      in
      kind, prefix_start, completed, uncertain)
;;

let scan_chatmd_bytes b ~end_byte ~cursor_result =
  let len = end_byte in
  let i = ref 0 in
  let script_start = ref None in
  let script_chatml = ref false in
  let scope = ref Chatmd_tag_scope.root in
  let untrustworthy_scope = ref false in
  let cursor_kind = ref Cursor.Kind.Text in
  let cursor_prefix_start = ref len in
  let cursor_attributes = ref [] in
  let cursor_uncertain = ref false in
  let starts_here offset needle =
    offset <= len && String.length needle <= len - offset && starts b offset needle
  in
  let script_close_at offset =
    if not (starts_here offset "</script")
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
    if starts_here !i "RAW|"
    then (
      emit b Role.Raw_delimiter !i (!i + 4);
      i := !i + 4;
      let left, right = scan_until ~stops:(fun offset -> starts_here offset "|RAW") in
      emit b Role.Raw_text left right;
      if !i < len
      then (
        emit b Role.Raw_delimiter !i (!i + 4);
        i := !i + 4)
      else (
        issue b Issue.Open_raw_block start len;
        cursor_kind := Cursor.Kind.Opaque))
    else if Option.is_some !script_start && not (script_close_at !i)
    then (
      let left, right =
        scan_until ~stops:(fun offset ->
          starts_here offset "RAW|" || script_close_at offset)
      in
      if !script_chatml
      then scan_chatml_region b ~start_byte:left ~end_byte:right
      else emit b Role.Script_body left right;
      if Int.equal !i len then cursor_kind := Cursor.Kind.Script_body)
    else if starts_here !i "<!--"
    then (
      i := !i + 4;
      while !i < len && not (starts_here !i "-->") do
        Int.incr i
      done;
      if !i < len then i := !i + 3;
      emit b Role.Comment start !i;
      if !i >= len && not (starts_here (Int.max 0 (!i - 3)) "-->")
      then (
        issue b Issue.Open_comment start !i;
        cursor_kind := Cursor.Kind.Opaque))
    else if Char.equal (char b !i) '<'
    then (
      let closing = starts_here !i "</" in
      let name_start = !i + if closing then 2 else 1 in
      if name_start < len && ident_start (char b name_start)
      then (
        let name_end = ref (name_start + 1) in
        while !name_end < len && ident_continue (char b !name_end) do
          Int.incr name_end
        done;
        let j = ref !name_end in
        let quote = ref None in
        let strict_fragment_invalid = ref false in
        while !j < len && (Option.is_some !quote || not (Char.equal (char b !j) '>')) do
          let c = char b !j in
          if Option.is_some !quote && Char.equal c '>'
          then strict_fragment_invalid := true;
          (match !quote with
           | Some q
             when Char.equal c '\\' && !j + 1 < len && Char.equal (char b (!j + 1)) q ->
             Int.incr j
           | Some q when Char.equal c q -> quote := None
           | Some _ -> ()
           | None when Char.equal c '\'' || Char.equal c '"' -> quote := Some c
           | None -> ());
          Int.incr j
        done;
        if !j >= len
        then (
          i := len;
          issue b Issue.Open_tag start len;
          if Option.is_some cursor_result
          then (
            let kind, prefix_start, attributes, uncertain =
              classify_cursor_tag
                b
                ~scope:!scope
                ~closing
                ~name_start
                ~name_end:!name_end
                ~cursor_byte:len
            in
            if !strict_fragment_invalid
            then (
              cursor_kind := Cursor.Kind.Opaque;
              cursor_prefix_start := len;
              cursor_attributes := [];
              cursor_uncertain := false)
            else (
              cursor_kind := kind;
              cursor_prefix_start := prefix_start;
              cursor_attributes := attributes;
              cursor_uncertain := uncertain)))
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
          let self_closing = !j > start && Char.equal b.source.[!j - 1] '/' in
          let attributes =
            if known && not closing
            then (
              match
                Chatmd_attribute_lex.complete_with_ranges
                  ~source:b.source
                  ~start_byte:!name_end
                  ~end_byte:(if self_closing then !j - 1 else !j)
                  ~checkpoint:(fun () -> charge b 1)
              with
              | Ok attributes -> attributes
              | Error _ ->
                untrustworthy_scope := true;
                [])
            else []
          in
          if known
          then (
            emit b Role.Punctuation start name_start;
            emit b Role.Tag_name name_start !name_end;
            List.iter attributes ~f:(fun attribute ->
              charge b 1;
              let raw_name = Chatmd_attribute_lex.Attribute.raw_name attribute in
              let raw_value = Chatmd_attribute_lex.Attribute.raw_value attribute in
              let name, value = Chatmd_attribute_lex.Attribute.decoded attribute in
              if String.equal name "src" then external_source := true;
              if String.equal name "language"
              then
                selected_chatml
                := Option.value_map value ~default:false ~f:(String.equal "chatml");
              emit
                b
                Role.Attribute_name
                (Chatmd_attribute_lex.Byte_range.start_byte raw_name)
                (Chatmd_attribute_lex.Byte_range.end_byte raw_name);
              Option.iter raw_value ~f:(fun value ->
                emit
                  b
                  Role.Attribute_value
                  (Chatmd_attribute_lex.Byte_range.start_byte value)
                  (Chatmd_attribute_lex.Byte_range.end_byte value)));
            emit b Role.Punctuation !j tag_end);
          if !strict_fragment_invalid then untrustworthy_scope := true;
          Option.iter name ~f:(fun name ->
            match Chatmd_tag_scope.recognize !scope ~name with
            | None -> ()
            | Some _ when !untrustworthy_scope -> ()
            | Some tag ->
              if closing
              then (
                if Int.equal !name_end !j
                then (
                  if
                    not
                      (Option.value_map
                         (Chatmd_tag_scope.top !scope)
                         ~default:false
                         ~f:(Chatmd_ast.tag_equal tag))
                  then untrustworthy_scope := true;
                  scope := Chatmd_tag_scope.leave !scope))
              else if not self_closing
              then
                scope
                := (match
                      Chatmd_tag_scope.enter
                        !scope
                        ~tag
                        ~attributes:
                          (List.map attributes ~f:(fun attribute ->
                             charge b 1;
                             Chatmd_attribute_lex.Attribute.decoded attribute))
                        ~max_depth:b.limits.max_nesting
                        ~checkpoint:(fun () -> charge b 1)
                    with
                     | Ok scope -> scope
                     | Error Chatmd_tag_scope.Error.Depth_limit ->
                       raise (Stop Nesting_limit)));
          i := tag_end;
          match name with
          | Some name when String.equal name "script" && known && not !untrustworthy_scope
            ->
            if closing
            then (
              script_start := None;
              script_chatml := false)
            else if !j > start && not (Char.equal b.source.[!j - 1] '/')
            then (
              script_start := Some start;
              script_chatml := !selected_chatml && not !external_source)
          | _ -> ()))
      else (
        if Option.is_some cursor_result && name_start >= len
        then (
          cursor_kind := Cursor.Kind.Tag_name { closing };
          cursor_prefix_start := len;
          cursor_uncertain := true);
        Int.incr i))
    else Int.incr i;
    b.completed <- !i
  done;
  Option.iter !script_start ~f:(fun opened -> issue b Issue.Open_script opened len);
  Option.iter cursor_result ~f:(fun result ->
    let script_scope =
      Option.value_map (Chatmd_tag_scope.top !scope) ~default:false ~f:(fun tag ->
        Chatmd_ast.tag_equal tag Script)
    in
    let partial_script_closer =
      if
        script_scope
        &&
        match !cursor_kind with
        | Cursor.Kind.Text | Script_body -> true
        | Tag_name _ | Attribute_name _ | Attribute_value _ | Opaque -> false
      then (
        let rec find pos =
          if pos < Int.max 0 (len - 16)
          then None
          else if Char.equal (char b pos) '<'
          then (
            let suffix_length = len - pos in
            let expected = "</script" in
            let comparable = Int.min suffix_length (String.length expected) in
            let matched = ref true in
            for offset = 0 to comparable - 1 do
              if not (Char.equal (char b (pos + offset)) expected.[offset])
              then matched := false
            done;
            if !matched && suffix_length >= 2 then Some (pos, suffix_length) else None)
          else find (pos - 1)
        in
        if len = 0 then None else find (len - 1))
      else None
    in
    Option.iter partial_script_closer ~f:(fun (start_byte, suffix_length) ->
      if suffix_length <= String.length "</script"
      then (
        cursor_kind := Cursor.Kind.Tag_name { closing = true };
        cursor_prefix_start := start_byte + 2;
        cursor_uncertain := true)
      else cursor_kind := Cursor.Kind.Opaque);
    let kind =
      if !untrustworthy_scope
      then Cursor.Kind.Opaque
      else (
        match !cursor_kind with
        | Cursor.Kind.Text when script_scope -> Cursor.Kind.Script_body
        | kind -> kind)
    in
    let prefix_start, completed_attributes, uncertain_discriminator =
      match kind with
      | Cursor.Kind.Text | Script_body | Opaque ->
        len, [], !cursor_uncertain || !untrustworthy_scope
      | Tag_name _ | Attribute_name _ | Attribute_value _ ->
        !cursor_prefix_start, !cursor_attributes, !cursor_uncertain
    in
    let prefix = raw_slice b prefix_start len in
    result
    := Some
         { Cursor.owner = b.owner
         ; cursor_byte = len
         ; raw_prefix = prefix
         ; kind
         ; scope = !scope
         ; completed_attributes
         ; uncertain_discriminator
         })
;;

let admit_chatmd input ~view ~limits ~checkpoint =
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
      ( source
      , Source_owner.Chatmd
          { input_identity = Chatmd_parser_input.identity_digest input ~checkpoint
          ; view
          ; view_sha256 = sha256
          } )
;;

let scan_chatmd input ~view ~limits ~checkpoint =
  Result.map (admit_chatmd input ~view ~limits ~checkpoint) ~f:(fun (source, owner) ->
    run source owner limits ~checkpoint (fun b ->
      scan_chatmd_bytes b ~end_byte:(String.length source) ~cursor_result:None))
;;

let scan_chatmd_cursor input ~cursor_byte ~limits ~checkpoint =
  let authored = Chatmd_parser_input.authored input in
  if cursor_byte < 0 || cursor_byte > String.length authored
  then Ok (Cursor.Unavailable Cursor.Unavailable.Invalid_cursor)
  else
    Result.map
      (admit_chatmd input ~view:Source_owner.Chatmd_view.Authored ~limits ~checkpoint)
      ~f:(fun (source, owner) ->
        let result = ref None in
        let snapshot =
          run source owner limits ~checkpoint (fun b ->
            scan_chatmd_bytes b ~end_byte:cursor_byte ~cursor_result:(Some result))
        in
        match snapshot.status with
        | Status.Truncated { next_byte; reason } ->
          Cursor.Unavailable
            (Cursor.Unavailable.Beyond_scanned_prefix { next_byte; reason })
        | Status.Reached_eof ->
          (match !result with
           | Some cursor -> Cursor.Available cursor
           | None -> assert false))
;;
