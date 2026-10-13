open! Core
open Authoring_assistance_base

module Lexical = struct
  module View = struct
    type t =
      | Chatml
      | Chatmd_authored
      | Chatmd_effective
    [@@deriving compare, equal, sexp_of]

    let to_json = function
      | Chatml -> str "chatml"
      | Chatmd_authored -> str "chatmd_authored"
      | Chatmd_effective -> str "chatmd_effective"
    ;;

    let of_json json =
      let* value = C.string ~max_bytes:24 json in
      match value with
      | "chatml" -> Ok Chatml
      | "chatmd_authored" -> Ok Chatmd_authored
      | "chatmd_effective" -> Ok Chatmd_effective
      | _ -> C.invalid "unknown lexical view"
    ;;
  end

  module Request = struct
    type t =
      { context : Context.Expected.t
      ; source : Source.t
      ; view : View.t
      }

    let create ~context ~source ~view = Ok { context; source; view }
    let context t = t.context
    let source t = t.source
    let view t = t.view

    let equal a b =
      Context.Expected.equal a.context b.context
      && Source.equal a.source b.source
      && View.equal a.view b.view
    ;;

    let sexp_of_t _ = Sexp.Atom "authoring_lexical_request"

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "source", Source.to_json t.source
        ; "view", View.to_json t.view
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "context"; "source"; "view" ] json in
      let* context = field fields "context" Context.Expected.of_json in
      let* source = field fields "source" Source.of_json in
      let* view = field fields "view" View.of_json in
      create ~context ~source ~view
    ;;
  end

  module Owner = struct
    type chatmd_view =
      | Authored
      | Effective
    [@@deriving compare, equal, sexp_of]

    type t =
      | Chatml of Source.t
      | Chatmd of
          { captured_source : Source.t
          ; input_identity : Digest.t
          ; view : chatmd_view
          ; view_sha256 : Digest.t
          ; view_byte_length : int
          }

    let chatml source = Chatml source

    let chatmd ~captured_source ~input_identity ~view ~view_sha256 ~view_byte_length =
      let* _ = bounded view_byte_length in
      if
        equal_chatmd_view view Authored
        && ((not (Digest.equal view_sha256 (Source.digest captured_source)))
            || view_byte_length <> Source.byte_length captured_source)
      then C.invalid "authored ChatMD view differs from captured source"
      else
        Ok
          (Chatmd { captured_source; input_identity; view; view_sha256; view_byte_length })
    ;;

    let byte_length = function
      | Chatml source -> Source.byte_length source
      | Chatmd value -> value.view_byte_length
    ;;

    let equal a b =
      match a, b with
      | Chatml x, Chatml y -> Source.equal x y
      | Chatmd x, Chatmd y ->
        Source.equal x.captured_source y.captured_source
        && Digest.equal x.input_identity y.input_identity
        && equal_chatmd_view x.view y.view
        && Digest.equal x.view_sha256 y.view_sha256
        && Int.equal x.view_byte_length y.view_byte_length
      | _ -> false
    ;;

    let captured_source = function
      | Chatml source -> source
      | Chatmd value -> value.captured_source
    ;;

    let view_to_json = function
      | Authored -> str "authored"
      | Effective -> str "effective"
    ;;

    let view_of_json json =
      let* value = C.string ~max_bytes:12 json in
      match value with
      | "authored" -> Ok Authored
      | "effective" -> Ok Effective
      | _ -> C.invalid "unknown lexical ChatMD view"
    ;;

    let to_json = function
      | Chatml source -> obj [ "kind", str "chatml"; "source", Source.to_json source ]
      | Chatmd value ->
        obj
          [ "kind", str "chatmd"
          ; "captured_source", Source.to_json value.captured_source
          ; "input_identity", C.digest_json value.input_identity
          ; "view", view_to_json value.view
          ; "view_sha256", C.digest_json value.view_sha256
          ; "view_byte_length", C.integer value.view_byte_length
          ]
    ;;

    let of_json json =
      let* fields =
        C.fields
          ~allowed:
            [ "kind"
            ; "source"
            ; "captured_source"
            ; "input_identity"
            ; "view"
            ; "view_sha256"
            ; "view_byte_length"
            ]
          json
      in
      let* kind = field fields "kind" (C.string ~max_bytes:12) in
      match kind with
      | "chatml" ->
        let* fields = C.fields ~allowed:[ "kind"; "source" ] json in
        let* source = field fields "source" Source.of_json in
        Ok (chatml source)
      | "chatmd" ->
        let* fields =
          C.fields
            ~allowed:
              [ "kind"
              ; "captured_source"
              ; "input_identity"
              ; "view"
              ; "view_sha256"
              ; "view_byte_length"
              ]
            json
        in
        let* captured_source = field fields "captured_source" Source.of_json in
        let* input_identity = field fields "input_identity" C.digest in
        let* view = field fields "view" view_of_json in
        let* view_sha256 = field fields "view_sha256" C.digest in
        let* view_byte_length = field fields "view_byte_length" C.nonnegative in
        chatmd ~captured_source ~input_identity ~view ~view_sha256 ~view_byte_length
      | _ -> C.invalid "unknown lexical owner"
    ;;
  end

  module Range = struct
    type t =
      { owner : Owner.t
      ; start_byte : int
      ; end_byte : int
      }

    let create ~owner ~start_byte ~end_byte =
      let* _ = bounded start_byte in
      let* _ = bounded end_byte in
      if start_byte < end_byte && end_byte <= Owner.byte_length owner
      then Ok { owner; start_byte; end_byte }
      else C.invalid "lexical range is empty or outside its source view"
    ;;

    let owner t = t.owner
    let start_byte t = t.start_byte
    let end_byte t = t.end_byte

    let to_json t =
      obj
        [ "owner", Owner.to_json t.owner
        ; "start_byte", C.integer t.start_byte
        ; "end_byte", C.integer t.end_byte
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "owner"; "start_byte"; "end_byte" ] json in
      let* owner = field fields "owner" Owner.of_json in
      let* start_byte = field fields "start_byte" C.nonnegative in
      let* end_byte = field fields "end_byte" C.nonnegative in
      create ~owner ~start_byte ~end_byte
    ;;
  end

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

    let entries =
      [ "tag_name", Tag_name
      ; "attribute_name", Attribute_name
      ; "attribute_value", Attribute_value
      ; "keyword", Keyword
      ; "identifier", Identifier
      ; "type_variable", Type_variable
      ; "variant_tag", Variant_tag
      ; "string_literal", String_literal
      ; "number_literal", Number_literal
      ; "boolean_literal", Boolean_literal
      ; "comment", Comment
      ; "raw_delimiter", Raw_delimiter
      ; "raw_text", Raw_text
      ; "script_body", Script_body
      ; "operator", Operator
      ; "punctuation", Punctuation
      ]
    ;;

    let to_json value =
      str
        (List.find_map_exn entries ~f:(fun (name, role) ->
           if equal role value then Some name else None))
    ;;

    let of_json json =
      let* value = C.string ~max_bytes:24 json in
      Option.value_map
        (List.Assoc.find entries value ~equal:String.equal)
        ~default:(C.invalid "unknown lexical role")
        ~f:(fun role -> Ok role)
    ;;
  end

  module Span = struct
    type t =
      { range : Range.t
      ; role : Role.t
      }

    let create ~range ~role = { range; role }
    let range t = t.range
    let role t = t.role
    let to_json t = obj [ "range", Range.to_json t.range; "role", Role.to_json t.role ]

    let of_json json =
      let* fields = C.fields ~allowed:[ "range"; "role" ] json in
      let* range = field fields "range" Range.of_json in
      let* role = field fields "role" Role.of_json in
      Ok (create ~range ~role)
    ;;
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

    let create ~kind ~range = { kind; range }
    let kind t = t.kind
    let range t = t.range

    let kind_to_json = function
      | Open_comment -> str "open_comment"
      | Open_string -> str "open_string"
      | Open_raw_block -> str "open_raw_block"
      | Open_tag -> str "open_tag"
      | Open_script -> str "open_script"
    ;;

    let kind_of_json json =
      let* value = C.string ~max_bytes:20 json in
      match value with
      | "open_comment" -> Ok Open_comment
      | "open_string" -> Ok Open_string
      | "open_raw_block" -> Ok Open_raw_block
      | "open_tag" -> Ok Open_tag
      | "open_script" -> Ok Open_script
      | _ -> C.invalid "unknown lexical issue"
    ;;

    let to_json t = obj [ "kind", kind_to_json t.kind; "range", Range.to_json t.range ]

    let of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "range" ] json in
      let* kind = field fields "kind" kind_of_json in
      let* range = field fields "range" Range.of_json in
      Ok (create ~kind ~range)
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

    let reason_to_json = function
      | Span_limit -> str "span_limit"
      | Issue_limit -> str "issue_limit"
      | Work_limit -> str "work_limit"
      | Nesting_limit -> str "nesting_limit"
    ;;

    let reason_of_json json =
      let* value = C.string ~max_bytes:16 json in
      match value with
      | "span_limit" -> Ok Span_limit
      | "issue_limit" -> Ok Issue_limit
      | "work_limit" -> Ok Work_limit
      | "nesting_limit" -> Ok Nesting_limit
      | _ -> C.invalid "unknown lexical truncation reason"
    ;;

    let to_json = function
      | Reached_eof -> obj [ "kind", str "reached_eof" ]
      | Truncated { next_byte; reason } ->
        obj
          [ "kind", str "truncated"
          ; "next_byte", C.integer next_byte
          ; "reason", reason_to_json reason
          ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "next_byte"; "reason" ] json in
      let* kind = field fields "kind" (C.string ~max_bytes:16) in
      match kind with
      | "reached_eof" ->
        let* _ = C.fields ~allowed:[ "kind" ] json in
        Ok Reached_eof
      | "truncated" ->
        let* next_byte = field fields "next_byte" C.nonnegative in
        let* reason = field fields "reason" reason_of_json in
        Ok (Truncated { next_byte; reason })
      | _ -> C.invalid "unknown lexical status"
    ;;
  end

  module Result = struct
    type t =
      { request : Request.t
      ; owner : Owner.t
      ; spans : Span.t list
      ; issues : Issue.t list
      ; status : Status.t
      }

    let request t = t.request
    let owner t = t.owner
    let spans t = t.spans
    let issues t = t.issues
    let status t = t.status
    let sexp_of_t _ = Sexp.Atom "authoring_lexical_result"

    let to_json t =
      obj
        [ "request", Request.to_json t.request
        ; "owner", Owner.to_json t.owner
        ; "spans", C.array Span.to_json t.spans
        ; "issues", C.array Issue.to_json t.issues
        ; "status", Status.to_json t.status
        ]
    ;;

    let preflight ~request ~owner ~spans ~issues ~status =
      let* budget = C.Budget.create ~max_bytes:Limits.max_result_bytes in
      (* Charge the constant wrapper with empty arrays; each item adds a
         conservative separator plus its full nested JSON cost. The array
         nodes, keys and status boolean/number nodes are not omitted. *)
      let* () =
        C.Budget.charge_json
          budget
          ~parent_depth:0
          (obj
             [ "request", Request.to_json request
             ; "owner", Owner.to_json owner
             ; "spans", `Array []
             ; "issues", `Array []
             ; "status", Status.to_json status
             ])
      in
      let charge encode values =
        List.fold values ~init:(Ok ()) ~f:(fun result value ->
          let* () = result in
          let* () = C.Budget.reserve budget ~bytes:1 ~nodes:0 in
          C.Budget.charge_json budget ~parent_depth:2 (encode value))
      in
      let* () = charge Span.to_json spans in
      charge Issue.to_json issues
    ;;

    let create ~request ~owner ~spans ~issues ~status =
      if not (at_most Limits.max_spans spans && at_most Limits.max_issues issues)
      then C.invalid "too many lexical spans or issues"
      else if not (Source.equal (Owner.captured_source owner) (Request.source request))
      then C.invalid "lexical owner differs from request source"
      else (
        let matching_view =
          match Request.view request, owner with
          | View.Chatml, Owner.Chatml _ -> true
          | View.Chatmd_authored, Owner.Chatmd { view = Owner.Authored; _ }
          | View.Chatmd_effective, Owner.Chatmd { view = Owner.Effective; _ } -> true
          | _ -> false
        in
        if not matching_view
        then C.invalid "lexical owner differs from requested view"
        else (
          let completed =
            match status with
            | Status.Reached_eof -> Ok (Owner.byte_length owner)
            | Truncated { next_byte; _ } ->
              let* _ = bounded next_byte in
              if next_byte <= Owner.byte_length owner
              then Ok next_byte
              else C.invalid "lexical completion exceeds source view"
          in
          let* completed = completed in
          let check_range range =
            Owner.equal (Range.owner range) owner && Range.end_byte range <= completed
          in
          let _, valid_spans =
            List.fold spans ~init:(0, true) ~f:(fun (previous_end, valid) span ->
              let range = Span.range span in
              ( Range.end_byte range
              , valid && check_range range && Range.start_byte range >= previous_end ))
          in
          if
            (not valid_spans)
            || not (List.for_all issues ~f:(fun issue -> check_range (Issue.range issue)))
          then C.invalid "lexical ranges are foreign, unordered or beyond scanned prefix"
          else
            let* () = preflight ~request ~owner ~spans ~issues ~status in
            Ok { request; owner; spans; issues; status }))
    ;;

    let of_json json =
      let* () = C.admit_json ~max_bytes:Limits.max_result_bytes json in
      let* fields =
        C.fields ~allowed:[ "request"; "owner"; "spans"; "issues"; "status" ] json
      in
      let* request = field fields "request" Request.of_json in
      let* owner = field fields "owner" Owner.of_json in
      let* spans =
        field fields "spans" (C.list ~max_items:Limits.max_spans Span.of_json)
      in
      let* issues =
        field fields "issues" (C.list ~max_items:Limits.max_issues Issue.of_json)
      in
      let* status = field fields "status" Status.of_json in
      create ~request ~owner ~spans ~issues ~status
    ;;
  end
end
