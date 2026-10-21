open! Core
open Authoring_assistance_base

module Assistance = struct
  module Request = struct
    type t =
      { context : Context.Expected.t
      ; source : Source.t
      ; cursor_byte : int
      ; max_items : int
      }

    let create ~context ~source ~cursor_byte ~max_items =
      let* _ = bounded cursor_byte in
      if cursor_byte > Source.byte_length source
      then C.invalid "assistance cursor exceeds source"
      else if max_items <= 0 || max_items > Limits.max_suggestions
      then C.invalid "invalid assistance item limit"
      else Ok { context; source; cursor_byte; max_items }
    ;;

    let context t = t.context
    let source t = t.source
    let cursor_byte t = t.cursor_byte
    let max_items t = t.max_items

    let equal a b =
      Context.Expected.equal a.context b.context
      && Source.equal a.source b.source
      && Int.equal a.cursor_byte b.cursor_byte
      && Int.equal a.max_items b.max_items
    ;;

    let sexp_of_t _ = Sexp.Atom "authoring_assistance_request"

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "source", Source.to_json t.source
        ; "cursor_byte", C.integer t.cursor_byte
        ; "max_items", C.integer t.max_items
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "context"; "source"; "cursor_byte"; "max_items" ] json
      in
      let* context = field fields "context" Context.Expected.of_json in
      let* source = field fields "source" Source.of_json in
      let* cursor_byte = field fields "cursor_byte" C.nonnegative in
      let* max_items = field fields "max_items" C.positive in
      create ~context ~source ~cursor_byte ~max_items
    ;;
  end

  module Selected_catalog = struct
    type t =
      { environment : string
      ; input_fingerprint : Digest.t
      ; compiler_contract : Digest.t
      }
    [@@deriving sexp_of]

    let create ~environment ~input_fingerprint ~compiler_contract =
      let* _ = text ~max:256 environment in
      if String.is_empty environment
      then C.invalid "empty selected environment"
      else Ok { environment; input_fingerprint; compiler_contract }
    ;;

    let environment t = t.environment
    let input_fingerprint t = t.input_fingerprint
    let compiler_contract t = t.compiler_contract

    let to_json t =
      obj
        [ "environment", str t.environment
        ; "input_fingerprint", C.digest_json t.input_fingerprint
        ; "compiler_contract", C.digest_json t.compiler_contract
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "environment"; "input_fingerprint"; "compiler_contract" ] json
      in
      let* environment = field fields "environment" (C.utf8_text ~max_bytes:256) in
      let* input_fingerprint = field fields "input_fingerprint" C.digest in
      let* compiler_contract = field fields "compiler_contract" C.digest in
      create ~environment ~input_fingerprint ~compiler_contract
    ;;
  end

  module Checked_template = struct
    type target =
      | One_off_v1
      | Tool_v1
      | Developer_tool_v1
      | Moderator_v1
      | Delegated_moderator_v1
    [@@deriving equal, sexp_of]

    type t =
      { selected : Selected_catalog.t
      ; target : target
      ; example_digest : Digest.t
      ; checked_input_fingerprint : Digest.t
      }
    [@@deriving sexp_of]

    let create ~selected ~target ~example_digest ~checked_input_fingerprint =
      Ok { selected; target; example_digest; checked_input_fingerprint }
    ;;

    let selected t = t.selected
    let target t = t.target
    let example_digest t = t.example_digest
    let checked_input_fingerprint t = t.checked_input_fingerprint

    let target_to_json = function
      | One_off_v1 -> str "one_off_v1"
      | Tool_v1 -> str "tool_v1"
      | Developer_tool_v1 -> str "developer_tool_v1"
      | Moderator_v1 -> str "moderator_v1"
      | Delegated_moderator_v1 -> str "delegated_moderator_v1"
    ;;

    let target_of_json json =
      let* name = C.string ~max_bytes:32 json in
      match name with
      | "one_off_v1" -> Ok One_off_v1
      | "tool_v1" -> Ok Tool_v1
      | "developer_tool_v1" -> Ok Developer_tool_v1
      | "moderator_v1" -> Ok Moderator_v1
      | "delegated_moderator_v1" -> Ok Delegated_moderator_v1
      | _ -> C.invalid "unknown checked template target"
    ;;

    let to_json t =
      obj
        [ "selected", Selected_catalog.to_json t.selected
        ; "target", target_to_json t.target
        ; "example_digest", C.digest_json t.example_digest
        ; "checked_input_fingerprint", C.digest_json t.checked_input_fingerprint
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields
          ~allowed:[ "selected"; "target"; "example_digest"; "checked_input_fingerprint" ]
          json
      in
      let* selected = field fields "selected" Selected_catalog.of_json in
      let* target = field fields "target" target_of_json in
      let* example_digest = field fields "example_digest" C.digest in
      let* checked_input_fingerprint =
        field fields "checked_input_fingerprint" C.digest
      in
      create ~selected ~target ~example_digest ~checked_input_fingerprint
    ;;
  end

  module Selected_tool = struct
    type t =
      { capability_id : Id.Capability.t
      ; binding_fingerprint : Digest.t
      ; selected_registry_fingerprint : Digest.t
      }
    [@@deriving sexp_of]

    let create ~capability_id ~binding_fingerprint ~selected_registry_fingerprint =
      let* _ = Id.Capability.of_json (Id.Capability.to_json capability_id) in
      Ok { capability_id; binding_fingerprint; selected_registry_fingerprint }
    ;;

    let capability_id t = t.capability_id
    let binding_fingerprint t = t.binding_fingerprint
    let selected_registry_fingerprint t = t.selected_registry_fingerprint

    let to_json t =
      obj
        [ "capability_id", Id.Capability.to_json t.capability_id
        ; "binding_fingerprint", C.digest_json t.binding_fingerprint
        ; "selected_registry_fingerprint", C.digest_json t.selected_registry_fingerprint
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields
          ~allowed:
            [ "capability_id"; "binding_fingerprint"; "selected_registry_fingerprint" ]
          json
      in
      let* capability_id = field fields "capability_id" Id.Capability.of_json in
      let* binding_fingerprint = field fields "binding_fingerprint" C.digest in
      let* selected_registry_fingerprint =
        field fields "selected_registry_fingerprint" C.digest
      in
      create ~capability_id ~binding_fingerprint ~selected_registry_fingerprint
    ;;
  end

  module Range = Authoring_analysis.Byte_range

  module Suggestion = struct
    type kind =
      | Tag
      | Attribute
      | Value
      | Reference
      | Path
      | Template
      | Environment
    [@@deriving compare, equal, sexp_of]

    type evidence =
      | Lexical
      | Parsed_document of Source.t
      | Parsed_project of Source.t
      | Installed_builtin of
          { id : string
          ; sha256 : Digest.t
          }
      | Selected_catalog of Selected_catalog.t
      | Checked_template of Checked_template.t
      | Selected_tool of Selected_tool.t
      | Host_root of Id.Authoring_root.t
    [@@deriving sexp_of]

    type t =
      { kind : kind
      ; label : string
      ; insert_text : string
      ; replace : Range.t
      ; evidence : evidence
      }

    let allowed kind evidence =
      match kind, evidence with
      | (Tag | Attribute | Value), (Lexical | Parsed_document _)
      | Reference, (Parsed_document _ | Selected_catalog _ | Selected_tool _)
      | Path, (Parsed_project _ | Selected_catalog _ | Host_root _)
      | Template, (Lexical | Parsed_document _ | Installed_builtin _ | Checked_template _)
      | Environment, (Parsed_project _ | Selected_catalog _) -> true
      | _ -> false
    ;;

    let valid_builtin_id id =
      let valid_first = function
        | 'a' .. 'z' -> true
        | _ -> false
      in
      let valid_rest = function
        | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
        | _ -> false
      in
      if
        String.length id > 0
        && String.length id <= 64
        && valid_first id.[0]
        && String.for_all id ~f:valid_rest
      then Ok ()
      else C.invalid "invalid installed builtin identity"
    ;;

    let create ~kind ~label ~insert_text ~replace ~evidence =
      let* _ = text ~max:4096 label in
      let* _ = text ~max:4096 insert_text in
      if String.is_empty label || not (allowed kind evidence)
      then C.invalid "invalid assistance suggestion kind or evidence"
      else
        let* () =
          match evidence with
          | Installed_builtin { id; _ } -> valid_builtin_id id
          | Host_root root ->
            Result.map
              (Id.Authoring_root.of_json (Id.Authoring_root.to_json root))
              ~f:ignore
          | Checked_template checked ->
            let actual = Digest.of_contents insert_text ~checkpoint:ignore in
            if Digest.equal actual (Checked_template.example_digest checked)
            then Ok ()
            else C.invalid "checked template text digest mismatch"
          | _ -> Ok ()
        in
        Ok { kind; label; insert_text; replace; evidence }
    ;;

    let kind t = t.kind
    let label t = t.label
    let insert_text t = t.insert_text
    let evidence t = t.evidence
    let replace t = t.replace

    let kind_to_json = function
      | Tag -> str "tag"
      | Attribute -> str "attribute"
      | Value -> str "value"
      | Reference -> str "reference"
      | Path -> str "path"
      | Template -> str "template"
      | Environment -> str "environment"
    ;;

    let kind_of_json json =
      let* value = C.string ~max_bytes:16 json in
      match value with
      | "tag" -> Ok Tag
      | "attribute" -> Ok Attribute
      | "value" -> Ok Value
      | "reference" -> Ok Reference
      | "path" -> Ok Path
      | "template" -> Ok Template
      | "environment" -> Ok Environment
      | _ -> C.invalid "unknown assistance suggestion kind"
    ;;

    let evidence_to_json = function
      | Lexical -> obj [ "kind", str "lexical" ]
      | Parsed_document source ->
        obj [ "kind", str "parsed_document"; "source", Source.to_json source ]
      | Parsed_project source ->
        obj [ "kind", str "parsed_project"; "source", Source.to_json source ]
      | Installed_builtin { id; sha256 } ->
        obj
          [ "kind", str "installed_builtin"
          ; "id", str id
          ; "sha256", C.digest_json sha256
          ]
      | Selected_catalog selected ->
        obj
          [ "kind", str "selected_catalog"; "catalog", Selected_catalog.to_json selected ]
      | Checked_template checked ->
        obj
          [ "kind", str "checked_template"
          ; "checked_template", Checked_template.to_json checked
          ]
      | Selected_tool selected ->
        obj [ "kind", str "selected_tool"; "tool", Selected_tool.to_json selected ]
      | Host_root root ->
        obj [ "kind", str "host_root"; "root", Id.Authoring_root.to_json root ]
    ;;

    let evidence_of_json json =
      let* fields =
        C.fields
          ~allowed:
            [ "kind"
            ; "source"
            ; "id"
            ; "sha256"
            ; "catalog"
            ; "checked_template"
            ; "tool"
            ; "root"
            ]
          json
      in
      let* kind = field fields "kind" (C.string ~max_bytes:24) in
      match kind with
      | "lexical" ->
        let* _ = C.fields ~allowed:[ "kind" ] json in
        Ok Lexical
      | "parsed_document" | "parsed_project" ->
        let* fields = C.fields ~allowed:[ "kind"; "source" ] json in
        let* source = field fields "source" Source.of_json in
        Ok
          (if String.equal kind "parsed_document"
           then Parsed_document source
           else Parsed_project source)
      | "installed_builtin" ->
        let* fields = C.fields ~allowed:[ "kind"; "id"; "sha256" ] json in
        let* id = field fields "id" (C.utf8_text ~max_bytes:64) in
        let* sha256 = field fields "sha256" C.digest in
        Ok (Installed_builtin { id; sha256 })
      | "selected_catalog" ->
        let* fields = C.fields ~allowed:[ "kind"; "catalog" ] json in
        let* value = field fields "catalog" Selected_catalog.of_json in
        Ok (Selected_catalog value)
      | "checked_template" ->
        let* fields = C.fields ~allowed:[ "kind"; "checked_template" ] json in
        let* value = field fields "checked_template" Checked_template.of_json in
        Ok (Checked_template value)
      | "selected_tool" ->
        let* fields = C.fields ~allowed:[ "kind"; "tool" ] json in
        let* value = field fields "tool" Selected_tool.of_json in
        Ok (Selected_tool value)
      | "host_root" ->
        let* fields = C.fields ~allowed:[ "kind"; "root" ] json in
        let* root = field fields "root" Id.Authoring_root.of_json in
        Ok (Host_root root)
      | _ -> C.invalid "unknown assistance evidence"
    ;;

    let to_json t =
      obj
        [ "kind", kind_to_json t.kind
        ; "label", str t.label
        ; "insert_text", str t.insert_text
        ; "replace", Range.to_json t.replace
        ; "evidence", evidence_to_json t.evidence
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "kind"; "label"; "insert_text"; "replace"; "evidence" ] json
      in
      let* kind = field fields "kind" kind_of_json in
      let* label = field fields "label" (C.utf8_text ~max_bytes:4096) in
      let* insert_text = field fields "insert_text" (C.utf8_text ~max_bytes:4096) in
      let* replace = field fields "replace" Range.of_json in
      let* evidence = field fields "evidence" evidence_of_json in
      create ~kind ~label ~insert_text ~replace ~evidence
    ;;
  end

  module Status = struct
    type unavailable =
      | No_local_context
      | Catalog_unavailable
      | Source_map_unavailable
    [@@deriving compare, equal, sexp_of]

    type truncation =
      | Item_limit
      | Work_limit
      | Result_bytes
      | Unrepresentable_text
    [@@deriving compare, equal, sexp_of]

    type t =
      | Complete
      | Truncated of truncation
      | Unavailable of unavailable
    [@@deriving compare, equal, sexp_of]

    let to_json = function
      | Complete -> obj [ "kind", str "complete" ]
      | Truncated reason ->
        obj
          [ "kind", str "truncated"
          ; ( "reason"
            , str
                (match reason with
                 | Item_limit -> "item_limit"
                 | Work_limit -> "work_limit"
                 | Result_bytes -> "result_bytes"
                 | Unrepresentable_text -> "unrepresentable_text") )
          ]
      | Unavailable reason ->
        obj
          [ "kind", str "unavailable"
          ; ( "reason"
            , str
                (match reason with
                 | No_local_context -> "no_local_context"
                 | Catalog_unavailable -> "catalog_unavailable"
                 | Source_map_unavailable -> "source_map_unavailable") )
          ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "reason" ] json in
      let* kind = field fields "kind" (C.string ~max_bytes:16) in
      match kind with
      | "complete" ->
        let* _ = C.fields ~allowed:[ "kind" ] json in
        Ok Complete
      | "truncated" ->
        let* reason = field fields "reason" (C.string ~max_bytes:24) in
        (match reason with
         | "item_limit" -> Ok (Truncated Item_limit)
         | "work_limit" -> Ok (Truncated Work_limit)
         | "result_bytes" -> Ok (Truncated Result_bytes)
         | "unrepresentable_text" -> Ok (Truncated Unrepresentable_text)
         | _ -> C.invalid "unknown assistance truncation reason")
      | "unavailable" ->
        let* reason = field fields "reason" (C.string ~max_bytes:24) in
        (match reason with
         | "no_local_context" -> Ok (Unavailable No_local_context)
         | "catalog_unavailable" -> Ok (Unavailable Catalog_unavailable)
         | "source_map_unavailable" -> Ok (Unavailable Source_map_unavailable)
         | _ -> C.invalid "unknown assistance unavailable reason")
      | _ -> C.invalid "unknown assistance status"
    ;;
  end

  module Result = struct
    type t =
      { request : Request.t
      ; items : Suggestion.t list
      ; status : Status.t
      }

    let request t = t.request
    let items t = t.items
    let status t = t.status
    let sexp_of_t _ = Sexp.Atom "authoring_assistance_result"

    let to_json t =
      obj
        [ "request", Request.to_json t.request
        ; "items", C.array Suggestion.to_json t.items
        ; "status", Status.to_json t.status
        ]
    ;;

    let valid_owner ~request item =
      let range = Suggestion.replace item in
      Source.equal (Range.source range) (Request.source request)
      && Range.start_byte range <= Request.cursor_byte request
      && Request.cursor_byte request <= Range.end_byte range
    ;;

    let item_text_bytes item =
      String.length (Suggestion.label item) + String.length (Suggestion.insert_text item)
    ;;

    let preflight ~request ~items ~status =
      let* budget = C.Budget.create ~max_bytes:Limits.max_result_bytes in
      let* () =
        C.Budget.charge_json
          budget
          ~parent_depth:0
          (obj
             [ "request", Request.to_json request
             ; "items", `Array []
             ; "status", Status.to_json status
             ])
      in
      List.fold items ~init:(Ok ()) ~f:(fun result item ->
        let* () = result in
        let* () = C.Budget.reserve budget ~bytes:1 ~nodes:0 in
        C.Budget.charge_json budget ~parent_depth:2 (Suggestion.to_json item))
    ;;

    let create ~request ~items ~status =
      if
        (not (at_most (Request.max_items request) items))
        || not (at_most Limits.max_suggestions items)
      then C.invalid "too many assistance suggestions"
      else if
        match status with
        | Status.Unavailable _ -> not (List.is_empty items)
        | _ -> false
      then C.invalid "unavailable assistance cannot have suggestions"
      else if not (List.for_all items ~f:(valid_owner ~request))
      then C.invalid "assistance replacement differs from authored request"
      else
        let* _ =
          List.fold items ~init:(Ok Limits.max_label_bytes) ~f:(fun remaining item ->
            let* remaining = remaining in
            let bytes = item_text_bytes item in
            if bytes > remaining
            then C.invalid "assistance label bytes exceed aggregate limit"
            else Ok (remaining - bytes))
        in
        let* () = preflight ~request ~items ~status in
        Ok { request; items; status }
    ;;

    let create_result = create

    module Builder = struct
      type completed = t

      type t =
        { request : Request.t
        ; budget : C.Budget.t
        ; mutable items_rev : Suggestion.t list
        ; mutable count : int
        ; mutable remaining_text_bytes : int
        ; mutable full : Status.truncation option
        }

      type addition =
        | Added
        | Full of Status.truncation

      let create ~request =
        let* budget = C.Budget.create ~max_bytes:Limits.max_result_bytes in
        (* Source_map_unavailable is the longest finite status spelling. It
           has the same object shape/node count as every other non-Complete
           status, so this reserves their worst envelope exactly once. *)
        let* () =
          C.Budget.charge_json
            budget
            ~parent_depth:0
            (obj
               [ "request", Request.to_json request
               ; "items", `Array []
               ; ( "status"
                 , Status.to_json (Status.Unavailable Status.Source_map_unavailable) )
               ])
        in
        Ok
          { request
          ; budget
          ; items_rev = []
          ; count = 0
          ; remaining_text_bytes = Limits.max_label_bytes
          ; full = None
          }
      ;;

      let add t item =
        if not (valid_owner ~request:t.request item)
        then C.invalid "assistance replacement differs from authored request"
        else (
          match t.full with
          | Some reason -> Ok (Full reason)
          | None ->
            let bytes = item_text_bytes item in
            if t.count >= Request.max_items t.request || t.count >= Limits.max_suggestions
            then (
              t.full <- Some Status.Item_limit;
              Ok (Full Status.Item_limit))
            else if bytes > t.remaining_text_bytes
            then (
              t.full <- Some Status.Result_bytes;
              Ok (Full Status.Result_bytes))
            else (
              let admitted =
                let* () = C.Budget.reserve t.budget ~bytes:1 ~nodes:0 in
                C.Budget.charge_json t.budget ~parent_depth:2 (Suggestion.to_json item)
              in
              match admitted with
              | Error _ ->
                (* A constructed suggestion has fixed JSON shape/depth; only
                 finite bytes or nodes can exhaust this shared budget. *)
                t.full <- Some Status.Result_bytes;
                Ok (Full Status.Result_bytes)
              | Ok () ->
                t.items_rev <- item :: t.items_rev;
                t.count <- t.count + 1;
                t.remaining_text_bytes <- t.remaining_text_bytes - bytes;
                Ok Added))
      ;;

      let finish t ~status =
        match t.full, status with
        | Some _, Status.Unavailable _ ->
          C.invalid "unavailable assistance cannot follow full result builder"
        | _ ->
          let status =
            match t.full with
            | Some reason -> Status.Truncated reason
            | None -> status
          in
          create_result ~request:t.request ~items:(List.rev t.items_rev) ~status
      ;;
    end

    let of_json json =
      let* () = C.admit_json ~max_bytes:Limits.max_result_bytes json in
      let* fields = C.fields ~allowed:[ "request"; "items"; "status" ] json in
      let* request = field fields "request" Request.of_json in
      let* items =
        field fields "items" (C.list ~max_items:Limits.max_suggestions Suggestion.of_json)
      in
      let* status = field fields "status" Status.of_json in
      create ~request ~items ~status
    ;;
  end
end
