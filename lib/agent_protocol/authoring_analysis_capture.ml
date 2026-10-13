open! Core
open Authoring_analysis_location

module Target = struct
  type selector =
    | Project
    | Selected_script of string
  [@@deriving compare, equal, sexp_of]

  let project = Project

  let selected_script script_id =
    let* script_id = bounded_string ~max:256 script_id in
    if String.is_empty script_id
    then C.invalid "empty selected script ID"
    else Ok (Selected_script script_id)
  ;;

  let selector_to_json = function
    | Project -> obj [ "kind", str "project" ]
    | Selected_script script_id ->
      obj [ "kind", str "selected_script"; "script_id", str script_id ]
  ;;

  let selector_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "script_id" ] json in
    let* kind = field fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "project" ->
      let* _ = C.fields ~allowed:[ "kind" ] json in
      Ok Project
    | "selected_script" ->
      let* script_id = field fields "script_id" (C.string ~max_bytes:256) in
      selected_script script_id
    | _ -> C.invalid "unknown analysis target"
  ;;

  type script_stamp =
    { document_source : Source.t
    ; parser_root_fingerprint : Digest.t
    ; script_id : string
    ; declaration_input_fingerprint : Digest.t
    ; declaration_input_byte_length : int
    ; declaration_start_byte : int
    ; declaration_end_byte : int
    ; selected_script_source : Source.t
    }

  let script_stamp
        ~document_source
        ~parser_root_fingerprint
        ~script_id
        ~declaration_input_fingerprint
        ~declaration_input_byte_length
        ~declaration_start_byte
        ~declaration_end_byte
        ~selected_script_source
    =
    let* _ = selected_script script_id in
    let* _ = bounded_nonnegative declaration_input_byte_length in
    let* _ = bounded_nonnegative declaration_start_byte in
    let* _ = bounded_nonnegative declaration_end_byte in
    if
      declaration_start_byte < declaration_end_byte
      && declaration_end_byte <= declaration_input_byte_length
    then
      Ok
        { document_source
        ; parser_root_fingerprint
        ; script_id
        ; declaration_input_fingerprint
        ; declaration_input_byte_length
        ; declaration_start_byte
        ; declaration_end_byte
        ; selected_script_source
        }
    else C.invalid "selected declaration exceeds parser input"
  ;;

  let document_source t = t.document_source
  let parser_root_fingerprint t = t.parser_root_fingerprint
  let script_id t = t.script_id
  let declaration_input_fingerprint t = t.declaration_input_fingerprint
  let declaration_input_byte_length t = t.declaration_input_byte_length
  let declaration_start_byte t = t.declaration_start_byte
  let declaration_end_byte t = t.declaration_end_byte
  let selected_script_source t = t.selected_script_source

  type resolved =
    | Project_input
    | Script_input of script_stamp

  let selector = function
    | Project_input -> Project
    | Script_input stamp -> Selected_script stamp.script_id
  ;;

  let equal_resolved left right =
    match left, right with
    | Project_input, Project_input -> true
    | Script_input left, Script_input right ->
      Source.equal left.document_source right.document_source
      && Digest.equal left.parser_root_fingerprint right.parser_root_fingerprint
      && String.equal left.script_id right.script_id
      && Digest.equal
           left.declaration_input_fingerprint
           right.declaration_input_fingerprint
      && Int.equal left.declaration_input_byte_length right.declaration_input_byte_length
      && Int.equal left.declaration_start_byte right.declaration_start_byte
      && Int.equal left.declaration_end_byte right.declaration_end_byte
      && Source.equal left.selected_script_source right.selected_script_source
    | _ -> false
  ;;

  let resolved_to_json = function
    | Project_input -> obj [ "kind", str "project" ]
    | Script_input stamp ->
      obj
        [ "kind", str "selected_script"
        ; "document_source", Source.to_json stamp.document_source
        ; "parser_root_fingerprint", C.digest_json stamp.parser_root_fingerprint
        ; "script_id", str stamp.script_id
        ; ( "declaration_input_fingerprint"
          , C.digest_json stamp.declaration_input_fingerprint )
        ; "declaration_input_byte_length", C.integer stamp.declaration_input_byte_length
        ; "declaration_start_byte", C.integer stamp.declaration_start_byte
        ; "declaration_end_byte", C.integer stamp.declaration_end_byte
        ; "selected_script_source", Source.to_json stamp.selected_script_source
        ]
  ;;

  let resolved_of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "kind"
          ; "document_source"
          ; "parser_root_fingerprint"
          ; "script_id"
          ; "declaration_input_fingerprint"
          ; "declaration_input_byte_length"
          ; "declaration_start_byte"
          ; "declaration_end_byte"
          ; "selected_script_source"
          ]
        json
    in
    let* kind = field fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "project" ->
      let* _ = C.fields ~allowed:[ "kind" ] json in
      Ok Project_input
    | "selected_script" ->
      let* document_source = field fields "document_source" Source.of_json in
      let* parser_root_fingerprint = field fields "parser_root_fingerprint" C.digest in
      let* script_id = field fields "script_id" (C.string ~max_bytes:256) in
      let* declaration_input_fingerprint =
        field fields "declaration_input_fingerprint" C.digest
      in
      let* declaration_input_byte_length =
        field fields "declaration_input_byte_length" C.nonnegative
      in
      let* declaration_start_byte = field fields "declaration_start_byte" C.nonnegative in
      let* declaration_end_byte = field fields "declaration_end_byte" C.nonnegative in
      let* selected_script_source =
        field fields "selected_script_source" Source.of_json
      in
      Result.map
        (script_stamp
           ~document_source
           ~parser_root_fingerprint
           ~script_id
           ~declaration_input_fingerprint
           ~declaration_input_byte_length
           ~declaration_start_byte
           ~declaration_end_byte
           ~selected_script_source)
        ~f:(fun stamp -> Script_input stamp)
    | _ -> C.invalid "unknown resolved analysis target"
  ;;
end

module Capture = struct
  type t =
    { context : Context.Expected.t
    ; target : Target.resolved
    ; input_fingerprint : Digest.t
    ; environment : string
    ; required_contract : Digest.t
    ; selected_contract : Digest.t
    }

  let create
        ~context
        ~target
        ~input_fingerprint
        ~environment
        ~required_contract
        ~selected_contract
    =
    let* environment = bounded_string ~max:1024 environment in
    if String.is_empty environment
    then C.invalid "empty analysis environment"
    else
      Ok
        { context
        ; target
        ; input_fingerprint
        ; environment
        ; required_contract
        ; selected_contract
        }
  ;;

  let context t = t.context
  let target t = t.target
  let input_fingerprint t = t.input_fingerprint
  let environment t = t.environment
  let required_contract t = t.required_contract
  let selected_contract t = t.selected_contract

  let equal left right =
    Context.Expected.equal left.context right.context
    && Target.equal_resolved left.target right.target
    && Digest.equal left.input_fingerprint right.input_fingerprint
    && String.equal left.environment right.environment
    && Digest.equal left.required_contract right.required_contract
    && Digest.equal left.selected_contract right.selected_contract
  ;;

  let to_json t =
    obj
      [ "context", Context.Expected.to_json t.context
      ; "target", Target.resolved_to_json t.target
      ; "input_fingerprint", C.digest_json t.input_fingerprint
      ; "environment", str t.environment
      ; "required_contract", C.digest_json t.required_contract
      ; "selected_contract", C.digest_json t.selected_contract
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "context"
          ; "target"
          ; "input_fingerprint"
          ; "environment"
          ; "required_contract"
          ; "selected_contract"
          ]
        json
    in
    let* context = field fields "context" Context.Expected.of_json in
    let* target = field fields "target" Target.resolved_of_json in
    let* input_fingerprint = field fields "input_fingerprint" C.digest in
    let* environment = field fields "environment" (C.string ~max_bytes:1024) in
    let* required_contract = field fields "required_contract" C.digest in
    let* selected_contract = field fields "selected_contract" C.digest in
    create
      ~context
      ~target
      ~input_fingerprint
      ~environment
      ~required_contract
      ~selected_contract
  ;;
end

module Snapshot_id = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let create value =
    let* value = bounded_string ~max:128 value in
    if String.is_empty value then C.invalid "empty analysis snapshot ID" else Ok value
  ;;

  let to_json t = str t
  let of_json json = C.string ~max_bytes:128 json |> Result.bind ~f:create
end
