open! Core
open Authoring_analysis_location
open Authoring_analysis_capture
open Authoring_analysis_facts_core
open Authoring_analysis_result

module Prefix = struct
  type unavailable =
    | No_complete_statement
    | Unsupported_source_phase
    | Unchecked_source
    | Prefix_rejected of Analyze.failure_phase
    | Invalid_rebased_metadata
  [@@deriving equal, sexp_of]

  module Authored_request = struct
    type t =
      { source : Source.t
      ; cursor_byte : int
      }

    let create ~source ~cursor_byte =
      let* _ = bounded_nonnegative cursor_byte in
      if cursor_byte <= Source.byte_length source
      then Ok { source; cursor_byte }
      else C.invalid "authored cursor exceeds source"
    ;;

    let source t = t.source
    let cursor_byte t = t.cursor_byte

    let equal left right =
      Source.equal left.source right.source
      && Int.equal left.cursor_byte right.cursor_byte
    ;;

    let to_json t =
      obj [ "source", Source.to_json t.source; "cursor_byte", C.integer t.cursor_byte ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "source"; "cursor_byte" ] json in
      let* source = field fields "source" Source.of_json in
      let* cursor_byte = field fields "cursor_byte" C.nonnegative in
      create ~source ~cursor_byte
    ;;
  end

  type provisional =
    { capture : Capture.t
    ; full_source : Source.t
    ; prefix_source : Source.t
    ; cursor_byte : int
    ; end_byte : int
    ; prefix_graph : Digest.t
    ; snapshot : Snapshot_id.t
    ; authored_request : Authored_request.t option
    }

  let create_internal
        ~capture
        ~full_source
        ~prefix_source
        ~cursor_byte
        ~end_byte
        ~prefix_graph
        ~snapshot
        ~authored_request
    =
    let* _ = bounded_nonnegative cursor_byte in
    let* _ = bounded_nonnegative end_byte in
    if
      end_byte > cursor_byte
      || cursor_byte > Source.byte_length full_source
      || Source.byte_length prefix_source <> end_byte
      || not (Chatml_source.Id.equal (Source.id full_source) (Source.id prefix_source))
    then C.invalid "invalid provisional prefix source range"
    else (
      match Capture.target capture with
      | Target.Project_input ->
        if Option.is_some authored_request
        then C.invalid "project prefix cannot echo authored script request"
        else
          Ok
            { capture
            ; full_source
            ; prefix_source
            ; cursor_byte
            ; end_byte
            ; prefix_graph
            ; snapshot
            ; authored_request
            }
      | Target.Script_input stamp ->
        if Source.equal (Target.selected_script_source stamp) full_source
        then
          Ok
            { capture
            ; full_source
            ; prefix_source
            ; cursor_byte
            ; end_byte
            ; prefix_graph
            ; snapshot
            ; authored_request
            }
        else C.invalid "provisional source differs from selected script")
  ;;

  let create
        ~capture
        ~full_source
        ~prefix_source
        ~cursor_byte
        ~end_byte
        ~prefix_graph
        ~snapshot
    =
    create_internal
      ~capture
      ~full_source
      ~prefix_source
      ~cursor_byte
      ~end_byte
      ~prefix_graph
      ~snapshot
      ~authored_request:None
  ;;

  let create_with_authored_request
        ~capture
        ~full_source
        ~prefix_source
        ~cursor_byte
        ~end_byte
        ~prefix_graph
        ~snapshot
        ~authored_request
    =
    create_internal
      ~capture
      ~full_source
      ~prefix_source
      ~cursor_byte
      ~end_byte
      ~prefix_graph
      ~snapshot
      ~authored_request:(Some authored_request)
  ;;

  let capture t = t.capture
  let full_source t = t.full_source
  let prefix_source t = t.prefix_source
  let cursor_byte t = t.cursor_byte
  let end_byte t = t.end_byte
  let prefix_graph t = t.prefix_graph
  let snapshot t = t.snapshot
  let authored_request t = t.authored_request

  let equal_provisional left right =
    Capture.equal left.capture right.capture
    && Source.equal left.full_source right.full_source
    && Source.equal left.prefix_source right.prefix_source
    && Int.equal left.cursor_byte right.cursor_byte
    && Int.equal left.end_byte right.end_byte
    && Digest.equal left.prefix_graph right.prefix_graph
    && Snapshot_id.equal left.snapshot right.snapshot
    && Option.equal Authored_request.equal left.authored_request right.authored_request
  ;;

  module Request = struct
    type t =
      { context : Context.Expected.t
      ; target : Target.selector
      ; source : Source.t
      ; cursor_byte : int
      }

    let create ~context ~target ~source ~cursor_byte =
      let* _ = bounded_nonnegative cursor_byte in
      if cursor_byte <= Source.byte_length source
      then Ok { context; target; source; cursor_byte }
      else C.invalid "prefix cursor exceeds source"
    ;;

    let context t = t.context
    let target t = t.target
    let source t = t.source
    let cursor_byte t = t.cursor_byte

    let equal a b =
      Context.Expected.equal a.context b.context
      && Target.equal_selector a.target b.target
      && Source.equal a.source b.source
      && Int.equal a.cursor_byte b.cursor_byte
    ;;

    let sexp_of_t _ = Sexp.Atom "authoring_prefix_request"

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "target", Target.selector_to_json t.target
        ; "source", Source.to_json t.source
        ; "cursor_byte", C.integer t.cursor_byte
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "context"; "target"; "source"; "cursor_byte" ] json
      in
      let* context = field fields "context" Context.Expected.of_json in
      let* target = field fields "target" Target.selector_of_json in
      let* source = field fields "source" Source.of_json in
      let* cursor_byte = field fields "cursor_byte" C.nonnegative in
      create ~context ~target ~source ~cursor_byte
    ;;
  end

  type t =
    | Provisional of provisional
    | Unavailable of
        { context : Context.Expected.t
        ; target : Target.selector
        ; source : Source.t
        ; cursor_byte : int
        ; reason : unavailable
        }

  let provisional value = Provisional value
  let sexp_of_t _ = Sexp.Atom "authoring_prefix_result"

  let unavailable ~context ~target ~source ~cursor_byte ~reason =
    let* _ = bounded_nonnegative cursor_byte in
    if cursor_byte <= Source.byte_length source
    then Ok (Unavailable { context; target; source; cursor_byte; reason })
    else C.invalid "unavailable prefix cursor exceeds source"
  ;;

  let unavailable_to_json = function
    | No_complete_statement -> obj [ "kind", str "no_complete_statement" ]
    | Unsupported_source_phase -> obj [ "kind", str "unsupported_source_phase" ]
    | Unchecked_source -> obj [ "kind", str "unchecked_source" ]
    | Prefix_rejected phase ->
      obj [ "kind", str "prefix_rejected"; "phase", Analyze.failure_phase_to_json phase ]
    | Invalid_rebased_metadata -> obj [ "kind", str "invalid_rebased_metadata" ]
  ;;

  let unavailable_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "phase" ] json in
    let* kind = field fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "prefix_rejected" ->
      let* phase = field fields "phase" Analyze.failure_phase_of_json in
      Ok (Prefix_rejected phase)
    | "no_complete_statement"
    | "unsupported_source_phase"
    | "unchecked_source"
    | "invalid_rebased_metadata" ->
      let* _ = C.fields ~allowed:[ "kind" ] json in
      Ok
        (match kind with
         | "no_complete_statement" -> No_complete_statement
         | "unsupported_source_phase" -> Unsupported_source_phase
         | "unchecked_source" -> Unchecked_source
         | _ -> Invalid_rebased_metadata)
    | _ -> C.invalid "unknown provisional unavailable reason"
  ;;

  let to_json = function
    | Provisional value ->
      obj
        ([ "status", str "provisional"
         ; "capture", Capture.to_json value.capture
         ; "full_source", Source.to_json value.full_source
         ; "prefix_source", Source.to_json value.prefix_source
         ; "cursor_byte", C.integer value.cursor_byte
         ; "end_byte", C.integer value.end_byte
         ; "prefix_graph", C.digest_json value.prefix_graph
         ; "snapshot", Snapshot_id.to_json value.snapshot
         ]
         @ C.optional_field
             "authored_request"
             value.authored_request
             Authored_request.to_json)
    | Unavailable { context; target; source; cursor_byte; reason } ->
      obj
        [ "status", str "unavailable"
        ; "context", Context.Expected.to_json context
        ; "target", Target.selector_to_json target
        ; "source", Source.to_json source
        ; "cursor_byte", C.integer cursor_byte
        ; "reason", unavailable_to_json reason
        ]
  ;;

  let of_json json =
    let* () = C.admit_json ~max_bytes:Limits.max_result_bytes json in
    let* fields =
      C.fields
        ~allowed:
          [ "status"
          ; "capture"
          ; "full_source"
          ; "prefix_source"
          ; "cursor_byte"
          ; "end_byte"
          ; "prefix_graph"
          ; "snapshot"
          ; "authored_request"
          ; "context"
          ; "target"
          ; "source"
          ; "reason"
          ]
        json
    in
    let* status = field fields "status" (C.string ~max_bytes:16) in
    match status with
    | "provisional" ->
      let* fields =
        C.fields
          ~allowed:
            [ "status"
            ; "capture"
            ; "full_source"
            ; "prefix_source"
            ; "cursor_byte"
            ; "end_byte"
            ; "prefix_graph"
            ; "snapshot"
            ; "authored_request"
            ]
          json
      in
      let* capture = field fields "capture" Capture.of_json in
      let* full_source = field fields "full_source" Source.of_json in
      let* prefix_source = field fields "prefix_source" Source.of_json in
      let* cursor_byte = field fields "cursor_byte" C.nonnegative in
      let* end_byte = field fields "end_byte" C.nonnegative in
      let* prefix_graph = field fields "prefix_graph" C.digest in
      let* snapshot = field fields "snapshot" Snapshot_id.of_json in
      let* authored_request = opt fields "authored_request" Authored_request.of_json in
      Result.map
        (create_internal
           ~capture
           ~full_source
           ~prefix_source
           ~cursor_byte
           ~end_byte
           ~prefix_graph
           ~snapshot
           ~authored_request)
        ~f:provisional
    | "unavailable" ->
      let* fields =
        C.fields
          ~allowed:[ "status"; "context"; "target"; "source"; "cursor_byte"; "reason" ]
          json
      in
      let* context = field fields "context" Context.Expected.of_json in
      let* target = field fields "target" Target.selector_of_json in
      let* source = field fields "source" Source.of_json in
      let* cursor_byte = field fields "cursor_byte" C.nonnegative in
      let* reason = field fields "reason" unavailable_of_json in
      unavailable ~context ~target ~source ~cursor_byte ~reason
    | _ -> C.invalid "unknown prefix result status"
  ;;

  let within_bytes t ~max_bytes =
    C.encoded_size ~max_bytes (to_json t) |> Result.map ~f:(fun _ -> ())
  ;;
end
