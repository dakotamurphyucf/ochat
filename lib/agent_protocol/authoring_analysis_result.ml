open! Core
open Authoring_analysis_location
open Authoring_analysis_capture
open Authoring_analysis_facts_core

module Analyze = struct
  module Request = struct
    type t =
      { context : Context.Expected.t
      ; target : Target.selector
      }

    let create ~context ~target = Ok { context; target }
    let context t = t.context
    let target t = t.target

    let equal a b =
      Context.Expected.equal a.context b.context
      && Target.equal_selector a.target b.target
    ;;

    let sexp_of_t _ = Sexp.Atom "authoring_analysis_request"

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "target", Target.selector_to_json t.target
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "context"; "target" ] json in
      let* context = field fields "context" Context.Expected.of_json in
      let* target = field fields "target" Target.selector_of_json in
      create ~context ~target
    ;;
  end

  type parser_phase =
    | Implementation
    | Interface
  [@@deriving compare, equal, sexp_of]

  let parser_phase_to_json = function
    | Implementation -> str "implementation"
    | Interface -> str "interface"
  ;;

  let parser_phase_of_json =
    enum
      ~name:"analysis parser phase"
      [ "implementation", Implementation; "interface", Interface ]
  ;;

  type failure_phase =
    | Parse of
        { source : Source.t
        ; phase : parser_phase
        }
    | Implementation of Source.t
    | Companion of Source.t
    | Host_requirements
    | Dependency
  [@@deriving equal, sexp_of]

  let failure_phase_to_json = function
    | Parse { source; phase } ->
      obj
        [ "kind", str "parse"
        ; "source", Source.to_json source
        ; "phase", parser_phase_to_json phase
        ]
    | Implementation source ->
      obj [ "kind", str "implementation"; "source", Source.to_json source ]
    | Companion source -> obj [ "kind", str "companion"; "source", Source.to_json source ]
    | Host_requirements -> obj [ "kind", str "host_requirements" ]
    | Dependency -> obj [ "kind", str "dependency" ]
  ;;

  let failure_phase_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "source"; "phase" ] json in
    let* kind = field fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "parse" ->
      let* fields = C.fields ~allowed:[ "kind"; "source"; "phase" ] json in
      let* source = field fields "source" Source.of_json in
      let* phase = field fields "phase" parser_phase_of_json in
      Ok (Parse { source; phase })
    | "implementation" | "companion" ->
      let* fields = C.fields ~allowed:[ "kind"; "source" ] json in
      let* source = field fields "source" Source.of_json in
      Ok
        (if String.equal kind "implementation"
         then Implementation source
         else Companion source)
    | "host_requirements" | "dependency" ->
      let* _ = C.fields ~allowed:[ "kind" ] json in
      Ok (if String.equal kind "host_requirements" then Host_requirements else Dependency)
    | _ -> C.invalid "unknown analysis failure phase"
  ;;

  type unavailable =
    | Document_basis
    | No_checked_closure
    | No_selected_script
    | Ambiguous_script_id
    | No_validated_project
    | Incomplete_or_malformed_document
    | Dynamic_only_source
    | Unsupported_target
  [@@deriving compare, equal, sexp_of]

  let unavailable_pairs =
    [ "document_basis", Document_basis
    ; "no_checked_closure", No_checked_closure
    ; "no_selected_script", No_selected_script
    ; "ambiguous_script_id", Ambiguous_script_id
    ; "no_validated_project", No_validated_project
    ; "incomplete_or_malformed_document", Incomplete_or_malformed_document
    ; "dynamic_only_source", Dynamic_only_source
    ; "unsupported_target", Unsupported_target
    ]
  ;;

  let unavailable_to_json reason =
    str
      (List.find_map unavailable_pairs ~f:(fun (name, item) ->
         if equal_unavailable item reason then Some name else None)
       |> Option.value_exn)
  ;;

  let unavailable_of_json = enum ~name:"analysis unavailable reason" unavailable_pairs

  type t =
    | Complete of
        { capture : Capture.t
        ; snapshot : Snapshot_id.t
        ; validated_graph : Digest.t
        ; diagnostics : Diagnostic.t list
        ; truncated_diagnostics : bool
        }
    | Failed of
        { capture : Capture.t
        ; phase : failure_phase
        ; syntax : Syntax_site.t list
        ; diagnostics : Diagnostic.t list
        ; truncated_syntax : bool
        ; truncated_diagnostics : bool
        }
    | Unavailable of
        { context : Context.Expected.t
        ; target : Target.selector
        ; reason : unavailable
        }

  let bounded_list ~max items =
    let rec walk remaining = function
      | [] -> Ok ()
      | _ when remaining = 0 -> C.invalid "analysis item count exceeds limit"
      | _ :: rest -> walk (remaining - 1) rest
    in
    walk max items
  ;;

  let preflight ~capture ~phase ~syntax ~diagnostics =
    let* budget = C.Budget.create ~max_bytes:Limits.max_result_bytes in
    let labels_left = ref Limits.max_label_bytes in
    let label value =
      let bytes = String.length value in
      if bytes > !labels_left
      then C.invalid "analysis labels exceed aggregate byte limit"
      else (
        labels_left := !labels_left - bytes;
        Ok ())
    in
    let rec charge_each encode = function
      | [] -> Ok ()
      | item :: rest ->
        let* () = C.Budget.reserve budget ~bytes:1 ~nodes:0 in
        let* () = C.Budget.charge_json budget ~parent_depth:2 (encode item) in
        charge_each encode rest
    in
    let rec scan_syntax = function
      | [] -> Ok ()
      | item :: rest ->
        let* () = label (Syntax_site.name item) in
        scan_syntax rest
    in
    let rec scan_related = function
      | [] -> Ok ()
      | item :: rest ->
        let* () = label (Diagnostic.Related.message item) in
        scan_related rest
    in
    let rec scan_diagnostics = function
      | [] -> Ok ()
      | item :: rest ->
        let* () = label (Diagnostic.message item) in
        let* () = scan_related (Diagnostic.related item) in
        scan_diagnostics rest
    in
    let* () = scan_syntax syntax in
    let* () = scan_diagnostics diagnostics in
    (* Reserve the fixed result object, status/phase fields and two arrays.
       Each list element separately charges its comma, then the exact item
       subtree debits one shared node/depth/byte budget. *)
    let* () = C.Budget.reserve budget ~bytes:8192 ~nodes:32 in
    let* () = C.Budget.charge_json budget ~parent_depth:1 (Capture.to_json capture) in
    let* () =
      match phase with
      | None -> Ok ()
      | Some phase ->
        C.Budget.charge_json budget ~parent_depth:1 (failure_phase_to_json phase)
    in
    let* () = charge_each Syntax_site.to_json syntax in
    charge_each Diagnostic.to_json diagnostics
  ;;

  let complete ~capture ~snapshot ~validated_graph ~diagnostics ~truncated_diagnostics =
    let* () = bounded_list ~max:Limits.max_diagnostics diagnostics in
    let* () = preflight ~capture ~phase:None ~syntax:[] ~diagnostics in
    Ok
      (Complete { capture; snapshot; validated_graph; diagnostics; truncated_diagnostics })
  ;;

  let failed ~capture ~phase ~syntax ~diagnostics ~truncated_syntax ~truncated_diagnostics
    =
    let* () = bounded_list ~max:Limits.max_syntax_sites syntax in
    let* () = bounded_list ~max:Limits.max_diagnostics diagnostics in
    let* () = preflight ~capture ~phase:(Some phase) ~syntax ~diagnostics in
    Ok
      (Failed
         { capture; phase; syntax; diagnostics; truncated_syntax; truncated_diagnostics })
  ;;

  let unavailable ~context ~target ~reason =
    let valid =
      match target, reason with
      | Target.Project, (Document_basis | No_checked_closure | Unsupported_target) -> true
      | ( Target.Selected_script _
        , ( No_selected_script
          | Ambiguous_script_id
          | No_validated_project
          | Incomplete_or_malformed_document
          | Dynamic_only_source
          | Unsupported_target ) ) -> true
      | _ -> false
    in
    if valid
    then Ok (Unavailable { context; target; reason })
    else C.invalid "incompatible unavailable target and reason"
  ;;

  let capture = function
    | Complete { capture; _ } | Failed { capture; _ } -> Some capture
    | Unavailable _ -> None
  ;;

  let sexp_of_t _ = Sexp.Atom "authoring_analysis_result"

  let to_json = function
    | Complete { capture; snapshot; validated_graph; diagnostics; truncated_diagnostics }
      ->
      obj
        [ "status", str "complete"
        ; "capture", Capture.to_json capture
        ; "snapshot", Snapshot_id.to_json snapshot
        ; "validated_graph", C.digest_json validated_graph
        ; "diagnostics", C.array Diagnostic.to_json diagnostics
        ; "truncated_diagnostics", C.boolean truncated_diagnostics
        ]
    | Failed
        { capture; phase; syntax; diagnostics; truncated_syntax; truncated_diagnostics }
      ->
      obj
        [ "status", str "failed"
        ; "capture", Capture.to_json capture
        ; "phase", failure_phase_to_json phase
        ; "syntax", C.array Syntax_site.to_json syntax
        ; "diagnostics", C.array Diagnostic.to_json diagnostics
        ; "truncated_syntax", C.boolean truncated_syntax
        ; "truncated_diagnostics", C.boolean truncated_diagnostics
        ]
    | Unavailable { context; target; reason } ->
      obj
        [ "status", str "unavailable"
        ; "context", Context.Expected.to_json context
        ; "target", Target.selector_to_json target
        ; "reason", unavailable_to_json reason
        ]
  ;;

  let within_bytes t ~max_bytes =
    C.encoded_size ~max_bytes (to_json t) |> Result.map ~f:(fun _ -> ())
  ;;

  let of_json json =
    let* () = C.admit_json ~max_bytes:Limits.max_result_bytes json in
    let* fields =
      C.fields
        ~allowed:
          [ "status"
          ; "capture"
          ; "snapshot"
          ; "validated_graph"
          ; "phase"
          ; "syntax"
          ; "diagnostics"
          ; "truncated_syntax"
          ; "truncated_diagnostics"
          ; "context"
          ; "target"
          ; "reason"
          ]
        json
    in
    let* status = field fields "status" (C.string ~max_bytes:16) in
    match status with
    | "complete" ->
      let* fields =
        C.fields
          ~allowed:
            [ "status"
            ; "capture"
            ; "snapshot"
            ; "validated_graph"
            ; "diagnostics"
            ; "truncated_diagnostics"
            ]
          json
      in
      let* capture = field fields "capture" Capture.of_json in
      let* snapshot = field fields "snapshot" Snapshot_id.of_json in
      let* validated_graph = field fields "validated_graph" C.digest in
      let* diagnostics =
        field fields "diagnostics" (list ~max:Limits.max_diagnostics Diagnostic.of_json)
      in
      let* truncated_diagnostics = field fields "truncated_diagnostics" Json_codec.bool in
      complete ~capture ~snapshot ~validated_graph ~diagnostics ~truncated_diagnostics
    | "failed" ->
      let* fields =
        C.fields
          ~allowed:
            [ "status"
            ; "capture"
            ; "phase"
            ; "syntax"
            ; "diagnostics"
            ; "truncated_syntax"
            ; "truncated_diagnostics"
            ]
          json
      in
      let* capture = field fields "capture" Capture.of_json in
      let* phase = field fields "phase" failure_phase_of_json in
      let* syntax =
        field fields "syntax" (list ~max:Limits.max_syntax_sites Syntax_site.of_json)
      in
      let* diagnostics =
        field fields "diagnostics" (list ~max:Limits.max_diagnostics Diagnostic.of_json)
      in
      let* truncated_syntax = field fields "truncated_syntax" Json_codec.bool in
      let* truncated_diagnostics = field fields "truncated_diagnostics" Json_codec.bool in
      failed ~capture ~phase ~syntax ~diagnostics ~truncated_syntax ~truncated_diagnostics
    | "unavailable" ->
      let* fields = C.fields ~allowed:[ "status"; "context"; "target"; "reason" ] json in
      let* context = field fields "context" Context.Expected.of_json in
      let* target = field fields "target" Target.selector_of_json in
      let* reason = field fields "reason" unavailable_of_json in
      unavailable ~context ~target ~reason
    | _ -> C.invalid "unknown analysis result status"
  ;;
end
