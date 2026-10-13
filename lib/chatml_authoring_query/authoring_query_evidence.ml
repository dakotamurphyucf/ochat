open Core
module Captured = Chatml_source
module Semantics = Chatml_semantics
module Wire = Agent_protocol.Authoring_analysis
module Budget = Authoring_query_budget

module Evidence = struct
  type t =
    | Complete of Semantics.Snapshot.t
    | Provisional of
        { provisional : Semantics.Provisional.t
        ; full_source : Captured.t
        }
end

type failure =
  | Evidence_mismatch
  | Stale_source
  | Source_unavailable

let equal_identity identity capture ~budget =
  Captured.Digest.equal
    (Semantics.Identity.input_fingerprint identity)
    (Wire.Capture.input_fingerprint capture)
  && Captured.Digest.equal
       (Semantics.Identity.required_contract identity)
       (Wire.Capture.required_contract capture)
  && Captured.Digest.equal
       (Semantics.Identity.selected_contract identity)
       (Wire.Capture.selected_contract capture)
  && Budget.equal_string
       budget
       (Chatml_prepared_program.Environment.name
          (Semantics.Identity.environment identity))
       (Wire.Capture.environment capture)
;;

let equal_contract identity capture ~budget =
  Captured.Digest.equal
    (Semantics.Identity.required_contract identity)
    (Wire.Capture.required_contract capture)
  && Captured.Digest.equal
       (Semantics.Identity.selected_contract identity)
       (Wire.Capture.selected_contract capture)
  && Budget.equal_string
       budget
       (Chatml_prepared_program.Environment.name
          (Semantics.Identity.environment identity))
       (Wire.Capture.environment capture)
;;

let source_matches captured wire ~budget =
  Budget.equal_source_id budget (Captured.id captured) (Wire.Source.id wire)
  && Captured.Digest.equal (Captured.digest captured) (Wire.Source.digest wire)
  && Int.equal (Captured.byte_length captured) (Wire.Source.byte_length wire)
;;

let find_source sources wire ~budget =
  let rec search = function
    | [] -> Error Source_unavailable
    | captured :: tail ->
      Budget.checkpoint budget;
      if Budget.equal_source_id budget (Captured.id captured) (Wire.Source.id wire)
      then
        if source_matches captured wire ~budget then Ok captured else Error Stale_source
      else search tail
  in
  search sources
;;

let semantic evidence request ~budget =
  let wire_source = Wire.Query.Request.source request in
  match evidence, Wire.Query.Request.evidence request with
  | Evidence.Complete snapshot, Wire.Query.Certified { capture; _ } ->
    if not (equal_identity (Semantics.Snapshot.identity snapshot) capture ~budget)
    then Error Evidence_mismatch
    else
      find_source (Semantics.Snapshot.sources snapshot) wire_source ~budget
      |> Result.map ~f:(fun source -> snapshot, source)
  | ( Evidence.Provisional { provisional; full_source = captured_full }
    , Wire.Query.Provisional prefix ) ->
    let snapshot = Semantics.Provisional.snapshot provisional in
    let capture = Wire.Prefix.capture prefix in
    let identity = Semantics.Snapshot.identity snapshot in
    let full_source = Wire.Prefix.full_source prefix in
    let prefix_source = Wire.Prefix.prefix_source prefix in
    if
      (not (equal_contract identity capture ~budget))
      || (not
            (Captured.Digest.equal
               (Semantics.Provisional.full_input_fingerprint provisional)
               (Wire.Capture.input_fingerprint capture)))
      || (not
            (Budget.equal_source_id
               budget
               (Semantics.Provisional.selected_source provisional)
               (Wire.Source.id full_source)))
      || (not
            (Captured.Digest.equal
               (Semantics.Provisional.full_source_digest provisional)
               (Wire.Source.digest full_source)))
      || (not (source_matches captured_full full_source ~budget))
      || (not
            (Captured.Digest.equal
               (Semantics.Provisional.prefix_source_digest provisional)
               (Wire.Source.digest prefix_source)))
      || (not
            (Int.equal
               (Semantics.Provisional.end_byte provisional)
               (Wire.Prefix.end_byte prefix)))
      || not
           (Budget.equal_source_id
              budget
              (Wire.Source.id prefix_source)
              (Wire.Source.id wire_source)
            && Captured.Digest.equal
                 (Wire.Source.digest prefix_source)
                 (Wire.Source.digest wire_source)
            && Int.equal
                 (Wire.Source.byte_length prefix_source)
                 (Wire.Source.byte_length wire_source)
            && Option.equal
                 Int.equal
                 (Wire.Source.editor_revision prefix_source)
                 (Wire.Source.editor_revision wire_source))
    then Error Evidence_mismatch
    else
      find_source (Semantics.Snapshot.sources snapshot) wire_source ~budget
      |> Result.map ~f:(fun source -> snapshot, source)
  | (Evidence.Complete _ | Evidence.Provisional _), _ -> Error Evidence_mismatch
;;

let syntax partial request ~budget =
  match Wire.Query.Request.evidence request with
  | Wire.Query.Syntax_only capture ->
    if not (equal_identity (Semantics.Partial.identity partial) capture ~budget)
    then Error Evidence_mismatch
    else
      find_source
        (Semantics.Partial.sources partial)
        (Wire.Query.Request.source request)
        ~budget
  | Wire.Query.Certified _ | Wire.Query.Provisional _ -> Error Evidence_mismatch
;;
