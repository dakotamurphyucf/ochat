open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Ledger = Chatml_authoring_context
module Captured = Chatml_source
module Prepared = Chatml_prepared_program

type row =
  { capture : A.Capture.t
  ; snapshot : A.Snapshot_id.t
  ; validated_graph : Captured.Digest.t
  }

type t =
  { nonce : string
  ; next : int
  ; max_rows : int
  ; rows : (Ledger.Identity.t, row, Ledger.Identity.comparator_witness) Map.t
  }

module Error = struct
  type t =
    | Retention_limit
    | Identity_exhausted
end

let create ~owner_nonce ~max_rows =
  if max_rows <= 0
  then Or_error.error_string "analysis index requires a positive retained-row limit"
  else
    Ok
      { nonce = P.Id.Transaction.to_string owner_nonce
      ; next = 1
      ; max_rows
      ; rows = Map.empty (module Ledger.Identity)
      }
;;

let retained_count t = Map.length t.rows

let prepare_complete t ~identity ~capture ~validated_graph =
  if t.next >= Int.max_value
  then Error Error.Identity_exhausted
  else if Map.length t.rows >= t.max_rows && not (Map.mem t.rows identity)
  then Error Error.Retention_limit
  else (
    let label = "analysis_" ^ t.nonce ^ "_" ^ Int.to_string t.next in
    match A.Snapshot_id.create label with
    | Error _ -> Error Error.Identity_exhausted
    | Ok snapshot ->
      let rows =
        Map.set t.rows ~key:identity ~data:{ capture; snapshot; validated_graph }
      in
      Ok ({ t with next = t.next + 1; rows }, snapshot))
;;

let retire t ~identity =
  if Map.mem t.rows identity then { t with rows = Map.remove t.rows identity } else t
;;

let same_target left right =
  match left, right with
  | A.Target.Project_input, A.Target.Project_input -> true
  | Script_input left, Script_input right ->
    P.Authoring.Source.equal
      (A.Target.document_source left)
      (A.Target.document_source right)
    && Captured.Digest.equal
         (A.Target.parser_root_fingerprint left)
         (A.Target.parser_root_fingerprint right)
    && String.equal (A.Target.script_id left) (A.Target.script_id right)
    && Captured.Digest.equal
         (A.Target.declaration_input_fingerprint left)
         (A.Target.declaration_input_fingerprint right)
    && Int.equal
         (A.Target.declaration_input_byte_length left)
         (A.Target.declaration_input_byte_length right)
    && Int.equal
         (A.Target.declaration_start_byte left)
         (A.Target.declaration_start_byte right)
    && Int.equal
         (A.Target.declaration_end_byte left)
         (A.Target.declaration_end_byte right)
    && P.Authoring.Source.equal
         (A.Target.selected_script_source left)
         (A.Target.selected_script_source right)
  | Project_input, Script_input _ | Script_input _, Project_input -> false
;;

let same_capture left right =
  P.Authoring.Context.Expected.equal (A.Capture.context left) (A.Capture.context right)
  && same_target (A.Capture.target left) (A.Capture.target right)
  && Captured.Digest.equal
       (A.Capture.input_fingerprint left)
       (A.Capture.input_fingerprint right)
  && String.equal (A.Capture.environment left) (A.Capture.environment right)
  && Captured.Digest.equal
       (A.Capture.required_contract left)
       (A.Capture.required_contract right)
  && Captured.Digest.equal
       (A.Capture.selected_contract left)
       (A.Capture.selected_contract right)
;;

let current t ~identity ~capture ~snapshot =
  match Map.find t.rows identity with
  | Some row
    when A.Snapshot_id.equal row.snapshot snapshot && same_capture row.capture capture ->
    Some row
  | Some _ | None -> None
;;

let capture row = row.capture
let validated_graph row = row.validated_graph

let same_checked_identity capture ~input ~required ~selected ~environment =
  Captured.Digest.equal (A.Capture.input_fingerprint capture) input
  && Captured.Digest.equal (A.Capture.required_contract capture) required
  && Captured.Digest.equal (A.Capture.selected_contract capture) selected
  && String.equal (A.Capture.environment capture) (Prepared.Environment.name environment)
;;

let same_project_stamp capture stamp =
  match A.Capture.target capture with
  | A.Target.Project_input ->
    same_checked_identity
      capture
      ~input:(Project.Project_stamp.input_fingerprint stamp)
      ~required:(Project.Project_stamp.required_contract stamp)
      ~selected:(Project.Project_stamp.selected_contract stamp)
      ~environment:(Project.Project_stamp.environment stamp)
  | Script_input _ -> false
;;

let same_document_stamp capture stamp =
  match A.Capture.target capture with
  | A.Target.Project_input -> false
  | Script_input target ->
    let source = A.Target.document_source target in
    let selected = A.Target.selected_script_source target in
    Captured.Id.equal
      (P.Authoring.Source.id source)
      (Document.Document_stamp.document_source stamp)
    && Captured.Digest.equal
         (P.Authoring.Source.digest source)
         (Document.Document_stamp.document_digest stamp)
    && String.equal
         (Captured.Digest.to_hex (A.Target.parser_root_fingerprint target))
         (Document.Document_stamp.parser_input_fingerprint stamp)
    && String.equal (A.Target.script_id target) (Document.Document_stamp.script_id stamp)
    && String.equal
         (Captured.Digest.to_hex (A.Target.declaration_input_fingerprint target))
         (Document.Document_stamp.declaration_input_fingerprint stamp)
    && Int.equal
         (A.Target.declaration_input_byte_length target)
         (Document.Document_stamp.declaration_input_byte_length stamp)
    && Int.equal
         (A.Target.declaration_start_byte target)
         (Document.Document_stamp.declaration_start_byte stamp)
    && Int.equal
         (A.Target.declaration_end_byte target)
         (Document.Document_stamp.declaration_end_byte stamp)
    && Captured.Id.equal
         (P.Authoring.Source.id selected)
         (Document.Document_stamp.selected_script_source stamp)
    && Captured.Digest.equal
         (P.Authoring.Source.digest selected)
         (Document.Document_stamp.selected_script_digest stamp)
    && same_checked_identity
         capture
         ~input:(Document.Document_stamp.input_fingerprint stamp)
         ~required:(Document.Document_stamp.required_contract stamp)
         ~selected:(Document.Document_stamp.selected_contract stamp)
         ~environment:(Document.Document_stamp.environment stamp)
;;

let transition t ~project ~document =
  let contexts = Project.Project_cache.contexts project in
  let rows =
    Map.filteri t.rows ~f:(fun ~key:identity ~data:row ->
      let expected = A.Capture.context row.capture in
      let current_basis =
        match A.Capture.target row.capture with
        | A.Target.Project_input ->
          (match Ledger.find contexts ~identity with
           | Some view ->
             Some
               ( Ledger.Generation.to_int (Ledger.View.generation view)
               , Ledger.View.base_graph view )
           | None -> None)
        | Script_input _ ->
          (match Ledger.find_document contexts ~identity with
           | Some view ->
             Some
               ( Ledger.Generation.to_int (Ledger.Document.View.generation view)
               , Captured.digest (Ledger.Document.View.source view) )
           | None -> None)
      in
      let same_basis =
        match current_basis with
        | Some (generation, fingerprint) ->
          Int.equal generation expected.generation
          && Captured.Digest.equal fingerprint expected.basis_fingerprint
        | None -> false
      in
      same_basis
      &&
      match A.Capture.target row.capture with
      | A.Target.Project_input ->
        (match Project.Project_cache.current project ~identity with
         | Some payload ->
           (match Project.Checked.status (Project.Project_payload.checked payload) with
            | Complete _ ->
              same_project_stamp row.capture (Project.Project_payload.stamp payload)
            | Failed _ -> false)
         | None -> false)
      | Script_input _ ->
        (match Document.Document_cache.current document ~contexts ~identity with
         | Some candidate ->
           (match Project.Checked.status (Document.Candidate.checked candidate) with
            | Complete _ ->
              same_document_stamp row.capture (Document.Candidate.stamp candidate)
            | Failed _ -> false)
         | None -> false))
  in
  if phys_equal rows t.rows then t else { t with rows }
;;
