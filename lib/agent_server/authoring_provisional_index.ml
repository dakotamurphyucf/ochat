open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Ledger = Chatml_authoring_context
module Semantics = Chatml_semantics
module Captured = Chatml_source
module Prepared = Chatml_prepared_program

module Error = struct
  type t =
    | Retention_limit
    | Identity_exhausted
    | Foreign_source
    | Evidence_mismatch
end

type document =
  Chatmd_authoring_analysis.Script.t * (Chatmd_parser_input.t * Captured.t) list

type row =
  { capture : A.Capture.t
  ; wire : A.Prefix.provisional
  ; semantics : Semantics.Provisional.t
  ; full_source : Captured.t
  ; document : document option
  }

type t =
  { nonce : string
  ; next : int
  ; max_rows : int
  ; rows : (Ledger.Identity.t, row, Ledger.Identity.comparator_witness) Map.t
  }

let create ~owner_nonce ~max_rows =
  if max_rows <= 0 || max_rows > 1024
  then Or_error.error_string "invalid provisional row limit"
  else
    Ok
      { nonce = P.Id.Transaction.to_string owner_nonce
      ; next = 1
      ; max_rows
      ; rows = Map.empty (module Ledger.Identity)
      }
;;

let same_head left right = phys_equal left right
let retained_count t = Map.length t.rows
let semantics row = row.semantics
let full_source row = row.full_source
let capture row = row.capture
let wire row = row.wire
let document row = row.document

let same_source native wire =
  Captured.Id.equal (Captured.id native) (P.Authoring.Source.id wire)
  && Captured.Digest.equal (Captured.digest native) (P.Authoring.Source.digest wire)
  && Int.equal (Captured.byte_length native) (P.Authoring.Source.byte_length wire)
;;

let same_request request ~full_wire_source ~compiled_cursor_byte ~authored_request =
  match authored_request with
  | None ->
    P.Authoring.Source.equal (A.Prefix.Request.source request) full_wire_source
    && Int.equal (A.Prefix.Request.cursor_byte request) compiled_cursor_byte
  | Some authored ->
    P.Authoring.Source.equal
      (A.Prefix.Request.source request)
      (A.Prefix.Authored_request.source authored)
    && Int.equal
         (A.Prefix.Request.cursor_byte request)
         (A.Prefix.Authored_request.cursor_byte authored)
;;

let same_identity capture provisional =
  let identity =
    Semantics.Snapshot.identity (Semantics.Provisional.snapshot provisional)
  in
  Captured.Digest.equal
    (A.Capture.input_fingerprint capture)
    (Semantics.Provisional.full_input_fingerprint provisional)
  && Captured.Digest.equal
       (A.Capture.required_contract capture)
       (Semantics.Identity.required_contract identity)
  && Captured.Digest.equal
       (A.Capture.selected_contract capture)
       (Semantics.Identity.selected_contract identity)
  && String.equal
       (A.Capture.environment capture)
       (Prepared.Environment.name (Semantics.Identity.environment identity))
;;

let prefix_source provisional ~checkpoint =
  let selected = Semantics.Provisional.selected_source provisional in
  let digest = Semantics.Provisional.prefix_source_digest provisional in
  let rec find found = function
    | [] ->
      (match found with
       | None -> Error Error.Foreign_source
       | Some source -> Ok source)
    | source :: remaining ->
      checkpoint ();
      if Captured.Id.equal (Captured.id source) selected
      then (
        match found with
        | Some _ -> Error Error.Foreign_source
        | None -> find (Some source) remaining)
      else find found remaining
  in
  let open Result.Let_syntax in
  let%bind source =
    find None (Semantics.Snapshot.sources (Semantics.Provisional.snapshot provisional))
  in
  if
    Captured.Digest.equal (Captured.digest source) digest
    && Int.equal
         (Captured.byte_length source)
         (Semantics.Provisional.end_byte provisional)
  then Ok source
  else Error Error.Foreign_source
;;

let prepare
      t
      ~identity
      ~capture
      ~request
      ~provisional
      ~full_source
      ~full_wire_source
      ~compiled_cursor_byte
      ~authored_request
      ~document
      ~checkpoint
  =
  let open Result.Let_syntax in
  if t.next >= Int.max_value
  then Error Error.Identity_exhausted
  else if Map.length t.rows >= t.max_rows && not (Map.mem t.rows identity)
  then Error Error.Retention_limit
  else if
    (not
       (P.Authoring.Context.Expected.equal
          (A.Prefix.Request.context request)
          (A.Capture.context capture)))
    || (not
          (A.Target.equal_selector
             (A.Prefix.Request.target request)
             (A.Target.selector (A.Capture.target capture))))
    || (not (same_source full_source full_wire_source))
    || (not
          (Captured.Digest.equal
             (Captured.digest full_source)
             (Semantics.Provisional.full_source_digest provisional)))
    || (not
          (Captured.Id.equal
             (Captured.id full_source)
             (Semantics.Provisional.selected_source provisional)))
    || (not (same_identity capture provisional))
    || not
         (same_request request ~full_wire_source ~compiled_cursor_byte ~authored_request)
  then Error Error.Evidence_mismatch
  else (
    let%bind prefix_source = prefix_source provisional ~checkpoint in
    let%bind prefix_wire_source =
      P.Authoring.Source.create
        ~id:(Captured.id prefix_source)
        ~digest:(Captured.digest prefix_source)
        ~byte_length:(Captured.byte_length prefix_source)
        ~editor_revision:None
      |> Result.map_error ~f:(fun _ -> Error.Foreign_source)
    in
    let%bind snapshot =
      A.Snapshot_id.create ("prefix_" ^ t.nonce ^ "_" ^ Int.to_string t.next)
      |> Result.map_error ~f:(fun _ -> Error.Identity_exhausted)
    in
    let prefix_graph =
      Semantics.Snapshot.identity (Semantics.Provisional.snapshot provisional)
      |> Semantics.Identity.input_fingerprint
    in
    let end_byte = Semantics.Provisional.end_byte provisional in
    let%bind wire =
      (match authored_request with
       | None ->
         A.Prefix.create
           ~capture
           ~full_source:full_wire_source
           ~prefix_source:prefix_wire_source
           ~cursor_byte:compiled_cursor_byte
           ~end_byte
           ~prefix_graph
           ~snapshot
       | Some authored_request ->
         A.Prefix.create_with_authored_request
           ~capture
           ~full_source:full_wire_source
           ~prefix_source:prefix_wire_source
           ~cursor_byte:compiled_cursor_byte
           ~end_byte
           ~prefix_graph
           ~snapshot
           ~authored_request)
      |> Result.map_error ~f:(fun _ -> Error.Evidence_mismatch)
    in
    let row = { capture; wire; semantics = provisional; full_source; document } in
    let rows = Map.set t.rows ~key:identity ~data:row in
    Ok ({ t with rows; next = t.next + 1 }, wire))
;;

let current t ~identity requested =
  match Map.find t.rows identity with
  | Some row when A.Prefix.equal_provisional row.wire requested -> Some row
  | Some _ | None -> None
;;

let retire t ~identity =
  if Map.mem t.rows identity then { t with rows = Map.remove t.rows identity } else t
;;

let transition t ~contexts =
  let rows =
    Map.filteri t.rows ~f:(fun ~key:identity ~data:row ->
      let generation = (A.Capture.context row.capture).generation in
      match Ledger.basis contexts ~identity with
      | Some Ledger.Basis.Project ->
        Option.exists (Ledger.find contexts ~identity) ~f:(fun view ->
          Int.equal generation (Ledger.Generation.to_int (Ledger.View.generation view)))
      | Some Ledger.Basis.Document ->
        Option.exists (Ledger.find_document contexts ~identity) ~f:(fun view ->
          Int.equal
            generation
            (Ledger.Generation.to_int (Ledger.Document.View.generation view)))
      | None -> false)
  in
  if Map.length rows = Map.length t.rows then t else { t with rows }
;;
