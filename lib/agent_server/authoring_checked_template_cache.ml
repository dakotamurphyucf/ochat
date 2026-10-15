open! Core
module Ledger = Chatml_authoring_context
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Captured = Chatml_source
module Document = Chatmd_authoring_analysis
module Target = Agent_protocol.Authoring.Analysis.Target
module Witness = Authoring_checked_template.Witness
module Effective = Chatml_authoring_effective

module Error = struct
  type t =
    | Invalid_binding
    | Capacity_limit
    | Retained_byte_limit
end

module Binding = struct
  type kind =
    | Project
    | Document of Document.Document_stamp.t

  type t =
    { identity : Ledger.Identity.t
    ; generation : Ledger.Generation.t
    ; kind : kind
    ; witness : Witness.t
    }

  let matches_selected witness prepared =
    Captured.Digest.equal
      (Witness.base_fingerprint witness)
      (Prepared.fingerprint prepared)
    && Captured.Digest.equal
         (Witness.selected_contract witness)
         (Prepared.compiler_contract prepared)
    && String.equal
         (Witness.environment witness)
         (Prepared.Environment.name (Prepared.environment prepared))
    && Captured.Id.equal (Witness.original_entry_id witness) (Prepared.entry prepared)
    && Option.value_map
         (Prepared.find_source prepared (Prepared.entry prepared))
         ~default:false
         ~f:(fun entry ->
           Captured.Digest.equal
             (Witness.original_entry_digest witness)
             (Captured.digest entry))
  ;;

  let project ~inputs ~receipt ~witness =
    if
      Ledger.Identity.equal
        (Ledger.Inputs.identity inputs)
        (Effective.Overlay.identity receipt)
      && Ledger.Generation.equal
           (Ledger.Inputs.generation inputs)
           (Effective.Overlay.generation receipt)
      && matches_selected witness (Build.prepared (Effective.Overlay.build receipt))
    then
      Ok
        { identity = Ledger.Inputs.identity inputs
        ; generation = Ledger.Inputs.generation inputs
        ; kind = Project
        ; witness
        }
    else Error Error.Invalid_binding
  ;;

  let document ~inputs ~script ~witness =
    let stamp = Document.Script.document_stamp script in
    if
      Ledger.Identity.equal
        (Ledger.Document.Inputs.identity inputs)
        (Document.Document_stamp.identity stamp)
      && Ledger.Generation.equal
           (Ledger.Document.Inputs.generation inputs)
           (Document.Document_stamp.generation stamp)
      && matches_selected witness (Document.Script.selected_prepared script)
    then
      Ok
        { identity = Ledger.Document.Inputs.identity inputs
        ; generation = Ledger.Document.Inputs.generation inputs
        ; kind = Document stamp
        ; witness
        }
    else Error Error.Invalid_binding
  ;;
end

type row = Binding.t

type t =
  { rows : (Ledger.Identity.t, row, Ledger.Identity.comparator_witness) Map.t
  ; retained_bytes : int
  ; max_rows : int
  ; max_retained_bytes : int
  }

let create ~max_rows ~max_retained_bytes =
  if max_rows <= 0 || max_retained_bytes <= 0
  then Or_error.error_string "invalid checked-template retention policy"
  else
    Ok
      { rows = Map.empty (module Ledger.Identity)
      ; retained_bytes = 0
      ; max_rows
      ; max_retained_bytes
      }
;;

let retained_count t = Map.length t.rows
let retained_bytes t = t.retained_bytes
let row_bytes row = String.length (Witness.example row.Binding.witness)

let retire t ~identity =
  match Map.find t.rows identity with
  | None -> t
  | Some old ->
    { t with
      rows = Map.remove t.rows identity
    ; retained_bytes = t.retained_bytes - row_bytes old
    }
;;

let prepare t ~binding =
  let without_old = retire t ~identity:binding.Binding.identity in
  let bytes = row_bytes binding in
  if Map.length without_old.rows >= t.max_rows
  then Error Error.Capacity_limit
  else if bytes > t.max_retained_bytes - without_old.retained_bytes
  then Error Error.Retained_byte_limit
  else
    Ok
      { t with
        rows = Map.set without_old.rows ~key:binding.identity ~data:binding
      ; retained_bytes = without_old.retained_bytes + bytes
      }
;;

let transition t ~contexts =
  Map.fold t.rows ~init:t ~f:(fun ~key:identity ~data:row state ->
    let current =
      match row.Binding.kind with
      | Project ->
        Option.value_map (Ledger.find contexts ~identity) ~default:false ~f:(fun view ->
          Ledger.Generation.equal (Ledger.View.generation view) row.generation)
      | Document _ ->
        Option.value_map
          (Ledger.find_document contexts ~identity)
          ~default:false
          ~f:(fun view ->
            Ledger.Generation.equal (Ledger.Document.View.generation view) row.generation)
    in
    if current then state else retire state ~identity)
;;

let current t ~identity ~generation ~target =
  match Map.find t.rows identity with
  | Some row when Ledger.Generation.equal row.generation generation ->
    (match row.kind, target with
     | Project, Target.Project -> Some row
     | Document stamp, Target.Selected_script script_id
       when String.equal (Document.Document_stamp.script_id stamp) script_id -> Some row
     | Project, Selected_script _ | Document _, Project | Document _, Selected_script _ ->
       None)
  | Some _ | None -> None
;;

let witness row = row.Binding.witness

let matches_project row ~inputs ~receipt =
  match row.Binding.kind with
  | Document _ -> false
  | Project ->
    Ledger.Identity.equal row.identity (Ledger.Inputs.identity inputs)
    && Ledger.Generation.equal row.generation (Ledger.Inputs.generation inputs)
    && Ledger.Identity.equal row.identity (Effective.Overlay.identity receipt)
    && Ledger.Generation.equal row.generation (Effective.Overlay.generation receipt)
    && Binding.matches_selected
         row.witness
         (Build.prepared (Effective.Overlay.build receipt))
;;

let matches_document row ~inputs ~script =
  match row.Binding.kind with
  | Project -> false
  | Document stamp ->
    Ledger.Identity.equal row.identity (Ledger.Document.Inputs.identity inputs)
    && Ledger.Generation.equal row.generation (Ledger.Document.Inputs.generation inputs)
    && Document.Document_stamp.equal stamp (Document.Script.document_stamp script)
    && Binding.matches_selected row.witness (Document.Script.selected_prepared script)
;;
