open! Core
module Owner = Authoring_context_service_core
module Effective = Chatml_authoring_effective
module Ledger = Chatml_authoring_context
module Build = Chatml_project_build
module Captured = Chatml_source
module Contract = Chatml.Chatml_compiler_contract

module Error = struct
  type t =
    | Context of Owner.Error.t
    | No_selected_contract
    | Work_limit
    | Lock_identity_unavailable
    | Invalid_overlay of Build.Error.t
end

exception Stop_work

module Prepared = struct
  type t =
    { overlay : Effective.Overlay.t
    ; contract : Contract.t
    }

  let overlay t = t.overlay
  let build t = Effective.Overlay.build t.overlay
  let contract t = t.contract
end

let selected_contract (current : Owner.Current.t) =
  match current.context.owner with
  | Owner.Host_project context -> Some (Owner.Sources.Context.contract context)
  | Uploaded_project context -> Owner.Upload.Context_capture.project_contract context
  | Host_document _ | Supplied_document _ -> None
;;

let fresh_lock_id_from_sources ~sources ~prior_lock ~checkpoint =
  Authoring_entry_recomposition.fresh_lock_id_from_sources
    ~sources
    ~prior_lock
    ~checkpoint
  |> Result.map_error ~f:(function
    | Authoring_entry_recomposition.Error.Lock_identity_unavailable ->
      Error.Lock_identity_unavailable
    | Invalid_selected_entry | Body_limit | Recomposition _ ->
      Error.Lock_identity_unavailable)
;;

let prepare_with_selection current ~select_contract ~limits ~checkpoint =
  let remaining = ref (Build.Limits.max_work_steps limits) in
  let poll () =
    if !remaining <= 0 then raise Stop_work;
    decr remaining;
    checkpoint ()
  in
  try
    let open Result.Let_syntax in
    let%bind inputs =
      Owner.Current.project_inputs current ~checkpoint:poll
      |> Result.map_error ~f:(fun error -> Error.Context error)
    in
    let%bind contract = select_contract () in
    let%bind lock_id =
      fresh_lock_id_from_sources
        ~sources:(Ledger.Inputs.sources inputs)
        ~prior_lock:(Captured.id (Build.lock_source (Ledger.Inputs.base inputs)))
        ~checkpoint:poll
    in
    let%map overlay =
      Effective.validate_overlay inputs ~lock_id ~contract ~limits ~checkpoint:poll
      |> Result.map_error ~f:(fun error -> Error.Invalid_overlay error)
    in
    ({ overlay; contract } : Prepared.t)
  with
  | Stop_work -> Error Error.Work_limit
;;

let prepare current ~limits ~checkpoint =
  prepare_with_selection
    current
    ~select_contract:(fun () ->
      selected_contract current |> Result.of_option ~error:Error.No_selected_contract)
    ~limits
    ~checkpoint
;;

let prepare_with_contract current ~contract ~limits ~checkpoint =
  prepare_with_selection
    current
    ~select_contract:(fun () -> Ok contract)
    ~limits
    ~checkpoint
;;
