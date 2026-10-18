open! Core
module P = Agent_protocol
module O = Developer_root_owner
module N = Agent_session.Native_tool_invocation
module D = Agent_session.Developer_invocation_owner.Selected
module Saved = Agent_session.Captured_agent_runner.Claimed_developer.Selected
module Content = Agent_session.Captured_agent_runner.Selected_content_target
module Model = Agent_session.Captured_agent_runner.Selected_model_target
module Prepared = Developer_live_resources.Child_prepared
module C = Chat_response.Tool_capability

type t =
  { root : O.run
  ; target : target
  ; saved_ancestor : Saved.Target.t
  ; prepared : Prepared.t
  ; parent : N.selected_borrowed
  ; issuer : D.t
  ; capabilities : C.t
  }

and target =
  | Saved of Saved.Target.t
  | Content of Content.t
  | Model of Model.t

type selection = t

module Admission = struct
  type t =
    { selection : selection
    ; borrowed : N.selected_borrowed
    ; refreshed : Prepared.Admission.t
    ; descendant : bool
    ; capabilities : C.t
    }

  let capabilities t = t.capabilities
  let source_capabilities t = t.selection.capabilities

  let check_now t =
    let open Result.Let_syntax in
    let%bind current = N.borrow_selected () in
    let%bind () =
      if phys_equal current t.borrowed
      then Ok ()
      else Error (P.Error.invalid_request "selected child scope changed")
    in
    let%bind () =
      if t.descendant
      then (
        match t.selection.target with
        | Saved target -> Saved.Target.check_descendant_now target ~child:current
        | Content target -> Content.check_descendant_now target ~child:current
        | Model target -> Model.check_descendant_now target ~child:current)
      else (
        match t.selection.target with
        | Saved target -> Saved.Target.check_now target
        | Content target -> Content.check_now target
        | Model target -> Model.check_now target)
    in
    Prepared.Admission.check_now t.refreshed
  ;;

  let checked_root_now t =
    let open Result.Let_syntax in
    let%bind () = check_now t in
    Prepared.Admission.checked_root_now t.refreshed
  ;;
end

let refresh_proof ~root ~target ~saved_ancestor ~prepared ~parent =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow_selected () in
  let saved = Saved.Target.saved saved_ancestor in
  let check_target () =
    match target with
    | Saved target -> Saved.Target.check target
    | Content target -> Content.check target
    | Model target -> Model.check target
  in
  let check_target_now () =
    match target with
    | Saved target -> Saved.Target.check_now target
    | Content target -> Content.check_now target
    | Model target -> Model.check_now target
  in
  let check_descendant_now () =
    match target with
    | Saved target -> Saved.Target.check_descendant_now target ~child:borrowed
    | Content target -> Content.check_descendant_now target ~child:borrowed
    | Model target -> Model.check_descendant_now target ~child:borrowed
  in
  let check_saved_root () =
    if Saved.matches_borrowed saved borrowed
    then O.Model_inference.check_saved_agent_source root ~selected:saved
    else
      O.Model_inference.check_saved_agent_source_descendant_now
        root
        ~selected:saved
        ~child:borrowed
  in
  if phys_equal borrowed parent
  then (
    let%bind () = check_target () in
    let%bind _ = O.current_after_mutations root in
    let%bind () = check_saved_root () in
    let%bind refreshed = Prepared.refresh prepared in
    let%bind () = check_target_now () in
    let%map () = Prepared.Admission.check_now refreshed in
    borrowed, refreshed, false)
  else (
    let%bind _ = O.current_after_mutations root in
    let%bind () = check_descendant_now () in
    let%bind () = check_saved_root () in
    let%bind refreshed = Prepared.refresh_descendant prepared ~child:borrowed in
    let%bind () = check_descendant_now () in
    let%map () = Prepared.Admission.check_now refreshed in
    borrowed, refreshed, true)
;;

let create ~root ~target ~prepared ~parent =
  let open Result.Let_syntax in
  let%bind () =
    if Prepared.matches_target prepared target
    then Ok ()
    else Error (P.Error.invalid_request "saved child resources belong to another target")
  in
  let%bind current, refreshed, descendant =
    refresh_proof ~root ~target:(Saved target) ~saved_ancestor:target ~prepared ~parent
  in
  let%bind () =
    if (not descendant) && phys_equal current parent
    then Prepared.Admission.check_now refreshed
    else Error (P.Error.invalid_request "saved child must begin under its exact parent")
  in
  let capabilities = Prepared.selected_native prepared in
  let parent_issuer = N.selected_borrowed_issuer parent in
  let parent_active = N.selected_borrowed_parent parent in
  let current_child () =
    let%bind _, refreshed, _ =
      refresh_proof ~root ~target:(Saved target) ~saved_ancestor:target ~prepared ~parent
    in
    Prepared.Admission.check_now refreshed
  in
  let%map issuer =
    D.create_source_child
      ~parent_issuer
      ~parent_active
      ~ceiling:capabilities
      ~current_child
  in
  { root
  ; target = Saved target
  ; saved_ancestor = target
  ; prepared
  ; parent
  ; issuer
  ; capabilities
  }
;;

let create_descendant ~root ~parent ~target ~prepared ~borrowed =
  let open Result.Let_syntax in
  let%bind () =
    if
      phys_equal root parent.root
      &&
      match target with
      | Content target -> Prepared.matches_content_target prepared target
      | Model target -> Prepared.matches_model_target prepared target
      | Saved _ -> false
    then Ok ()
    else
      Error (P.Error.invalid_request "selected child belongs to another Root or target")
  in
  let expected_parent =
    match parent.target with
    | Saved target -> Content.Saved target
    | Content target -> Content.Content target
    | Model target -> Content.Model target
  in
  let%bind () =
    if
      match target with
      | Content target -> Content.matches_parent target expected_parent
      | Model target -> Model.matches_parent target expected_parent
      | Saved _ -> false
    then Ok ()
    else Error (P.Error.invalid_request "selected child has a foreign immediate parent")
  in
  let%bind _, parent_refreshed, _ =
    refresh_proof
      ~root:parent.root
      ~target:parent.target
      ~saved_ancestor:parent.saved_ancestor
      ~prepared:parent.prepared
      ~parent:parent.parent
  in
  let%bind () = Prepared.Admission.check_now parent_refreshed in
  let%bind current = N.borrow_selected () in
  let%bind () =
    if phys_equal current borrowed
    then Ok ()
    else Error (P.Error.invalid_request "selected child lost its physical native borrow")
  in
  let%bind _, refreshed, descendant =
    refresh_proof
      ~root
      ~target
      ~saved_ancestor:parent.saved_ancestor
      ~prepared
      ~parent:borrowed
  in
  let%bind () =
    if descendant
    then
      Error
        (P.Error.invalid_request
           "selected child begins outside its selected native borrow")
    else Prepared.Admission.check_now refreshed
  in
  let capabilities = Prepared.selected_native prepared in
  let parent_issuer = N.selected_borrowed_issuer borrowed in
  let parent_active = N.selected_borrowed_parent borrowed in
  let current_child () =
    let%bind _, refreshed, _ =
      refresh_proof
        ~root
        ~target
        ~saved_ancestor:parent.saved_ancestor
        ~prepared
        ~parent:borrowed
    in
    let%bind _, parent_refreshed, _ =
      refresh_proof
        ~root:parent.root
        ~target:parent.target
        ~saved_ancestor:parent.saved_ancestor
        ~prepared:parent.prepared
        ~parent:parent.parent
    in
    let%bind () = Prepared.Admission.check_now parent_refreshed in
    Prepared.Admission.check_now refreshed
  in
  let%map issuer =
    D.create_source_child
      ~parent_issuer
      ~parent_active
      ~ceiling:capabilities
      ~current_child
  in
  { root
  ; target
  ; saved_ancestor = parent.saved_ancestor
  ; prepared
  ; parent = borrowed
  ; issuer
  ; capabilities
  }
;;

let create_content ~root ~parent ~target ~prepared ~borrowed =
  create_descendant ~root ~parent ~target:(Content target) ~prepared ~borrowed
;;

let create_model ~root ~parent ~target ~prepared ~borrowed =
  create_descendant ~root ~parent ~target:(Model target) ~prepared ~borrowed
;;

let issuer t = t.issuer
let issued_by_root t root = phys_equal t.root root
let first_parent t = D.Source_child (N.selected_borrowed_parent t.parent)

let first_borrowed t =
  let open Result.Let_syntax in
  let%bind current = N.borrow_selected () in
  if phys_equal current t.parent
  then Ok current
  else Error (P.Error.invalid_request "saved child first entry lost its exact parent")
;;

let managed_definition t = Prepared.managed_definition t.prepared

let narrowed_capabilities t borrowed ~descendant =
  if not descendant
  then Ok t.capabilities
  else
    let open Result.Let_syntax in
    let%bind ceiling = N.selected_borrowed_capabilities borrowed in
    let%bind names =
      List.fold_result (C.references ceiling) ~init:[] ~f:(fun names reference ->
        C.resolve t.capabilities ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
        |> Result.map ~f:(fun _ -> reference.name :: names))
    in
    C.select t.capabilities ~names:(List.rev names)
    |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
;;

let refresh t =
  let open Result.Let_syntax in
  let%bind borrowed, refreshed, descendant =
    refresh_proof
      ~root:t.root
      ~target:t.target
      ~saved_ancestor:t.saved_ancestor
      ~prepared:t.prepared
      ~parent:t.parent
  in
  let%map narrowed = narrowed_capabilities t borrowed ~descendant in
  Admission.{ selection = t; borrowed; refreshed; descendant; capabilities = narrowed }
;;

let capture_issuer_source t ~live ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if Prepared.issued_by_live t.prepared live
    then Ok ()
    else Error (P.Error.invalid_request "selected child belongs to another Live owner")
  in
  let%bind admission = refresh t in
  let%bind () = Admission.check_now admission in
  let%bind issuer = Prepared.issuer_source t.prepared ~checkpoint in
  let%bind after = refresh t in
  let%map () = Admission.check_now after in
  issuer
;;

let prepare_agent_source t ~live ~binding ~launch ~request ~max_registrations ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if Prepared.issued_by_live t.prepared live
    then Ok ()
    else Error (P.Error.invalid_request "selected child belongs to another Live owner")
  in
  let%bind admission = refresh t in
  let%bind () = Admission.check_now admission in
  let%bind artifact =
    Chat_response.Agent_runtime.prepare_agent_source
      (Prepared.runtime t.prepared)
      ~binding
      ~launch
      ~request
      ~max_registrations
      ~checkpoint
  in
  let%bind after = refresh t in
  let%map () = Admission.check_now after in
  artifact
;;

let source_current t =
  let open Result.Let_syntax in
  let%bind admission = refresh t in
  let%map () = Admission.check_now admission in
  Admission.source_capabilities admission
;;
