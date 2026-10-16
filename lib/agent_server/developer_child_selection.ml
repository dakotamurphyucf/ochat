open! Core
module P = Agent_protocol
module O = Developer_root_owner
module N = Agent_session.Native_tool_invocation
module D = Agent_session.Developer_invocation_owner.Selected
module Saved = Agent_session.Captured_agent_runner.Claimed_developer.Selected
module Prepared = Developer_live_resources.Child_prepared
module C = Chat_response.Tool_capability

type t =
  { root : O.run
  ; target : Saved.Target.t
  ; prepared : Prepared.t
  ; parent : N.selected_borrowed
  ; issuer : D.t
  ; capabilities : C.t
  }

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
      then Saved.Target.check_descendant_now t.selection.target ~child:current
      else Saved.Target.check_now t.selection.target
    in
    Prepared.Admission.check_now t.refreshed
  ;;
end

let refresh_proof ~root ~target ~prepared ~parent =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow_selected () in
  let saved = Saved.Target.saved target in
  if Saved.matches_borrowed saved borrowed
  then (
    let%bind () =
      if phys_equal parent borrowed
      then Ok ()
      else Error (P.Error.invalid_request "saved child parent borrow changed")
    in
    let%bind () = Saved.Target.check target in
    let%bind () = O.Model_inference.check_saved_agent_source root ~selected:saved in
    let%bind refreshed = Prepared.refresh prepared in
    let%bind () = Saved.Target.check_now target in
    let%map () = Prepared.Admission.check_now refreshed in
    borrowed, refreshed, false)
  else (
    let%bind () = Saved.Target.check_descendant_now target ~child:borrowed in
    let%bind () =
      O.Model_inference.check_saved_agent_source_descendant_now
        root
        ~selected:saved
        ~child:borrowed
    in
    let%bind refreshed = Prepared.refresh_descendant prepared ~child:borrowed in
    let%bind () = Saved.Target.check_descendant_now target ~child:borrowed in
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
    refresh_proof ~root ~target ~prepared ~parent
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
    let%bind _, refreshed, _ = refresh_proof ~root ~target ~prepared ~parent in
    Prepared.Admission.check_now refreshed
  in
  let%map issuer =
    D.create_source_child
      ~parent_issuer
      ~parent_active
      ~ceiling:capabilities
      ~current_child
  in
  { root; target; prepared; parent; issuer; capabilities }
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
    refresh_proof ~root:t.root ~target:t.target ~prepared:t.prepared ~parent:t.parent
  in
  let%map narrowed = narrowed_capabilities t borrowed ~descendant in
  Admission.{ selection = t; borrowed; refreshed; descendant; capabilities = narrowed }
;;

let source_current t =
  let open Result.Let_syntax in
  let%bind admission = refresh t in
  let%map () = Admission.check_now admission in
  Admission.source_capabilities admission
;;
