open! Core
module P = Agent_protocol
module Cause = Policy_cause_data
module PC = Policy_canonical_usage
module Descriptor = Policy_standalone_source

type selection =
  | Standalone of Descriptor.t
  | Registered of
      { data : Delegated_policy_governance_data.Registered.t
      ; callee : P.Session_ref.t
      ; path : Agent_store.Delegation_store.Reference.t list
      }

type t =
  { changes : Session_delta.t list
  ; classification : Descriptor.Classification.t
  }

let changes t = t.changes
let classification t = t.classification
let invalid message = Error (P.Error.invalid_request message)

let check_child receipt ~session_id ~generation =
  match receipt.P.Moderator_execution.delegation with
  | Some child
    when P.Id.Session.equal child.child_session_id session_id
         && Int.equal child.child_generation generation -> Ok ()
  | Some _ | None -> invalid "policy seed differs from its actual child association"
;;

let prepare state ~selection ~source ~receipt ~max_inherited_scopes =
  let open Result.Let_syntax in
  let%bind current_source =
    match state.Session_state.run_state with
    | None -> invalid "policy seed source prefix has no actual installation"
    | Some index ->
      Run_source_installation.captured
        (Run_state.installation index)
        ~generation:state.identity.generation
  in
  let%bind () =
    if
      P.Id.Session.equal
        state.Session_state.identity.session_id
        receipt.P.Moderator_execution.context.session_id
      && Int.equal state.identity.generation receipt.context.generation
      && P.Run_source.equal current_source source
      && P.Invocation.equal_observer source.observer receipt.context.source
    then Ok ()
    else invalid "policy seed is not the actual installed parent receipt prefix"
  in
  let receipt_change = Session_delta.Moderator_execution_changed receipt in
  let%bind after_state = Session_delta.apply state receipt_change in
  match selection with
  | Standalone descriptor ->
    let%bind () =
      check_child
        receipt
        ~session_id:descriptor.session_id
        ~generation:descriptor.generation
    in
    let%bind origin =
      Invocation_inference_provenance.of_event
        ~runs:after_state.run_state
        ~jobs:after_state.jobs
        receipt
    in
    let%bind origin =
      Result.of_option
        origin
        ~error:
          (P.Error.invalid_request "actual parent policy receipt origin is unavailable")
    in
    let%bind member =
      Cause.Member.event receipt.context.id ~generation:receipt.context.generation
    in
    let%bind source =
      Descriptor.Source.create
        ~observer:(Some receipt.context.source)
        ~installation:(Some source)
    in
    let%map descriptor =
      Descriptor.create
        ~session_id:receipt.context.session_id
        ~generation:receipt.context.generation
        ~subject:(Member member)
        ~origin
        ~source
    in
    { changes = [ receipt_change ]; classification = Standalone_unrelated descriptor }
  | Registered { data; callee; path } ->
    let%bind () =
      check_child
        receipt
        ~session_id:(P.Session_ref.session_id data.owner)
        ~generation:(Cause.Caller_source.generation data.source)
    in
    let%bind () =
      if
        P.Id.Session.equal (P.Session_ref.session_id callee) state.identity.session_id
        && P.Id.Server.equal
             (P.Session_ref.server_id data.owner)
             (P.Session_ref.server_id callee)
      then Ok ()
      else invalid "policy seed callee differs from its actual controller"
    in
    let%bind inherited =
      Result.of_option
        data.inherited
        ~error:(P.Error.invalid_request "policy seed inherited origin is unavailable")
    in
    let direct_targets =
      Option.to_list data.local
      @ List.map
          (Agent_store.Delegation_goal_origin.scopes inherited)
          ~f:Cause.Goal_target.inherited
    in
    let%bind cause =
      match data.incoming with
      | None ->
        Cause.create
          ~parent:receipt
          ~callee
          ~callee_source:source
          ~caller:data.owner
          ~caller_source:data.source
          ~caller_origin:(Some data.origin)
          ~path
          ~targets:direct_targets
          ~max_inherited_scopes
      | Some forwarded ->
        Cause.create_forwarded
          ~parent:receipt
          ~callee
          ~callee_source:source
          ~caller:data.owner
          ~caller_source:data.source
          ~caller_origin:(Some data.origin)
          ~path
          ~direct_targets
          ~forwarded
          ~max_inherited_scopes
    in
    let%bind before = Session_state.policy_context state in
    let%bind after = Session_state.policy_context after_state in
    let%map update =
      PC.register_event
        state.policy_canonical_usage
        ~before
        ~after
        ~ledger:state.inference_ledger
        ~cause
    in
    { changes = [ receipt_change; Policy_cause_changed update ]
    ; classification = Unknown
    }
;;
