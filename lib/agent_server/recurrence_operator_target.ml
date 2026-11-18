open! Core
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module Target = A.Recurrence_target
module Capture = P.Recurrence_control.Capture

type t =
  { value : Target.t
  ; check_current : unit -> (unit, P.Error.t) result
  }

let value t = t.value
let check_current t = t.check_current ()

let conflict () =
  Error
    (P.Error.create
       Conflict
       ~message:"recurrence target or source changed"
       ~retryable:false
       ())
;;

let observe factory (entry : Session_registry.entry) ~session ~authorize =
  let open Result.Let_syntax in
  let%bind observation = Actor.observe_resource_state entry.actor in
  let check () =
    let%bind () = Session_factory.check_registered_owner factory entry ~session in
    let%bind state = Actor.Resource_observation.check_current observation in
    let%map () = authorize state in
    state
  in
  let%map state = check () in
  state, fun () -> Result.map (check ()) ~f:ignore
;;

let capture factory ~controller ~session ~actor ~authorize selector =
  let open Result.Let_syntax in
  match (selector : Capture.selector) with
  | Existing { session = target_session; input } ->
    let%bind entry =
      Session_factory.acquire_recurrence_target
        factory
        ~session:target_session
        ~actor
        ~authorize
    in
    let%bind preview =
      Session_factory.preview_recurrence_source factory entry ~actor ~authorize
    in
    let%bind state, check_current =
      observe factory entry ~session:target_session ~authorize
    in
    let module Preview = Session_factory.Recurrence_source_preview in
    if not (Int64.equal state.counters.revision (Preview.expected_revision preview))
    then conflict ()
    else (
      let%map value =
        Target.create_existing
          ~session:target_session
          ~input
          ~source:(Preview.source preview)
          ~configuration_revision:state.spec.configuration_revision
      in
      { value; check_current })
  | Authored_child { name; mode } ->
    let%bind _, check_current = observe factory controller ~session ~authorize in
    let%bind value =
      Session_factory.preview_recurrence_authored_target
        factory
        controller
        ~actor
        ~authorize
        ~name
        ~mode
    in
    let%map () = check_current () in
    { value; check_current }
;;

let check_frozen factory ~controller ~session ~actor ~authorize frozen =
  let open Result.Let_syntax in
  let check_selector selector =
    let%bind selected = capture factory ~controller ~session ~actor ~authorize selector in
    let%bind original = Target.digest frozen in
    let%bind current = Target.digest selected.value in
    if String.equal original current then Ok selected else conflict ()
  in
  match Target.view frozen with
  | Existing_session { session; input; _ } -> check_selector (Existing { session; input })
  | Fresh_child { source_kind = Authored_tool authored; _ } ->
    let%bind mode = Target.run_mode frozen in
    check_selector (Authored_child { name = authored.name; mode })
  | Fresh_child { source_kind = Artifact; parent; _ } ->
    (* The artifact itself is immutable. Its parent configuration must still
       match; actual creation retains the Factory's policy and artifact checks. *)
    let%bind _ = Target.run_mode frozen in
    let%bind state, check_current = observe factory controller ~session ~authorize in
    if Recurrence_parent_configuration.matches parent state.spec
    then Ok { value = frozen; check_current }
    else conflict ()
;;
