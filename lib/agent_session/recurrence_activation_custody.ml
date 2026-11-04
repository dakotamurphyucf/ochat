open! Core
module P = Agent_protocol
module I = Recurrence_identity
module F = Recurrence_definition
module E = Recurrence_entry
module Intent = Recurrence_execution_intent
module Attempt = Recurrence_execution_attempt

module Owner = struct
  type t = unit ref

  let create () = ref ()
end

type t =
  { owner : Owner.t
  ; actor : Operator_authorization.t
  ; authorize : Session_state.t -> F.t -> (unit, P.Error.t) result
  ; definition : F.t
  ; intent : Intent.t
  ; attempt : Attempt.t
  ; origin : Recurrence_run_origin.t
  ; generation : int
  ; control_revision : int64
  ; mutable live : bool
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let origin t = t.origin
let definition t = t.definition
let nominal t = Intent.nominal t.intent
let is_live t = t.live
let belongs_to t ~owner = phys_equal t.owner owner
let close t = t.live <- false

let same_occurrence a b =
  I.Occurrence.equal (Intent.occurrence a.intent) (Intent.occurrence b.intent)
;;

let find_entry state origin =
  Option.bind state.Session_state.recurrence_catalog ~f:(fun catalog ->
    Recurrence_catalog.find
      catalog
      (I.Definition.recurrence_id
         (I.Occurrence.definition (Recurrence_run_origin.occurrence origin))))
;;

let entry t state =
  Result.of_option
    (find_entry state t.origin)
    ~error:
      (P.Error.create
         Conflict
         ~message:"activation recurrence is not retained"
         ~retryable:false
         ())
;;

let retained state origin =
  let open Result.Let_syntax in
  let%bind entry =
    Result.of_option
      (find_entry state origin)
      ~error:(P.Error.invalid_request "activation recurrence is not retained")
  in
  let%bind frontier = E.frontier entry in
  let%bind execution =
    Result.of_option
      (Recurrence_frontier.find_execution
         frontier
         (Recurrence_run_origin.occurrence origin))
      ~error:(P.Error.invalid_request "activation execution is not retained")
  in
  let attempt = Recurrence_execution.current_attempt execution in
  let%bind () =
    match
      ( Recurrence_execution.Attempt.origin attempt
      , Recurrence_execution.Attempt.terminal attempt )
    with
    | Some saved, None when Recurrence_run_origin.equal saved origin -> Ok ()
    | (Some _ | None), (Some _ | None) ->
      conflict "activation requires the exact acknowledged nonterminal association"
  in
  Ok (entry, execution, attempt)
;;

(* Common immutable candidate facts are decoded once per invalidation pass,
   never once per retained occurrence. This private result grants no authority. *)
let candidate_facts (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind installed = Recurrence_source.installed state in
  let%bind moderator_halted =
    Runtime_builder.moderator_snapshot_is_halted state.moderator
  in
  let observed_failed =
    match state.lifecycle.observed with
    | Failed _ -> true
    | Stopped
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping -> false
  in
  if
    (not state.halted)
    && (not moderator_halted)
    && (not observed_failed)
    && Option.is_none state.failure
    && P.Session.equal_desired_state state.lifecycle.desired Running
    && Session_state.Runtime_initialization.equal state.runtime_initialization Ready
  then Ok installed
  else conflict "activation controller runtime is unavailable"
;;

let semantic_basis_with t (state : Session_state.t) installed =
  let open Result.Let_syntax in
  let%bind entry, execution, attempt = retained state t.origin in
  if
    Int.equal t.generation state.identity.generation
    && Int64.equal t.control_revision (E.control_revision entry)
    && E.equal_control (E.control entry) Enabled
    && String.equal (F.digest t.definition) (F.digest (E.current entry))
    && I.Definition.equal
         (I.Occurrence.definition (Intent.occurrence t.intent))
         (F.key t.definition)
    && P.Run_source.equal installed (F.source t.definition)
    && Intent.equal t.intent (Recurrence_execution.intent execution)
    && Attempt.equal t.attempt (Recurrence_execution.Attempt.request attempt)
  then Ok ()
  else conflict "activation controller source, definition, control or runtime changed"
;;

let semantic_basis t state =
  let open Result.Let_syntax in
  let%bind facts = candidate_facts state in
  semantic_basis_with t state facts
;;

let check_policy t state =
  Recurrence_command_admission.check_write
    t.actor
    ~authorize:(fun state -> t.authorize state t.definition)
    state
;;

let check_authority t =
  if not t.live
  then conflict "activation custody is closed or belongs to another owner"
  else if
    Operator_authorization.is_current t.actor
    && P.Principal.has_scope (Operator_authorization.principal t.actor) Send_messages
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"activation original authority is unavailable"
         ~retryable:false
         ())
;;

let check_basis t ~owner state =
  let open Result.Let_syntax in
  let%bind () =
    if belongs_to t ~owner && t.live
    then Ok ()
    else conflict "activation custody is closed or belongs to another owner"
  in
  semantic_basis t state
;;

let create ~owner ~state ~actor ~authorize ~origin =
  let open Result.Let_syntax in
  let%bind entry, execution, attempt = retained state origin in
  let t =
    { owner
    ; actor
    ; authorize
    ; definition = E.current entry
    ; intent = Recurrence_execution.intent execution
    ; attempt = Recurrence_execution.Attempt.request attempt
    ; origin
    ; generation = state.identity.generation
    ; control_revision = E.control_revision entry
    ; live = true
    }
  in
  let%bind () =
    Recurrence_run_origin.validate origin ~intent:t.intent ~attempt:t.attempt
  in
  let%bind () = check_policy t state in
  let%bind () = check_authority t in
  let%map () = check_basis t ~owner state in
  t
;;

let retain_candidates candidates state =
  match candidates with
  | [] -> []
  | _ :: _ ->
    (match candidate_facts state with
     | Error _ ->
       List.iter candidates ~f:close;
       []
     | Ok facts ->
       List.filter candidates ~f:(fun t ->
         if t.live && Result.is_error (semantic_basis_with t state facts) then close t;
         t.live))
;;
