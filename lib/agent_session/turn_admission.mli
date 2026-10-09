(** Focused pure actual Turn admission data; the owning actor checks runtime
    admission/writer/generation/real publisher owners before construction. It
    neither executes recovery nor grants runtime/provider authorization. *)
type t

val create
  :  ?pending_retention:Pending_disposition.Retention.t option
  -> ?runtime_admission_open:bool
  -> Session_state.t
  -> operation:Agent_protocol.Operation.t
  -> notification_wakes:Agent_protocol.Delivery.t list
  -> adopt_deferred:bool
  -> (t, Agent_protocol.Error.t) result

val operation : t -> Agent_protocol.Operation.t
val deltas : t -> Session_delta.t list
val payloads : t -> Agent_protocol.Event.Durable.Payload.t list

(** Actor integration:
    existing real-drain helper retains its existing actual reconciliation, then
    adds Moderator_changed real drain + real drain payloads to this admission.
    OCH167 combined path starts from pure post-edit candidate; requires pure
    recovery appended=[] / next_sequence unchanged and joins recovery deltas to
    edit+archive+these actual operation/lifecycle/wake deltas in ONE transition.
    Standalone Continue uses pure current-history recovery and its allocator
    reservation/retained-outcome appends + this actual admission in ONE normal
    carrier transition. No fake Runtime_builder.moderator_drain or handler call.
    Commit emits actual Operation_started; launch_worker executes only after
    successful protected persistence→canonical install handoff. *)
