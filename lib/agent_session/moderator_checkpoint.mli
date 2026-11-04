open Core

(** Complete checkpoint custody. The parent state/delta treats this validated
    child as a value; [shape] belongs to this checkpoint's preservation codec. *)
type t

val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val to_jsonaf : t -> Jsonaf.t
val snapshot : t -> Session.Moderator_state.Identity_snapshot.t

module Queue_update : sig
  type t =
    | Keep
    | Replace of Moderator_queue.t
end

(** Update known state while retaining all compatible raw extensions. [Keep]
    preserves queue absence/null/value. Incompatible deletion, array alignment
    or source changes with retained extensions fail before returning a value.
    This operation grants no source, Manager or execution authority. *)
val update
  :  t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> queue:Queue_update.t
  -> (t, Agent_protocol.Error.t) result

(** A first checkpoint is authored when absent; otherwise update the actual full
    previous checkpoint. The supplied queue still requires its real owner's
    producer/consumption admission. *)
val update_with_queue
  :  Jsonaf.t option
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> queue:Moderator_queue.t
  -> (Jsonaf.t, Agent_protocol.Error.t) result

(** Native/durable no-loss law. Compatible raw fields must already be present in
    the incoming native value: no hidden parent carrier repairs a dropped field.
    Nullable removal and source changes retain universal protection. Queue
    producer, ordinal-transition and source-installation laws remain separate. *)
val validate_update
  :  previous:Jsonaf.t option
  -> Jsonaf.t option
  -> (unit, Agent_protocol.Error.t) result

val encode : Session.Moderator_state.Identity_snapshot.t -> Jsonaf.t
val shape : Document_schema.Shape.t

(** Shared identity-safe decoding for runtime restoration and delegated authority.
    This reads persisted identity/state; it grants no manager or execution lease. *)
val decode
  :  Jsonaf.t option
  -> (Session.Moderator_state.Identity_snapshot.t option, Agent_protocol.Error.t) result

val observer
  :  Jsonaf.t option
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

val is_halted : Jsonaf.t option -> (bool, Agent_protocol.Error.t) result

(** Optional durable occurrence metadata. Historical absent/null children remain
    unavailable; a present current child is validated strictly. *)
val queue : Jsonaf.t option -> (Moderator_queue.t option, Agent_protocol.Error.t) result

val encode_with_queue
  :  Session.Moderator_state.Identity_snapshot.t
  -> queue:Moderator_queue.t
  -> (Jsonaf.t, Agent_protocol.Error.t) result

(** Complete raw optional queue child bounds, including its unknown fields. *)
val validate_encoded_capacity : Jsonaf.t option -> (unit, Agent_protocol.Error.t) result

(** Existing receipt checkpoint digest over the complete known identity snapshot.
    This is a data comparison, not proof of an installed runtime or authority. *)
val digest : Session.Moderator_state.Identity_snapshot.t -> string

(** Constructor/runtime owners may reuse the ORIGINAL full checkpoint when the
    incoming value is exactly the bare known-snapshot encoding and both validated
    known snapshots agree. Explicit metadata, unknown incoming data and any known
    change are returned unchanged for ordinary strict transition validation.
    This preserves raw extension custody; it neither merges nor authorizes an
    install, and generic checkpoint mutations must keep their existing policy. *)
val reuse_unchanged_snapshot
  :  previous:Jsonaf.t option
  -> incoming:Jsonaf.t option
  -> (Jsonaf.t option, Agent_protocol.Error.t) result
