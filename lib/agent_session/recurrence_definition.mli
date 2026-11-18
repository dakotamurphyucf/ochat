open! Core

(** One immutable captured recurrence revision. Its complete universal document
    owns its compatible unknown metadata and embeds the complete target carrier
    as an opaque value. It stores no live clock phase, control flags or writable
    execution state. An enclosing owner authenticates creation and currentness. *)
type t

module Schedule = Agent_protocol.Recurrence_schedule

module Goal_binding : sig
  (** Bounded immutable creator/owner association. Construction and decoding
      validate facts only. The actual owning Actor authenticates currentness
      and retained Workflow admission before capturing a new definition. *)
  module Capture : sig
    type t [@@deriving equal]

    val create
      :  owner:Agent_protocol.Session_ref.t
      -> goal_id:Agent_protocol.Id.Goal.t
      -> creator_episode:Agent_protocol.Goal_episode.t
      -> goal_revision_at_capture:int64
      -> (t, Agent_protocol.Error.t) result

    (** Exact capture fields shared by storage and native request fingerprints. *)
    val to_json : t -> Jsonaf.t

    val owner : t -> Agent_protocol.Session_ref.t
    val goal_id : t -> Agent_protocol.Id.Goal.t
    val creator_episode : t -> Agent_protocol.Goal_episode.t
    val goal_revision_at_capture : t -> int64
  end

  type t =
    | Unbound
    | Controller_goal of Capture.t
  [@@deriving equal]
end

val create
  :  key:Recurrence_identity.Definition.t
  -> creator:Agent_protocol.Id.Principal.t
  -> source:Agent_protocol.Run_source.t
  -> birth_lifecycle_revision:Agent_protocol.Session_lifecycle.Revision.t
  -> schedule:Schedule.t
  -> target:Recurrence_target.t
  -> policy:Recurrence_policy.t
  -> created_at:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val create_bound
  :  binding:Goal_binding.Capture.t
  -> key:Recurrence_identity.Definition.t
  -> creator:Agent_protocol.Id.Principal.t
  -> source:Agent_protocol.Run_source.t
  -> birth_lifecycle_revision:Agent_protocol.Session_lifecycle.Revision.t
  -> schedule:Schedule.t
  -> target:Recurrence_target.t
  -> policy:Recurrence_policy.t
  -> created_at:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val key : t -> Recurrence_identity.Definition.t
val creator : t -> Agent_protocol.Id.Principal.t
val source : t -> Agent_protocol.Run_source.t
val birth_lifecycle_revision : t -> Agent_protocol.Session_lifecycle.Revision.t
val schedule : t -> Schedule.t
val target : t -> Recurrence_target.t
val policy : t -> Recurrence_policy.t
val created_at : t -> Agent_protocol.Timestamp.t
val initial_cursor : t -> Recurrence_cursor.t
val goal_binding : t -> Goal_binding.t
val digest : t -> string
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> Document_schema.Document.t
