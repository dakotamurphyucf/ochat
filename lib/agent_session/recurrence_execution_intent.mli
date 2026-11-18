open! Core

(** Immutable occurrence content shared by its bounded admission attempts. The
    exact nominal, input and compatible unknown JSON are retained once. Captured
    artifact bytes remain in the definition's existing custody. This is data,
    never current source, target or execution authority. *)
type t

module Child : sig
  (** Validated immutable reservation/association pair, not child existence or
      execution authority. The actual store/actor owns those separate checks. *)
  type t = private
    { reservation : Recurrence_child_reservation.t
    ; association : Recurrence_child_association.t
    }

  val create
    :  reservation:Recurrence_child_reservation.t
    -> association:Recurrence_child_association.t
    -> (t, Agent_protocol.Error.t) result
end

(** [child] is absent for an existing-session target and is the exact retained
    reservation/association pair for a captured child. A proposed target source
    is checked data; the runtime owner must authenticate its actual installation
    before admission. Controller and target may be the same session. *)
val create
  :  definition:Recurrence_definition.t
  -> control_revision:int64
  -> nominal_json:Jsonaf.t
  -> target:Agent_protocol.Session_ref.t
  -> source:Agent_protocol.Run_source.t
  -> child:Child.t option
  -> (t, Agent_protocol.Error.t) result

val occurrence : t -> Recurrence_identity.Occurrence.t
val nominal : t -> Recurrence_nominal.t
val nominal_json : t -> Jsonaf.t

(** Original capture audit data. A later retry/resume must not rewrite it or use
    it as current permission; the actual owner reauthorizes current policy and
    resolves genuinely unstarted attempts before any new effects. *)
val control_revision : t -> int64

val controller_source : t -> Agent_protocol.Run_source.t
val birth_lifecycle_revision : t -> Agent_protocol.Session_lifecycle.Revision.t
val target : t -> Agent_protocol.Session_ref.t
val source : t -> Agent_protocol.Run_source.t
val mode : t -> Agent_protocol.Run.Mode.t
val input : t -> Agent_protocol.Run_start.Input.t
val input_json : t -> Jsonaf.t
val child : t -> Recurrence_child_association.t option
val digest : t -> string

(** Reconstruct the existing canonical headless request without copying input
    into each attempt. The actual CAS and key are immutable attempt fields. *)
val request
  :  t
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> (Run_admission_request.Headless.t, Agent_protocol.Error.t) result

(** Validate the complete retained definition and child association. Historical
    evidence need not match the controller's present source/control. *)
val validate
  :  t
  -> definition:Recurrence_definition.t
  -> reservation:Recurrence_child_reservation.t option
  -> (unit, Agent_protocol.Error.t) result

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Complete owned content, including compatible fields, is bounded to 384 KiB.
    The existing 256 KiB target input profile has separate wrapper headroom here.
    The enclosing execution/frontier and shared persistence profiles are separate
    and may reject earlier. Decoding bounds JSON before recursion or hashing. *)
val limits : Document_schema.Limits.t
