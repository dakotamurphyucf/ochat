open! Core

(** Authenticated shared-host run admission through the actual retained runtime.
    Borrows compiled resources using Runtime_owner; never constructs another
    executor or repeats initialized lifecycle callbacks. *)
type t

val create
  :  server_id:Agent_protocol.Id.Server.t
  -> authorize:
       (Operator_authorization.t
        -> Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> t

(** [authorize] must be non-yielding and consult current host policy and the
    original authenticated actor's currentness, independently of connection
    lifetime. [entry] is already the actual attached writer's authorized entry.
    Scope captures the actual compiled manager identity and genuine Session_start
    capability. Reused initialized runtime/no-input rejects; user submission uses
    the existing parser, history allocator and worker admission.

    Original CAS is checked before cold runtime construction. Only explicit owned
    constructor/input reservations advance its ephemeral preparation; every exit
    releases custody. Rejected preserves durable constructor facts. Uncertain
    final admission stays in protected receipt reconciliation without replay.

    Durable receipt retry is checked before parsing/reserving user history. A
    lost reply is reconciled by the exact original principal/key/digest; it never
    recreates runtime authority or replays initialization. *)
val start
  :  t
  -> actor:Operator_authorization.t
  -> entry:Session_registry.entry
  -> request:Agent_protocol.Run_start.t
  -> command_audit:Document_schema.Document.t option
  -> Agent_session.Run_admission_outcome.t

(** Internal host command through the same retained runtime, parser, exclusive
    actor preparation and persistence-before-worker boundary. Requires the real
    original [actor], current Send_messages and [create]'s current host policy;
    no client attachment is created or borrowed. Retained retries return before
    runtime/parser work, after current authorization. Intent is data, not authority.

    This is not a controller-ID permit: actual goal/recurrence command owners
    must separately implement durable origin, current policy, atomic association
    and stop/edit admission fencing before using automation. *)
val start_headless_command
  :  t
  -> actor:Operator_authorization.t
  -> entry:Session_registry.entry
  -> request:Agent_session.Run_admission_request.Headless.t
  -> command_audit:Document_schema.Document.t option
  -> Agent_session.Run_admission_outcome.t
