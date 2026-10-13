(** Focused real-helper qualification. Each fixture is owned by one daemon
    epoch; callbacks must use that epoch's admitted client, grants and runtime.
    Do not retain a fixture or its callbacks across daemon shutdown. *)
open! Core

module P = Agent_protocol

type t =
  { create_parent : key:string -> P.Id.Session.t
  ; state : P.Id.Session.t -> Agent_session.Session_state.t
  ; invoke : P.Id.Session.t -> string -> Jsonaf.t -> P.Invocation.status
  ; bridge : name:string -> P.Id.Session.t -> string -> Jsonaf.t -> P.Invocation.outcome
  ; start_watch : P.Id.Session.t -> (string * Jsonaf.t) list -> P.Id.Subscription.t
  ; await_timer : P.Id.Session.t -> P.Id.Subscription.t -> unit
  ; await_watch :
      require_wake:bool -> P.Id.Session.t -> P.Id.Subscription.t -> P.Completion.t
  ; check_watches : P.Id.Session.t -> unit
  ; check_capabilities : P.Id.Session.t -> unit
  ; check_child_tools : P.Id.Session.t -> unit
  ; await : (unit -> bool) -> unit
  ; pause_child : unit -> unit
  ; release_child : unit -> unit
  ; request_private_read : unit -> unit
  ; child_calls : unit -> int
  ; session_count : unit -> int
  ; start_child : P.Id.Session.t -> (unit, P.Error.t) result
  ; stop_child : P.Id.Session.t -> unit
  ; child_request : Jsonaf.t
  }

type retained =
  { parent : P.Id.Session.t
  ; child : P.Id.Session.t
  ; receipt : string
  ; interrupted_watch : P.Id.Subscription.t
  ; child_calls_at_shutdown : int
  }

(** Execute confined grant denials, idempotent child creation, readonly/foreign
    denial, an actual private-file denial and successful receipt notification.
    Leave one genuine child provider blocked with a receipt watcher durably
    armed. The returned IDs and provider count are immutable recovery evidence;
    they grant no execution authority. *)
val initial : t -> retained

(** In a fresh epoch, require interrupted receipt classification, one committed
    notification per watch, retained successful receipt readability and no
    child inference resubmission. Stop the child using the real client. *)
val recover : t -> retained -> unit

(** In a fresh epoch with changed operator grants, permit stored status reads
    but reject child activation before any provider execution. *)
val revoked : t -> retained -> unit
