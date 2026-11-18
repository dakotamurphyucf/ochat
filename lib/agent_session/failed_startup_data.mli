open! Core

(** Private bounded original facts. No custody receipt, execution callback,
    disposal setter or cleanup executor is supplied by this module. *)
val writer_failure
  :  progress:Session_commit_observation.Progress.t
  -> outcome:Session_commit_observation.Outcome.t
  -> unit Actor_reply.t option

module Input : sig
  type close =
    { outcome : unit Actor_reply.t
    ; normal_root_close : bool
    }

  type root =
    { observation : Session_commit_observation.Owner.t
    ; requested_candidate : bool
    }

  type t

  val create : unit -> t
  val record_producer_exit : t -> unit Actor_reply.t -> unit
  val producer_exit : t -> unit Actor_reply.t option
  val record_close : t -> normal_root_close:bool -> unit Actor_reply.t -> unit
  val close : t -> close option

  val record_root
    :  t
    -> requested_candidate:bool
    -> Session_commit_observation.Owner.t
    -> unit

  val root : t -> root option
end

module Authored : sig
  type t

  type settled_lifecycle =
    | Legacy
    | Extension of Agent_protocol.Id.Moderator_execution.t

  type cut =
    | First_lifecycle
    | Final_aggregate of settled_lifecycle

  type lifecycle =
    | Uninstalled
    | Installed
    | Legacy_acknowledged
    | Callback of
        { id : Agent_protocol.Id.Moderator_execution.t
        ; completion : Callback_cleanup_data.Completion.t
        }

  (** One cell per actual outer producer, associated with the construction when
      its first persistence attempt claims the authored scope. *)
  val create : unit -> t

  val record_producer_exit : t -> unit Actor_reply.t -> unit
  val producer_exit : t -> unit Actor_reply.t option
  val acknowledge_lifecycle : t -> unit
  val record_legacy_acknowledgement : t -> unit

  val install_callback
    :  t
    -> id:Agent_protocol.Id.Moderator_execution.t
    -> Callback_cleanup_data.Completion.t
    -> unit

  val lifecycle : t -> lifecycle

  (** Project only the first exact candidate's completed failure. An ACK,
      unrelated write or unknown result carries no retained transaction data. *)
  val record_write
    :  t
    -> cut:cut
    -> requested_candidate:bool
    -> Session_commit_observation.Owner.t
    -> unit

  val first_lifecycle_failure : t -> unit Actor_reply.t option
  val final_aggregate_failure : t -> (settled_lifecycle * unit Actor_reply.t) option
end

module Initialization : sig
  type t =
    | Unseen
    | One of Callback_cleanup_data.Initialization.t
    | Ambiguous
end

type t

val create : unit -> t
val install_input : t -> Input.t -> unit
val input : t -> Input.t option
val install_authored : t -> Authored.t -> unit
val authored : t -> Authored.t option

(** First actual Initial construction End only. Repeated legacy calls keep their
    own result/exception behavior but cannot rewrite this original outcome. *)
val record_original_end : t -> unit Actor_reply.t -> unit

val original_end : t -> unit Actor_reply.t option

(** Actual successful Initial Init-scope installation only. Retains the original
    callback parent beyond current scope clear. Same association is idempotent;
    a second distinct parent conservatively makes this bounded release unsupported,
    without refusing or changing any legacy Init execution. No history is kept. *)
val record_initialization : t -> Callback_cleanup_data.Initialization.t -> unit

val initialization : t -> Initialization.t
