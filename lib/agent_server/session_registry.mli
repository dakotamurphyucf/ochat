open! Core

(** Loaded-session registry. Actor state remains authoritative. *)

type entry =
  { actor : Agent_session.Session_actor.t
  ; history_ids : Agent_session.History_id_source.t
  ; runtime : Runtime_owner.t
  ; durable_events : Agent_session.Durable_event_log.t
  ; capacity : Session_capacity.t option
  ; store_handle : Agent_store.Session_store.Handle.t option
  ; expire_permissions : now:Agent_protocol.Timestamp.t -> unit
  ; close : unit -> unit
  }

type t

type stats =
  { loaded : int
  ; indexed : int
  }
[@@deriving sexp]

val create : unit -> t

(** Installs the durable loader used by [load]. The callback is serialized
    with registry mutations so one process-local actor is constructed per
    session. *)
val install_loader
  :  t
  -> (Agent_store.Session_index.Entry.t -> (entry, Agent_protocol.Error.t) result)
  -> unit

val index : t -> Agent_store.Session_index.Entry.t -> unit
val index_all : t -> Agent_store.Session_index.Entry.t list -> unit

val add
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> entry
  -> (unit, Agent_protocol.Error.t) result

val find : t -> Agent_protocol.Id.Session.t -> entry option

(** Returns a loaded entry or reconstructs an indexed stopped session on
    demand from its durable store. *)
val load : t -> Agent_protocol.Id.Session.t -> (entry, Agent_protocol.Error.t) result

val entries : t -> entry list
val stats : t -> stats
val load_all : t -> (entry list, Agent_protocol.Error.t) result
val remove : t -> Agent_protocol.Id.Session.t -> entry option
val summaries : t -> Agent_protocol.Session.t list

(** Closes actors for stopped sessions with no attachments, runnable work, or
    active schedules, retaining their durable index entries for lazy reload. *)
val unload_inactive : t -> index_entries:Agent_store.Session_index.Entry.t list -> int

val shutdown : t -> unit
