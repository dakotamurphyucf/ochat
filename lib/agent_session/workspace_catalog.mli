open! Core

(** Atomically replaceable indexed workspace definitions. *)

type t

val create : Workspace_definition.t list -> (t, Agent_store.Store_error.t) result
val install : t -> replacement:t -> unit
val definitions : t -> Workspace_definition.t list
val find : t -> Agent_protocol.Id.Workspace_definition.t -> Workspace_definition.t option
val find_by_name : t -> string -> Workspace_definition.t option
