open Core

(** Generated daemon configuration, prompt, workspace, and credentials. *)

type t

(** [create environment ~name ~http_port] creates one isolated configuration
    fixture with physical and temporary workspaces and two bearer credentials. *)
val create : Temporary_environment.t -> name:string -> http_port:int -> t

(** [configuration t ?data_dir ?unix_socket ?http_port ()] renders normalized
    version 1 source using the supplied server-path overrides. *)
val configuration
  :  t
  -> ?data_dir:string
  -> ?unix_socket:string
  -> ?http_port:int
  -> unit
  -> string

(** [write_configuration t ~name contents] writes an absolute configuration
    path beneath the fixture directory. *)
val write_configuration : t -> name:string -> string -> string

(** [config_path t] is the default valid configuration path. *)
val config_path : t -> string

(** [data_dir t] is the configured durable data root. *)
val data_dir : t -> string

(** [unix_socket t] is the configured private Unix-socket path. *)
val unix_socket : t -> string

(** [http_port t] is the configured loopback HTTP port. *)
val http_port : t -> int

(** [admin_token t] carries the diagnostics scope. *)
val admin_token : t -> string

(** [public_token t] authenticates without the diagnostics scope. *)
val public_token : t -> string

(** [grant_public_all_scopes t] rewrites the fixture token file so the public
    credential has full scopes under its distinct principal identity. *)
val grant_public_all_scopes : t -> unit

(** [prompt_path t] is the fixture ChatMD root. *)
val prompt_path : t -> string

(** [physical_workspace t] is the configured physical workspace. *)
val physical_workspace : t -> string

(** [environment t] is the owning temporary environment. *)
val environment : t -> Temporary_environment.t
