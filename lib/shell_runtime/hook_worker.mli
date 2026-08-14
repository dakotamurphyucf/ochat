open! Core

(** Executes one shell-hook-json-v1 process per request through a separately
    configured shell runtime. *)

type t

type error =
  { code : string
  ; message : string
  ; stderr : string option
  }
[@@deriving sexp, compare, equal]

val create
  :  env:Eio_unix.Stdenv.base
  -> hook_id:string
  -> kind:Hook_protocol.kind
  -> executable:Shell_access.Executable.t
  -> executor_config:Shell_access.Executor.config
  -> ?timeout_seconds:float
  -> max_input_bytes:int
  -> max_output_bytes:int
  -> redact:(string -> string)
  -> unit
  -> t

(** [invoke t payload] runs one pinned hook process and validates its response. *)
val invoke : t -> Jsonaf.t -> (Hook_protocol.action, error) result

val executable : t -> Shell_access.Executable.t
