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

(** [invoke t payload] runs one pinned hook process and validates its response.
    Active caller services and their trusted code adapter are applied at dispatch;
    expired caller scopes reject without execution. Unbound calls retain the
    original worker owner's session and approval services. *)
val invoke : t -> Jsonaf.t -> (Hook_protocol.action, error) result

val executable : t -> Shell_access.Executable.t

(** Pure immutable copy with the selected worker config. The resolved executable,
    environment, protocol/limits and redaction function remain the actual original
    values. Acquires no resource and executes no hook. Trusted registry assembly
    supplies a config over the same admitted worker resource owner. *)
val with_executor_config : t -> Shell_access.Executor.config -> (t, string) Result.t
