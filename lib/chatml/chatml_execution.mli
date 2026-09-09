open Core

(** Invocation-local execution budget shared by program initialization, pure
    evaluation and task continuations. Does not grant tool authority. *)
type limits =
  { fuel : int
  ; max_tasks : int
  ; wall_seconds : float
  ; max_value_bytes : int
  ; max_array_items : int
  ; max_depth : int
  ; allocation_bytes : int
  }

(** Chosen by the host, never by submitted source. [Unrestricted] installs no
    resource budget; it does not grant any additional operation or tool authority.
    [Bounded] accepts host-selected limits without fixed policy ceilings. *)
type policy =
  | Unrestricted
  | Bounded of limits

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp]

val default_limits : limits

(** Run a fresh standalone task entrypoint. Defaults to [Bounded default_limits].
    Bounded evaluation polls cancellation at regular expression intervals. Elapsed
    time includes external tool waits; a host callback must cooperate with Eio
    cancellation. A single builtin is checked before/after invocation, so this
    is not a hard deadline or heap sandbox. Allocation accounting estimates
    language operations, not actual OCaml heap use. Selected-tool policy and
    serialized output bounds remain the owning host's responsibility.
    Cancellation from the caller propagates; fuel/deadline failures return stable
    host errors and cannot be converted to success by Task.catch.

    [Unrestricted] omits resource checks and automatic pure-evaluation yields;
    caller cancellation still propagates at cooperative host operations. Normal
    language errors, schemas and operation authority continue to apply. *)
val run
  :  ?policy:policy
  -> env:Eio_unix.Stdenv.base
  -> config:Chatml_host_runtime.runtime_config
  -> program:Chatml_host_runtime.compiled_script
  -> entrypoint:string
  -> arguments:Chatml.Chatml_lang.value list
  -> unit
  -> (Chatml.Chatml_lang.value, error) result
