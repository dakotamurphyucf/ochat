open Core

(** Host execution budget shared by initialization, pure evaluation, task
    continuations and nested ChatML runs. Does not grant tool authority. *)
type limits =
  { fuel : int
  ; max_tasks : int
  ; wall_seconds : float
  ; max_value_bytes : int
  ; max_array_items : int
  ; max_depth : int
  ; allocation_bytes : int
  ; max_calls : int
    (** Executed Tool.call/Tool.spawn attempts, shared with descendants. *)
  ; max_invocation_depth : int
    (** ChatML execution levels, including this run. Native dispatch wrappers
        alone do not add a level. Must be positive. *)
  }

(** Chosen by the host, never by submitted source. [Unrestricted] adds no new
    budget and cannot remove inherited ceilings or grant operation/tool authority.
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

(** Opaque active budget ancestry for a host domain/executor handoff. Capturing
    it grants no lifetime extension. [run] merges it with ambient ancestry,
    preserving every ceiling and the stricter depth allowance for shared owners. *)
type context

val capture_context : ?inherited:context -> unit -> context

(** A persistent environment keeps this runner's control proxy while each
    initialization/event receives a fresh lexical budget. The proxy consults
    the current fiber's binding; use outside [run_scoped] fails. Nested runs
    inherit ambient budgets. The runner grants neither tool authority nor
    synchronization: the owner must serialize mutable runtime access. *)
type runner

val create_runner
  :  env:< mono_clock : _ Eio.Time.Mono.t ; .. >
  -> policy:policy
  -> unit
  -> runner

val runner_control : runner -> Chatml.Chatml_lang.execution_control

(** Run an owned initialization/event under the runner's policy. Host
    cancellation propagates and lexical bindings expire even on failure.
    Checks that protect a transaction must happen before its irreversible
    commit; this wrapper does not retroactively validate or undo host effects.
    [context] merges explicit caller ancestry with the current fiber's budgets,
    preserving their lifetimes and ceilings across host domain handoffs. *)
val run_scoped : ?context:context -> runner -> (unit -> 'a) -> ('a, error) result

(** Run a fresh standalone task entrypoint. Defaults to [Bounded default_limits].
    Bounded evaluation polls cancellation at regular expression intervals. Nested
    runs debit all active ancestor budgets, even with larger or unrestricted local
    policy. Exhaustion stays attached to its owner, so a native error wrapper or
    Task.catch cannot turn ancestor exhaustion into success. A stricter child-only
    failure does not poison an otherwise valid parent. Captured ancestry merges
    with current ancestry and expires with its owner; it never starts a fresh
    budget implicitly after expiration. Elapsed
    time includes external tool waits; a host callback must cooperate with Eio
    cancellation. A single builtin is checked before/after invocation, so this
    is not a hard deadline or heap sandbox. Allocation accounting estimates
    language operations and host-returned values, not actual OCaml heap use.
    Host effect results are checked before debug rendering or continuation use;
    failure after an effect cannot undo it. Selected-tool policy and
    serialized output bounds remain the owning host's responsibility.
    Cancellation from the caller propagates; fuel/deadline failures return stable
    host errors and cannot be converted to success by Task.catch.

    A root [Unrestricted] run omits resource checks and automatic pure-evaluation
    yields; caller cancellation still propagates at cooperative host operations. Normal
    language errors, schemas and operation authority continue to apply. *)
val run
  :  ?policy:policy
  -> ?context:context
  -> env:< mono_clock : _ Eio.Time.Mono.t ; .. >
  -> config:Chatml_host_runtime.runtime_config
  -> program:Chatml_host_runtime.compiled_script
  -> entrypoint:string
  -> arguments:Chatml.Chatml_lang.value list
  -> unit
  -> (Chatml.Chatml_lang.value, error) result
