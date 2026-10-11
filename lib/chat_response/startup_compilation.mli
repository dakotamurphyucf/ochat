open Core

(** One caller-domain compilation preparation context. It is borrowed only during
    [with_context], never retained by artifacts or sent to compiler domains.
    Runtime initializer execution and fuel are separate from this deadline. *)
type t

(** Borrow an existing invocation, or start one before validation. Default whole
    invocation ceilings are the existing distinct text/help 8 MiB and distinct
    canonical graph-input 8 MiB limits. Explicit smaller ceilings support hosts
    with tighter policy. Original checkpoint exceptions/backtraces propagate,
    including Timeout; the context rejects use after the callback returns.
    A borrowed context requires the same original limits; it cannot weaken a
    caller's smaller policy. This scopes lifetime, without timing the whole
    callback/activation. Foreign-domain use rejects before accessing the ledger. *)
val with_context
  :  ?startup:t
  -> ?max_distinct_text_bytes:int
  -> ?max_distinct_graph_input_bytes:int
  -> limits:Chatml_compilation.limits
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> map_error:(Chatml_compilation.error -> 'error)
  -> f:(t -> ('a, 'error) Result.t)
  -> unit
  -> ('a, 'error) Result.t

(** Check current caller cancellation before/after the original callback, then
    the absolute deadline. Policy rejection is handled by [within]/[with_context].
    Never call from another domain or wrap this callback in catch-all mapping. *)
val checkpoint : t -> unit

(** Exact source/schema/canonical-help bytes deduplicate across preparation
    phases. Candidate length is checked before chunked hashing; digest buckets
    compare exact bytes with polling. No copy of the text is retained. *)
val admit_text : t -> string -> (unit, Chatml_compilation.error) Result.t

(** Canonical source+metadata input deduplicates by complete Prepared fingerprint.
    Different scopes/contracts count separately even with shared physical bodies.
    This does not replace custody's per-program logical accounting or tool checks. *)
val admit_prepared
  :  t
  -> Chatml_prepared_program.t
  -> (unit, Chatml_compilation.error) Result.t

(** Time remaining on the original deadline, retaining the original per-input
    source ceiling. Expiration rejects; it never grants a fresh/clamped budget. *)
val remaining_limits : t -> (Chatml_compilation.limits, Chatml_compilation.error) Result.t

(** Time one actual host preparation/preflight phase using the original remaining
    allowance. Do not include activation. Compiler calls inside remain joined by
    their existing worker on cancellation; original failures propagate unchanged. *)
val within
  :  t
  -> map_error:(Chatml_compilation.error -> 'error)
  -> f:(unit -> ('a, 'error) Result.t)
  -> ('a, 'error) Result.t
