(** Explicitly authorized, text-only live OpenAI fixture relay. Only
    [gpt-5.6-sol] and client-executed tools are allowed. Requests are capped at
    32 KiB serialized input and 4096 output tokens, default service tier, no
    stored/hidden context. Reserve a conservative $1 before each upstream
    request, at most 14 requests across invocations sharing the persistent ledger.
    Published default rates verified September 6, 2026 are $4/M input and $20/M
    output. This is a conservative reservation, not an actual billing reading.
    Run only one live fixture process at a time; the ledger mutex is process-local.
    The key stays in the relay and is registered with fixture redaction.
    [OCHAT_E2E_STREAM_DIAGNOSTICS=1] observes consumed bytes and SSE event names.
    Unknown-length upstream bodies use explicit HTTP/1.1 chunked framing.
    The original body and its upstream error state are preserved. *)

type t

(** [response_for_http1 response] adds explicit chunked framing for unknown-length
    upstream bodies, removing conflicting framing headers without replacing or
    consuming the body. Other known body lengths retain their framing. *)
val response_for_http1 : Piaf.Response.t -> Piaf.Response.t

val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> environment:Temporary_environment.t
  -> port:int
  -> t

val metrics : t -> (string * Jsonaf.t) list
val forwarded : t -> int
