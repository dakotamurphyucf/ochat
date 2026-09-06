open Core

(** [run env ~case] runs the shell manifest, approval-grant, and redaction
    E2E scenario. Live redaction probes subscribe to production HTTP SSE
    before invoking a real shell tool with explicitly registered literal and
    base64 secret forms split
    into one-character provider deltas. Check reassembled sourced and
    history-correlated arguments, direct starts and nested fork start traces.

    Argument display waits for a complete payload so secrets cannot cross
    published delta boundaries; execution still receives the original input.
    Direct probes collect through operation completion. The nested trace probe
    collects through the actual nested start and does not assert fork lifecycle
    completion, jobs, raw provider logs or arbitrary progress-text redaction. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
