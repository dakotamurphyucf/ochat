open! Core

(** One bounded NDJSON client over caller-owned flows. This is the common
    request-id, response, notification, method-affinity and finite framing
    engine for Unix sockets and a child process's separate stdout/stdin pipes.
    [max_line_length] includes the newline terminator: all encoded outbound
    requests must leave one byte for it, and incoming response preflight makes
    the same allowance. It creates no socket or process and owns no
    mutation-retry state. *)

(** [close_flows] must idempotently close both borrowed flow ends. It must not
    await this client's reader or a child process: EOF, malformed input, full
    notification queue, failed/partial writes and Connection.close can all
    invoke it from the reader or writer. The caller keeps the owning Eio switch
    alive until the returned owner and its reader have closed. Cancellation
    propagates with its original backtrace; a failed/cancelled partial write
    retires the connection before a second request can append to it. *)
type t

val create
  :  sw:Eio.Switch.t
  -> transport_name:string
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> close_flows:(unit -> unit)
  -> max_line_length:int
  -> notification_capacity:int
  -> (t, Agent_protocol.Error.t) result

val connection : t -> Agent_client.Connection.t

(** Close the typed connection and join its one stdout protocol reader after
    closing its borrowed input flow. Do not call from that reader itself.
    Cancellation of the caller leaves the reader under its owning switch. *)
val close_and_wait : t -> unit
