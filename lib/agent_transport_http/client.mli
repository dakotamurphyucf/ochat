open! Core

(** HTTP POST plus SSE client transport for the Ochat agent protocol. *)

(** [connect ~sw ~env ~uri ~bearer_token ~notification_capacity] creates a logical
    HTTP connection. SSE EOF, failed event requests, malformed event envelopes,
    body-reader failures and notification overflow terminate notification delivery
    after queued envelopes drain. Subsequent commands fail with [Interrupted].
    Transport-local retries never hide event-stream loss: the shared reconnect
    owner creates a new logical connection and reattaches with its durable cursor.
    Closing a failed connection does not send a network-dependent DELETE.
    Each connection owns a child switch; close cancels and joins its reader,
    response monitors and transport fibers without cancelling the caller's switch. *)
val connect
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> uri:Uri.t
  -> bearer_token:string option
  -> notification_capacity:int
  -> (Agent_client.Connection.t, Agent_protocol.Error.t) result
