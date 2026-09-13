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

(** Shared production-host fixture for recursive fork/tool compatibility probes. *)
type session =
  { summary : Agent_protocol.Session.t
  ; attachment : Agent_protocol.Session.Attachment.t
  }

val fail : string -> 'a
val require : bool -> string -> unit
val protocol_ok : ('a, Agent_protocol.Error.t) result -> 'a
val save : Support.Temporary_environment.t -> string -> string -> unit
val live_prompt : unit -> string

val live_fixture
  :  Eio_unix.Stdenv.base
  -> Support.Temporary_environment.t
  -> string
  -> Support.Config_fixture.t

val live_tool_stream : string -> string -> int -> Openai.Responses.Response_stream.t list
val message_stream : int -> Openai.Responses.Response_stream.t list

val with_client
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> (Support.Http_driver.t -> 'a)
  -> 'a

val create_session : Support.Http_driver.t -> string -> session
val start_session : Support.Http_driver.t -> session -> string -> session

val capture_live
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Support.Http_driver.t
  -> session
  -> nested:bool
  -> Agent_protocol.Event.Recoverable.t list

val is_nested_tool_start : Agent_protocol.Event.Recoverable.t -> bool
val require_live_tool_events : Agent_protocol.Event.Recoverable.t list -> bool -> unit
