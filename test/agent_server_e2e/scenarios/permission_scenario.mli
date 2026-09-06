open Core

val run : Eio_unix.Stdenv.base -> case:string option -> unit

(** Shared deterministic tool fixture for runtime assertions over the production
    HTTP listener. Every host and file belongs to the supplied temporary root. *)
type session =
  { summary : Agent_protocol.Session.t
  ; attachment : Agent_protocol.Session.Attachment.t
  }

val configure_fixture
  :  Eio_unix.Stdenv.base
  -> Support.Temporary_environment.t
  -> string
  -> profile:string
  -> tool_default:string
  -> ?approval_timeout:string
  -> ?fallback:string
  -> unit
  -> Support.Config_fixture.t

val with_client
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> (Support.Http_driver.t -> 'a)
  -> 'a

val create_session : Support.Http_driver.t -> string -> string -> session
val start_session : Support.Http_driver.t -> session -> string -> session

val send_message
  :  Support.Http_driver.t
  -> session
  -> string
  -> Agent_protocol.Method_result.Send_message.t

val await_pending_permission
  :  Eio_unix.Stdenv.base
  -> Support.Http_driver.t
  -> Agent_protocol.Id.Session.t
  -> int
  -> Agent_protocol.Permission.t

val await_operation_end
  :  Eio_unix.Stdenv.base
  -> Support.Http_driver.t
  -> Agent_protocol.Id.Session.t
  -> int
  -> Agent_protocol.Session.t

val respond_approve
  :  Support.Http_driver.t
  -> session
  -> Agent_protocol.Permission.t
  -> string
  -> unit

val model_post_stream
  :  ?before_tool_call:(unit -> unit)
  -> string
  -> Chat_response.In_memory_stream.post_stream

val message_stream : unit -> Openai.Responses.Response_stream.t list
