open Core

(** Direct HTTP and SSE driver for the agent-server E2E scenarios. *)

type t

type response =
  { status : int
  ; headers : Piaf.Headers.t
  ; body : string
  }

type rpc_response =
  { result : Agent_protocol.Method_result.t
  ; response : response
  }

module Sse : sig
  type t

  type event =
    { id : string option
    ; event : string option
    ; data : string
    }
  [@@deriving sexp]

  val next
    :  t
    -> clock:_ Eio.Time.clock
    -> timeout_seconds:float
    -> (event, string) result

  val close : t -> unit
end

val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> port:int
  -> token:string option
  -> (t, string) result

val connection_id : t -> string option
val set_connection_id : t -> string option -> unit

val request_raw
  :  t
  -> ?headers:(string * string) list
  -> ?body:string
  -> meth:Piaf.Method.t
  -> path:string
  -> unit
  -> (response, string) result

val rpc_raw : t -> ?headers:(string * string) list -> string -> (response, string) result

val request
  :  t
  -> Agent_protocol.Command.t
  -> (rpc_response, Agent_protocol.Error.t) result

val initialize
  :  t
  -> (Agent_protocol.Initialize.Response.t * response, Agent_protocol.Error.t) result

val notify : t -> Agent_protocol.Command.t -> (response, string) result
val close_connection : t -> (response, string) result
val open_connection_events : t -> sw:Eio.Switch.t -> (Sse.t * response, string) result

val open_session_events
  :  t
  -> sw:Eio.Switch.t
  -> session_id:Agent_protocol.Id.Session.t
  -> ?buffer_capacity:int
  -> ?after_sequence:int64
  -> ?last_event_id:int64
  -> unit
  -> (Sse.t * response, string) result

val get_snapshot
  :  t
  -> Agent_protocol.Id.Session.t
  -> (Agent_protocol.Snapshot.t * response, string) result

val shutdown : t -> unit
