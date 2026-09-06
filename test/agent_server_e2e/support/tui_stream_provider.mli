open Core

(** Gated loopback SSE provider. Requests remain open until the test emits each
    phase and explicitly finishes, making intermediate TUI assertions causal. *)
type t

type request

val start : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> port:int -> t
val await_request : t -> Eio_unix.Stdenv.base -> int -> request
val request_count : t -> int
val request_at : t -> int -> request option
val body : request -> Jsonaf.t
val emit : request -> Openai.Responses.Response_stream.t list -> unit
val finish : request -> unit

(** Answer one gated non-streaming compaction request. Reject stream requests
    and double completion. *)
val reply_summary : request -> string -> unit

val reasoning : string -> string -> Openai.Responses.Response_stream.Item.t
val message : string -> string -> Openai.Responses.Response_stream.Item.t
val added : Openai.Responses.Response_stream.Item.t -> Openai.Responses.Response_stream.t
val done_ : Openai.Responses.Response_stream.Item.t -> Openai.Responses.Response_stream.t
val reasoning_delta : string -> string -> Openai.Responses.Response_stream.t
val text_delta : string -> string -> Openai.Responses.Response_stream.t
val fork_call : unit -> Openai.Responses.Response_stream.t list

(** [fork_call_for ~call_id] creates a fork with unique caller-supplied identities
    for repeated manual turns. *)
val fork_call_for : call_id:string -> Openai.Responses.Response_stream.t list
