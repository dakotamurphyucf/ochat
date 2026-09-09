open Core

(** Host-owned pre-tool routing for synchronous descendants. The callback must
    run the owning session's policy and emit runtime requests at that owned
    boundary. It grants no native capability or actor executor. Capturing it
    does not extend its lifetime or permit an active moderator to re-enter itself. *)
type t

val with_handler
  :  observer:Agent_protocol.Invocation.observer option
  -> prepare:
       (Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Tool_moderation.t option, string) result)
  -> (unit -> 'a)
  -> 'a

(** Carry only through an owned handoff. Explicit None shadows ambient handlers. *)
val capture : unit -> t option

val with_context : t option -> (unit -> 'a) -> 'a
val current : unit -> (t, string) result
val observer : t -> Agent_protocol.Invocation.observer option

(** Fails after the lexical owner returns, before entering the callback. *)
val prepare
  :  t
  -> Chat_response.Moderation.Tool_call.t
  -> (Chat_response.Moderation.Tool_moderation.t option, string) result
