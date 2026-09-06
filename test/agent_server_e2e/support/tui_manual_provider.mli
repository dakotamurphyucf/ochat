(** Operator-controlled Responses fixture; never invokes a real model. *)
type t

val create : Tui_stream_provider.t -> t

(** [respond t ~env ~action ~index] answers, holds, completes, forks, or summarizes
    one accepted request. The background action returns the fixed background-result
    text for either a streaming or non-streaming nested job. The suggest action
    returns fifteen deterministic lines with wide text and a request-indexed end
    marker for a nonstreaming request. The automatic driver delays each response
    independently for two seconds. Invalid phase
    transitions fail without sending data. *)
val respond : t -> env:Eio_unix.Stdenv.base -> action:string -> index:int -> unit
