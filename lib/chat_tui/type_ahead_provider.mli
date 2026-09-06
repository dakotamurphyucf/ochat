open! Core

(** Private, bounded, client-local typeahead requests. No draft or response logs. *)

type input = private
  { draft : string
  ; history : string
  }

type outcome = (string, [ `Unavailable | `Timeout ]) result

(** [prepare config ...] windows the draft to 8192 UTF-8 bytes plus markers,
    and opted-in visible history to 16384 bytes. Never supply hidden/tool data. *)
val prepare
  :  Type_ahead_config.t
  -> messages:(string * string) list
  -> draft:string
  -> cursor:int
  -> input

(** [sanitize text] removes outer fences, cursor markers and terminal controls,
    limiting the insertion to 4096 UTF-8 bytes. *)
val sanitize : string -> string

(** [inputs input] pairs developer insertion instructions and a partial-word
    example with user context and draft enclosed in explicit section delimiters.
    Delimiters do not change the bounds applied by [prepare]. *)
val inputs : input -> Openai.Responses.Item.t list

(** [complete_with ...] enforces a ten-second total deadline and redacts errors.
    Cancellation propagates. Injectable request supports offline verification. *)
val complete_with
  :  clock:_ Eio.Time.clock
  -> request:(Openai.Responses.Item.t list -> Openai.Responses.Response.t)
  -> input
  -> outcome

val complete_suffix
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> config:Type_ahead_config.t
  -> input
  -> outcome
