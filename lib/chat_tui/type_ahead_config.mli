open! Core

(** Client-local, opt-in suggestion settings, independent of the agent model. *)

type mode =
  | Off
  | Manual
  | Auto
[@@deriving sexp, equal]

type t = private
  { mode : mode
  ; model : Openai.Responses.Request.model
  ; history_messages : int
  ; debounce_ms : int
  ; max_output_tokens : int
  }

val default : t

(** [create ...] validates all bounds without accessing the environment. *)
val create
  :  mode:string
  -> model:string
  -> history_messages:int
  -> debounce_ms:int
  -> max_output_tokens:int
  -> t Or_error.t

(** [validate_credentials t ~api_key] requires a nonblank local key when enabled.
    Never include credentials in returned errors. *)
val validate_credentials : t -> api_key:string option -> unit Or_error.t
