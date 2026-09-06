open! Core

(** Bounded model-backed shell reviewer with strict JSON responses. *)

type completion =
  { text : string
  ; model : string
  ; input_tokens : int option
  ; output_tokens : int option
  }

type complete = prompt:string -> (completion, string) result
type t

val create
  :  env:Eio_unix.Stdenv.base
  -> ?wall_time_seconds:float
  -> ?max_prompt_bytes:int
  -> ?max_response_bytes:int
  -> id:string
  -> complete:complete
  -> unit
  -> t

val review : t -> Shell_access.Approval.request -> Shell_access.Approval.response

(** [review_result t request] returns transport and protocol failures without
    applying a hook failure policy. *)
val review_result
  :  t
  -> Shell_access.Approval.request
  -> (Shell_access.Approval.review, string) result
