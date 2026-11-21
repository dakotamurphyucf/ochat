(** Facts from the actual root request after all canonical tool output writes
    and post-tool observers returned. They cannot authorize a host transition. *)

type pair =
  { call : History_entry.Id.t
  ; output : History_entry.Id.t
  }

type t

val create
  :  Inference_runtime.Attempt.t
  -> Inference_runtime.Receipt.t
  -> (Transcript.Item.Key.t * pair) list
  -> t option

val attempt : t -> Inference_runtime.Attempt.t
val terminal : t -> Inference.Event.Terminal.t
val pairs : t -> pair list

type decision =
  | Continue
  | Retire
