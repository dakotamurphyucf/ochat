open Core

type prompt_type =
  | General
  | Tool

type action =
  | Generate
  | Update

type t =
  { proposer_model : string option
  ; rng : Random.State.t
  ; env : Eio_unix.Stdenv.base option
  ; inference : Inference_client.Execution.t option
  ; guidelines : string option
  ; model_to_optimize : string option
  ; action : action
  ; prompt_type : prompt_type
  }

let default () : t =
  (* Deterministic default RNG to keep inline tests reproducible. *)
  { proposer_model = None
  ; rng = Random.State.make [| 0x1337beef |]
  ; env = None
  ; inference = None
  ; guidelines = None
  ; model_to_optimize = None
  ; action = Generate
  ; prompt_type = General
  }
;;

let with_proposer_model (ctx : t) ~(model : string option) : t =
  { ctx with proposer_model = model }
;;

let with_guidelines (ctx : t) ~(guidelines : string option) : t = { ctx with guidelines }
