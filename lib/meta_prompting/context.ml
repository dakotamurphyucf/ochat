open Core
module Model = Openai.Responses.Request

type prompt_type =
  | General
  | Tool

type action =
  | Generate
  | Update

type t =
  { proposer_model : Model.model option
  ; rng : Random.State.t
  ; env : Eio_unix.Stdenv.base option
  ; guidelines : string option
  ; model_to_optimize : Model.model option
  ; action : action
  ; prompt_type : prompt_type
  }

let default () : t =
  (* Deterministic default RNG to keep inline tests reproducible. *)
  { proposer_model = None
  ; rng = Random.State.make [| 0x1337beef |]
  ; env = None
  ; guidelines = None
  ; model_to_optimize = Some Model.O3
  ; action = Generate
  ; prompt_type = General
  }
;;

let with_proposer_model (ctx : t) ~(model : Model.model option) : t =
  { ctx with proposer_model = model }
;;

let with_guidelines (ctx : t) ~(guidelines : string option) : t = { ctx with guidelines }
