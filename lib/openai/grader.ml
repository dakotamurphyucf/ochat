open Core
open Io.Net
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

(** Wrapper around the OpenAI "graders" HTTP API (currently in alpha)
    providing a thin, typed interface for running a {e score model}
    grader and retrieving the scalar reward.  The scope is deliberately
    kept minimal – just enough for the needs of
    {!module:Meta_prompting.Evaluator.Reward_model_judge}. *)

let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:""

(************************************************************)
(*                              Types                        *)
(************************************************************)

module Sampling_params = struct
  type t =
    { temperature : float option [@jsonaf.option]
    ; top_p : int option [@jsonaf.option]
    ; seed : int option [@jsonaf.option]
    ; reasoning_effort : string option [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, bin_io] [@@jsonaf.allow_extra_fields]
end

module Message = struct
  type t =
    { role : string
    ; content : string
    }
  [@@deriving sexp, jsonaf, bin_io]
end

module Grader = struct
  type t =
    { type_ : string [@key "type"]
    ; name : string
    ; input : Message.t list
    ; model : string
    ; sampling_params : Sampling_params.t option [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, bin_io]

  let create ?(model = "o3-2025-04-16") ?sampling_params ~name ~input () : t =
    { type_ = "score_model"; name; input; model; sampling_params }
  ;;
end

module Request = struct
  type t =
    { grader : Grader.t
    ; model_sample : string
    }
  [@@deriving sexp, jsonaf, bin_io]

  let create ~grader ~model_sample = { grader; model_sample }
end

module Response = struct
  (* We only care about the top-level [reward] field.  All other fields
     are retained as opaque JSON blobs so the decoder stays forward-
     compatible with potential API additions. *)
  type t =
    { reward : float
    ; metadata : Jsonaf.t
    }
  [@@deriving sexp, jsonaf, bin_io] [@@jsonaf.allow_extra_fields]
end

(************************************************************)
(*                         HTTP call                        *)
(************************************************************)

exception Grader_error of string

let post_run
      ?(host = "api.openai.com")
      ?(path = "/v1/fine_tuning/alpha/graders/run")
      ~net
      ~dir
      (req : Request.t)
  : Response.t
  =
  if String.is_empty api_key
  then raise (Grader_error "OPENAI_API_KEY environment variable not set")
  else (
    let headers =
      Http.Header.of_list
        [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
    in
    let body = Jsonaf.to_string (Request.jsonaf_of_t req) in
    (* Persist raw request for debugging when needed. *)
    Io.log ~dir ~file:"raw-openai-grader-request.txt" (body ^ "\n");
    let raw = post Default ~net ~host ~headers ~path body in
    (* Log raw response for traceability. *)
    Io.log ~dir ~file:"raw-openai-grader-response.txt" (raw ^ "\n");
    try Response.t_of_jsonaf (Jsonaf.of_string raw) with
    | ex ->
      raise
        (Grader_error
           (Printf.sprintf
              "Failed to parse OpenAI grader response: %s\nRaw: %s"
              (Exn.to_string ex)
              raw)))
;;

let run_score_model
      ?(name = "reward_model_judge")
      ?(model = "o3-2025-04-16")
      ?sampling_params
      ~prompt
      ~candidate
      ~dir
      ~net
      ()
  : float
  =
  (* Build the grader message – include the placeholder so the service
     substitutes the sample text. *)
  let content = prompt ^ "\n\nEvaluate: {{sample.output_text}}" in
  let message : Message.t = { role = "system"; content } in
  let grader = Grader.create ~name ~input:[ message ] ~model ?sampling_params () in
  let req = Request.create ~grader ~model_sample:candidate in
  let ({ Response.reward; _ } : Response.t) = post_run ~net ~dir req in
  reward
;;

(*------------------------------------------------------------------*)
(*                        Stub behaviour                            *)
(*------------------------------------------------------------------*)

let run_score_model_stub ~candidate:_ : float =
  (* Deterministic fallback value (0.5) when the API key is not
     available or during offline test runs. *)
  0.5
;;

let run_score_model_or_stub ~prompt ?sampling_params ~candidate ~dir ~net () : float =
  if String.is_empty api_key
  then run_score_model_stub ~candidate
  else run_score_model ~prompt ?sampling_params ~candidate ~dir ~net ()
;;
