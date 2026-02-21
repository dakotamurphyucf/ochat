open Core
open Io.Net
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

(** [api_key] is the API key for the OpenAI API. *)
let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:""

let host = Sys.getenv "EMBEDDINGS_HOST" |> Option.value ~default:"api.openai.com"

let model =
  Sys.getenv "EMBEDDINGS_MODEL" |> Option.value ~default:"text-embedding-3-large"
;;

(** Type definition for the input to the embeddings API. *)
type embeddings_input =
  { model : string
  ; input : string list
  }
[@@deriving jsonaf, sexp, bin_io]

(** Type definition for the response from the embeddings API. *)
type response = { data : embedding list }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(** Type definition for an individual embedding in the response. *)
and embedding =
  { embedding : float list
  ; index : int
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(** [post_openai_embeddings ~input env] makes an HTTP POST request to the OpenAI API embeddings endpoint with the given [input] and [env].

It returns the parsed response as a [response] record. *)
let generate_stub_embedding text ~dim : float list =
  (* Deterministic pseudo-random vector based on [text] hash so results are
     stable across runs and consistent between index and query. *)
  let seed = String.hash text |> abs in
  let st = Random.State.make [| seed |] in
  List.init dim ~f:(fun _ -> Random.State.float st 2.0 -. 1.0)
;;

let post_openai_embeddings net ~input =
  (* Fallback behaviour: if no API key is configured _or_ the environment
     requests stub embeddings (e.g. test runs without network access), we
     return deterministic pseudo-random vectors instead of calling the
     OpenAI API. *)
  let use_stub =
    String.is_empty api_key || Option.is_some (Sys.getenv "OPENAI_EMBEDDINGS_STUB")
  in
  if use_stub
  then (
    let dim = 128 in
    let data =
      List.mapi input ~f:(fun idx text ->
        { embedding = generate_stub_embedding text ~dim; index = idx })
    in
    { data })
  else (
    let headers =
      Http.Header.of_list
        [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
    in
    let input = { input; model } in
    let json = Jsonaf.to_string @@ jsonaf_of_embeddings_input input in
    let res = post Default ~net ~host ~headers ~path:"/v1/embeddings" json in
    let json = Jsonaf.of_string res in
    try response_of_jsonaf json with
    | _ as exe -> raise exe)
;;
