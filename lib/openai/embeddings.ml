open Core
open Io.Net
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

(** [api_key] is the API key for the OpenAI API. *)
let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:""

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
let post_openai_embeddings net ~input =
  let host = "api.openai.com" in
  let headers =
    Http.Header.of_list
      [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
  in
  let input = { input; model = "text-embedding-3-large" } in
  let json = Jsonaf.to_string @@ jsonaf_of_embeddings_input input in
  let res = post Default ~net ~host ~headers ~path:"/v1/embeddings" json in
  let json = Jsonaf.of_string res in
  try response_of_jsonaf json with
  | _ as exe ->
    (* print_endline @@ Fmt.str "%a" Eio.Exn.pp exe;
    print_endline @@ String.concat ~sep:"\n" input.input;
    print_endline @@ Jsonaf.to_string json; *)
    raise exe
;;
