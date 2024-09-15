(** This module provides functions to interact with the OpenAI API for embeddings.

    It includes functions to make HTTP POST requests to the OpenAI API embeddings endpoint and parse the response. *)

open Core
open Eio
open Io.Net
open Jsonaf.Export

(** Type definition for the input to the embeddings API. *)
type embeddings_input =
  { model : string
  ; input : string list
  }
[@@deriving jsonaf, sexp, bin_io]

(** Type definition for the response from the embeddings API. *)
type response = { data : embedding list } [@@jsonaf.allow_extra_fields]

(** Type definition for an individual embedding in the response. *)
and embedding =
  { embedding : float list
  ; index : int
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(** [api_key] is the API key for the OpenAI API. *)
let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:""

(** [post_openai_embeddings ~input env] makes an HTTP POST request to the OpenAI API embeddings endpoint with the given [input] and [env].

    It returns the parsed response as a [response] record. *)
let post_openai_embeddings net ~input =
  let host = "api.openai.com" in
  let headers =
    Http.Header.of_list
      [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
  in
  let input = { input; model = "text-embedding-ada-002" } in
  let json = Jsonaf.to_string @@ jsonaf_of_embeddings_input input in
  let res = post Default ~net ~host ~headers ~path:"/v1/embeddings" json in
  let json = Jsonaf.of_string res in
  try response_of_jsonaf json with
  | _ as exe ->
    print_endline @@ Fmt.str "%a" Eio.Exn.pp exe;
    print_endline @@ String.concat ~sep:"\n" input.input;
    print_endline @@ Jsonaf.to_string json;
    raise exe
;;

type function_call =
  { name : string [@default ""]
  ; arguments : string [@default ""]
  }
[@@deriving jsonaf, sexp, bin_io]

type message =
  { role : string
  ; content : string option
  ; name : string option [@jsonaf.option]
  ; function_call : function_call option [@jsonaf.option]
  }
[@@deriving jsonaf, sexp, bin_io]

type func =
  { name : string
  ; description : string option
  ; parameters : Jsonaf.t
  }
[@@deriving jsonaf, sexp]

type completion_body =
  { model : string
  ; messages : message list
  ; functions : func list option [@jsonaf.option]
  ; temperature : float option [@jsonaf.option]
  ; top_p : float option [@jsonaf.option]
  ; n : int option [@jsonaf.option]
  ; stream : bool option [@jsonaf.option]
  ; stop : string list option [@jsonaf.option]
  ; max_tokens : int option [@jsonaf.option]
  ; presence_penalty : float option [@jsonaf.option]
  ; frequency_penalty : float option [@jsonaf.option]
  ; logit_bias : (int * float) list option [@jsonaf.option]
  ; user : string option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

let create_request_body
  ~model
  ~messages
  ?functions
  ?temperature
  ?top_p
  ?n
  ?stream
  ?stop
  ?max_tokens
  ?presence_penalty
  ?frequency_penalty
  ?logit_bias
  ?user
  ()
  =
  { model
  ; messages
  ; functions
  ; temperature
  ; top_p
  ; n
  ; stream
  ; stop
  ; max_tokens
  ; presence_penalty
  ; frequency_penalty
  ; logit_bias
  ; user
  }
;;

type delta =
  { role : string option [@jsonaf.option]
  ; content : string option [@default None]
  ; function_call : function_call option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

type choice =
  { delta : delta
  ; finish_reason : string option
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

type message_response =
  { message : delta
  ; finish_reason : string option
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

type stream_event = { choices : choice list }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

type default_response = { choices : message_response list }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

type _ response_type =
  | Stream : (choice -> unit) -> unit response_type
  | Default : message_response response_type

(* let log ~env = Io.log ~dir:(Eio.Stdenv.fs env) *)
(* let console_log = Io.console_log *)

type model =
  | Gpt4
  | Gpt3
  | Gpt3_16k

let model_to_str = function
  | Gpt3 -> "gpt-3.5-turbo"
  | Gpt3_16k -> "gpt-3.5-turbo-16k"
  | Gpt4 -> "gpt-4-1106-preview"
;;

let post_chat_completion
  : type a.
    a response_type
    -> ?max_tokens:int
    -> ?temperature:float
    -> ?functions:func list
    -> ?model:model
    -> _ Eio.Net.t
    -> inputs:message list
    -> a
  =
  fun res_typ
    ?(max_tokens = 600)
    ?(temperature = 0.0)
    ?functions
    ?(model = Gpt4)
    net
    ~inputs ->
  let host = "api.openai.com" in
  let headers =
    Http.Header.of_list
      [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
  in
  let messages = inputs in
  let stream =
    match res_typ with
    | Default -> false
    | Stream _ -> true
  in
  let input =
    create_request_body
      ~model:(model_to_str model)
      ~messages
      ?functions
      ~temperature
      ~max_tokens
      ~stream
      ()
  in
  let json = Jsonaf.to_string @@ jsonaf_of_completion_body input in
  let post json f = post ~net ~host ~headers ~path:"/v1/chat/completions" (Raw f) json in
  post json
  @@ fun res ->
  let response, reader = res in
  match res_typ with
  | Default ->
    let read_all flow = Eio.Buf_read.(parse_exn take_all) flow ~max_size:Int.max_value in
    let data = read_all reader in
    let json_result =
      Jsonaf.parse data
      |> Result.bind ~f:(fun json ->
        match Jsonaf.member "error" json with
        | None -> Or_error.error_string ""
        | Some json -> Ok json)
    in
    (match json_result with
     | Ok _ -> failwith data
     | Error _ ->
       let event = default_response_of_jsonaf @@ Jsonaf.of_string @@ data in
       (List.hd_exn event.choices : a))
  | Stream cb ->
    let reader = Eio.Buf_read.of_flow reader ~max_size:Int.max_value in
    (* EDIT HERE *)
    let rec loop () =
      match Eio.Buf_read.at_end_of_input reader with
      | true ->
        (* (match res_typ with
           | Default -> failwith "should not be at end of input for Default"
           | Stream _ -> ()) *)
        ()
      | false ->
        let line =
          match Http.Response.content_length response with
          | Some content_length -> Buf_read.take content_length reader
          | None -> Buf_read.take (Buf_read.buffered_bytes reader) reader
        in
        (* EDIT HERE *)
        let json_result =
          Jsonaf.parse line
          |> Result.bind ~f:(fun json ->
            match Jsonaf.member "error" json with
            | None -> Or_error.error_string ""
            | Some json -> Ok json)
        in
        (match json_result with
         | Ok _ -> failwith line
         | Error _ ->
           let lines = String.split_lines line in
           let choices =
             List.filter_map
               ~f:(fun s ->
                 match
                   String.is_prefix ~prefix:"data: " s
                   && (not @@ String.is_prefix ~prefix:"data: [DONE]" s)
                 with
                 | false -> None
                 | true ->
                   let event =
                     stream_event_of_jsonaf
                     @@ Jsonaf.of_string
                     @@ String.chop_prefix_exn s ~prefix:"data: "
                   in
                   Some (List.hd_exn event.choices))
               lines
           in
           List.iter ~f:cb choices;
           let done_ =
             List.exists
               ~f:(fun res ->
                 match res with
                 | "data: [DONE]" -> true
                 | _ -> false)
               lines
           in
           if done_ then () else loop ())
    in
    loop ()
;;
