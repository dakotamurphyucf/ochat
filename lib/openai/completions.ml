open Core
open Eio
open Io.Net
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

(** [api_key] is the API key for the OpenAI API. *)
let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:""

(* Define the function call structure *)

(** Type definition for the input to the completions API. *)
type function_call =
  { arguments : string [@default ""]
  ; name : string [@default ""]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(* Define the tool call structure *)
type tool_call_chunk =
  { id : string [@default ""]
  ; function_ : function_call [@key "function"]
  ; type_ : string [@key "type"] [@default ""]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(* Define the tool call structure *)
type tool_call_default =
  { id : string option [@default None]
  ; function_ : function_call option [@key "function"] [@default None]
  ; type_ : string option [@key "type"] [@default None]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(* First, define a type to represent each item in the array of content objects. *)
type image_url = { url : string } [@@deriving jsonaf, sexp]

type content_item =
  { type_ : string [@key "type"] (* e.g. "text" or "image_url" *)
  ; text : string option [@jsonaf.option]
  ; image_url : image_url option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

(* Next, define a sum type that can be either a string or a list of content items. *)
type chat_message_content =
  | Text of string
  | Items of content_item list
[@@deriving sexp, jsonaf]

(* Provide custom Jsonaf serialization/deserialization for chat_message_content. *)
let chat_message_content_of_jsonaf (json : Jsonaf_kernel__Type.t) =
  match json with
  | `String s -> Text s
  | `Array _ -> Items (list_of_jsonaf content_item_of_jsonaf json)
  | _ -> failwith "chat_message_content_of_jsonaf: Expected string or list."
;;

let jsonaf_of_chat_message_content = function
  | Text s -> `String s
  | Items lst -> jsonaf_of_list jsonaf_of_content_item lst
;;

(* Now integrate that into chat_message, using the [@jsonaf.of] and [@jsonaf.to] attributes
   to tell ppx_jsonaf how to handle the custom content field. *)
type chat_message =
  { role : string
  ; content : chat_message_content option
        [@jsonaf.option]
        [@jsonaf.of chat_message_content_of_jsonaf]
        [@jsonaf.to jsonaf_of_chat_message_content]
  ; name : string option [@jsonaf.option]
  ; tool_call_id : string option [@jsonaf.option]
  ; function_call : function_call option [@jsonaf.option]
  ; tool_calls : tool_call_default list option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving sexp, jsonaf]

(* Define the message structure for default responses *)
type message =
  { content : string option [@default None]
  ; refusal : string option [@default None]
  ; role : string
  ; function_call : function_call option [@jsonaf.option]
  ; tool_calls : tool_call_default list option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

type func =
  { name : string
  ; description : string option
  ; parameters : Jsonaf.t
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

type tool_func =
  { name : string
  ; description : string option
  ; parameters : Jsonaf.t
  ; strict : bool
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

type tool =
  { type_ : string [@key "type"]
  ; function_ : tool_func [@key "function"]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

type schema =
  { description : string option [@jsonaf.option]
  ; name : string
  ; schema : Jsonaf.t
  ; strict : bool
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

type response_format =
  { type_ : string [@key "type"]
  ; json_schema : schema
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

type completion_body =
  { model : string
  ; messages : chat_message list
  ; store : bool
  ; functions : func list option [@jsonaf.option]
  ; tools : tool list option [@jsonaf.option]
  ; temperature : float option [@jsonaf.option]
  ; top_p : float option [@jsonaf.option]
  ; n : int option [@jsonaf.option]
  ; stream : bool option [@jsonaf.option]
  ; stop : string list option [@jsonaf.option]
  ; max_completion_tokens : int option [@jsonaf.option]
  ; presence_penalty : float option [@jsonaf.option]
  ; frequency_penalty : float option [@jsonaf.option]
  ; logit_bias : (int * float) list option [@jsonaf.option]
  ; user : string option [@jsonaf.option]
  ; response_format : response_format option [@jsonaf.option]
  ; parallel_tool_calls : bool option [@jsonaf.option]
  ; reasoning_effort : string option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

let create_request_body
      ~model
      ~messages
      ?(store = true)
      ?functions
      ?tools
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
      ?parallel_tool_calls
      ?response_format
      ?reasoning_effort
      ()
  =
  { model
  ; messages
  ; store
  ; functions
  ; tools
  ; temperature
  ; top_p
  ; n
  ; stream
  ; stop
  ; max_completion_tokens = max_tokens
  ; presence_penalty
  ; frequency_penalty
  ; logit_bias
  ; user
  ; response_format
  ; parallel_tool_calls
  ; reasoning_effort
  }
;;

(* Define the delta structure for streamed responses *)
type delta =
  { content : string option [@default None]
  ; function_call : function_call option [@jsonaf.option]
  ; refusal : string option [@default None]
  ; role : string option [@jsonaf.option]
  ; tool_calls : tool_call_chunk list option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(* Define the choice structure for streamed responses *)
type stream_choice =
  { delta : delta
  ; finish_reason : string option [@default None]
  ; index : int
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(* Define the chat completion chunk for streamed responses *)
type chat_completion_chunk = { choices : stream_choice list }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(* Define the choice structure for default responses *)
type default_choice =
  { finish_reason : string option
  ; message : message
  }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

(* Define the chat completion for default responses *)
type chat_completion = { choices : default_choice list }
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]

type _ response_type =
  | Stream : (stream_choice -> unit) -> unit response_type
  | Default : default_choice response_type

(* let log ~env = Io.log ~dir:(Eio.Stdenv.fs env) *)
(* let console_log = Io.console_log *)

type model =
  | O3 [@name "o3"]
  | O3_mini [@name "o3-mini"]
  | Gpt4 [@name "gpt-4.5-preview"]
  | O4_mini [@name "o4-mini"]
  | Gpt4o [@name "gpt-4o"]
  | Gpt4_1 [@name "gpt-4.1"]
  | Gpt3 [@name "gpt-3.5-turbo"]
  | Gpt3_16k [@name "gpt-3.5-turbo-16k"]
[@@deriving sexp, bin_io]

let jsonaf_of_model = function
  | O3 -> `String "o3"
  | O3_mini -> `String "o3-mini"
  | O4_mini -> `String "o4-mini"
  | Gpt4_1 -> `String "gpt-4.1"
  | Gpt4 -> `String "gpt-4.5-preview"
  | Gpt4o -> `String "gpt-4o"
  | Gpt3 -> `String "gpt-3.5-turbo"
  | Gpt3_16k -> `String "gpt-3.5-turbo-16k"
;;

let model_of_jsonaf = function
  | `String "o3" -> O3
  | `String "o3-mini" -> O3_mini
  | `String "o4-mini" -> O4_mini
  | `String "gpt-4.1" -> Gpt4_1
  | `String "gpt-4.5-preview" -> Gpt4
  | `String "gpt-4o" -> Gpt4o
  | `String "gpt-3.5-turbo" -> Gpt3
  | `String "gpt-3.5-turbo-16k" -> Gpt3_16k
  | _ -> failwith "Invalid model"
;;

let model_to_str = function
  | O3 -> "o3"
  | O3_mini -> "o3-mini"
  | Gpt4o -> "gpt-4o"
  | O4_mini -> "o4-mini"
  | Gpt4_1 -> "gpt-4.1"
  | Gpt3 -> "gpt-3.5-turbo"
  | Gpt3_16k -> "gpt-3.5-turbo-16k"
  | Gpt4 -> "gpt-4.5-preview"
;;

let model_of_str_exn = function
  | "o3" -> O3
  | "o3-mini" -> O3_mini
  | "o4-mini" -> O4_mini
  | "gpt-4.1" -> Gpt4_1
  | "gpt-3.5-turbo" -> Gpt3
  | "gpt-3.5-turbo-16k" -> Gpt3_16k
  | "gpt-4.5" -> Gpt4
  | "gpt-4o" -> Gpt4o
  | _ -> failwith "Invalid model"
;;

let post_chat_completion
  : type a.
    a response_type
    -> ?max_tokens:int
    -> ?temperature:float
    -> ?functions:func list
    -> ?tools:tool list
    -> ?model:model
    -> ?reasoning_effort:string
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> _ Eio.Net.t
    -> inputs:chat_message list
    -> a
  =
  fun res_typ
    ?(max_tokens = 600)
    ?temperature
    ?functions
    ?tools
    ?(model = Gpt4)
    ?reasoning_effort
    ~dir
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
      ?tools
      ?temperature
      ~max_tokens
      ~stream
      ?reasoning_effort
      ()
  in
  let json = Jsonaf.to_string @@ jsonaf_of_completion_body input in
  let post json f = post ~net ~host ~headers ~path:"/v1/chat/completions" (Raw f) json in
  post json
  @@ fun res ->
  let _, reader = res in
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
       let event = chat_completion_of_jsonaf @@ Jsonaf.of_string @@ data in
       (List.hd_exn event.choices : a))
  | Stream cb ->
    let reader = Eio.Buf_read.of_flow reader ~max_size:Int.max_value in
    let lines = Buf_read.lines reader in
    let rec loop seq =
      match Seq.uncons seq with
      | None -> ()
      | Some (line, seq) ->
        Io.log ~dir ~file:"./raw-openai-chat-streaming-response.txt" (line ^ "\n");
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
           let line =
             String.concat
             @@ List.filter ~f:(fun s -> not @@ String.is_empty s)
             @@ String.split_lines line
           in
           let choice =
             match
               String.is_prefix ~prefix:"data: " line
               && (not @@ String.is_prefix ~prefix:"data: [DONE]" line)
             with
             | false -> None
             | true ->
               (match
                  Jsonaf.parse @@ String.chop_prefix_exn line ~prefix:"data: "
                  |> Result.bind ~f:(fun json -> Ok json)
                with
                | Ok json ->
                  let event = chat_completion_chunk_of_jsonaf @@ json in
                  Some (List.hd_exn event.choices)
                | Error _ -> None)
           in
           (match choice with
            | None ->
              let done_ =
                match line with
                | "data: [DONE]" -> true
                | _ -> false
              in
              if done_ then () else loop seq
            | Some choice ->
              cb choice;
              loop seq))
    in
    loop lines
;;
