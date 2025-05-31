(* Define the function call structure *)
type function_call =
  { arguments : string [@default ""]
  ; name : string [@default ""]
  }
[@@deriving jsonaf, sexp, bin_io]

(* Define the tool call structure *)
type tool_call_chunk =
  { id : string [@default ""]
  ; function_ : function_call [@key "function"]
  ; type_ : string [@key "type"] [@default ""]
  }
[@@deriving jsonaf, sexp, bin_io]

(* Define the tool call structure *)
type tool_call_default =
  { id : string option [@default None]
  ; function_ : function_call option [@key "function"] [@default None]
  ; type_ : string option [@key "type"] [@default None]
  }
[@@deriving jsonaf, sexp, bin_io]

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

type chat_message =
  { role : string
  ; content : chat_message_content option [@jsonaf.option]
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
[@@deriving jsonaf, sexp, bin_io]

type func =
  { name : string
  ; description : string option
  ; parameters : Jsonaf.t
  }
[@@deriving jsonaf, sexp]

type tool_func =
  { name : string
  ; description : string option
  ; parameters : Jsonaf.t
  ; strict : bool
  }
[@@deriving jsonaf, sexp]

type tool =
  { type_ : string [@key "type"]
  ; function_ : tool_func [@key "function"]
  }
[@@deriving jsonaf, sexp]

type schema =
  { description : string option [@jsonaf.option]
  ; name : string
  ; schema : Jsonaf.t
  ; strict : bool
  }
[@@deriving jsonaf, sexp]

type response_format =
  { type_ : string [@key "type"]
  ; json_schema : schema
  }
[@@deriving jsonaf, sexp]

(* Define the delta structure for streamed responses *)
type delta =
  { content : string option [@default None]
  ; function_call : function_call option [@jsonaf.option]
  ; refusal : string option [@default None]
  ; role : string option [@jsonaf.option]
  ; tool_calls : tool_call_chunk list option [@jsonaf.option]
  }
[@@deriving jsonaf, sexp, bin_io]

(* Define the choice structure for streamed responses *)
type stream_choice =
  { delta : delta
  ; finish_reason : string option [@default None]
  ; index : int
  }
[@@deriving jsonaf, sexp, bin_io]

(* Define the chat completion chunk for streamed responses *)
type chat_completion_chunk = { choices : stream_choice list }
[@@deriving jsonaf, sexp, bin_io]

(* Define the choice structure for default responses *)
type default_choice =
  { finish_reason : string option
  ; message : message
  }
[@@deriving jsonaf, sexp, bin_io]

(* Define the chat completion for default responses *)
type chat_completion = { choices : default_choice list } [@@deriving jsonaf, sexp, bin_io]

type _ response_type =
  | Stream : (stream_choice -> unit) -> unit response_type
  | Default : default_choice response_type

type model =
  | O3
  | O3_mini
  | Gpt4
  | O4_mini
  | Gpt4o
  | Gpt4_1
  | Gpt3
  | Gpt3_16k
[@@deriving jsonaf, sexp]

val model_of_str_exn : string -> model

val post_chat_completion
  : 'a.
  'a response_type
  -> ?max_tokens:int
  -> ?temperature:float
  -> ?functions:func list
  -> ?tools:tool list
  -> ?model:model
  -> ?reasoning_effort:string
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> 'n Eio.Net.t
  -> inputs:chat_message list
  -> 'a
