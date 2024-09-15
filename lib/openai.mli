(** This module provides functions to interact with the OpenAI API for embeddings.

    It includes functions to make HTTP POST requests to the OpenAI API embeddings endpoint and parse the response. *)

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

type func =
  { name : string
  ; description : string option
  ; parameters : Jsonaf.t
  }
[@@deriving jsonaf, sexp]

(** [post_openai_embeddings ~input net] makes an HTTP POST request to the OpenAI API embeddings endpoint with the given [input] and [net].

    It returns the parsed response as a [response] record. *)
val post_openai_embeddings : _ Eio.Net.t -> input:string list -> response

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

type delta =
  { role : string option [@jsonaf.option]
  ; content : string option [@jsonaf.option]
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

type _ response_type =
  | Stream : (choice -> unit) -> unit response_type
  | Default : message_response response_type

type model =
  | Gpt4
  | Gpt3
  | Gpt3_16k

(** [post_chat_completion ?max_tokens ?stream env inputs] makes an HTTP POST request to the OpenAI API chat completion endpoint with the given optional [max_tokens], optional [stream], [env], and [inputs].

    It returns the parsed response as a string. *)

val post_chat_completion
  : 'a.
  'a response_type
  -> ?max_tokens:int
  -> ?temperature:float
  -> ?functions:func list
  -> ?model:model
  -> 'n Eio.Net.t
  -> inputs:message list
  -> 'a

(* module type GPT_PROMPT_INTERACTION = sig
   (* Type representing a problem with its interaction history *)
   type problem

   (* Type representing a tool *)
   type tool

   (* Type representing a tool output *)
   type tool_output
   type tool_input

   (* Type representing a response *)
   type response

   (* Type representing a prompt output *)
   type prompt_output = ToolInput of tool_input | Response of response

   (* Function to create a problem *)
   val create_problem :  unit -> problem

   (* Function to update the problem with a new interaction *)
   val update_problem :
   problem -> tool_output -> (problem, string) result

   (* Function to get the current prompt from the problem *)
   val get_current_prompt : problem -> string

   (* Function to get the interaction history from the problem *)
   val get_interaction_history : problem -> prompt_output list

   (* Function to check if a final answer has been reached or a threshold is met *)
   val is_threshold_reached :
   problem -> bool

   end *)
(* module type OPENAI_CALL = sig
   module Gpt: GPT_PROMPT_INTERACTION
   type openai_response
   type openai_request
   val env : < net : Eio.Net.t ; stdout : #Eio.Flow.sink ; .. >
   (* Function to map a problem to an OpenAI call *)
   val make_openai_call : Gpt.problem -> openai_request

   (* Function to map an OpenAI response to a prompt_output *)
   val parse_openai_response : openai_response -> Gpt.prompt_output
   end

   module type ProblemRuntime =
   functor

   (M: OPENAI_CALL )
   ->
   sig
   exception Threshold_reached
   type runtime
   module Gpt = M.Gpt

   (* Function to create a new runtime for a problem *)
   val create_runtime : unit -> runtime

   (* Function to evaluate the problem using the runtime *)
   val evaluate_problem : runtime -> Gpt.problem -> response
   end *)
