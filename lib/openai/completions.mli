(* ———————————————————————————————————————————————————————————————— *)
(** Chat completions (a.k.a. “ChatGPT”) client.

    This module exposes a minimal yet expressive wrapper around the
    OpenAI [v1/chat/completions] endpoint.  It is designed for advanced
    use-cases that require

    • *Capability-safe*, effect-typed IO (uses {!module:Eio})
    • *Streaming* responses delivered incrementally
    • *Tool calling* / *function calling* with full JSON schemas

    The high-level helper {!post_chat_completion} is all you need for
    most workflows.  All record types below are faithful OCaml mirrors
    of OpenAI’s public JSON schema – you will typically construct a
    list of {!chat_message} values, invoke [post_chat_completion], and
    then pattern-match on the returned choices.

    {1 Environment}

    The environment variable [OPENAI_API_KEY] must contain a valid
    secret key.  A missing or empty key will result in HTTP 401.

    {1 Quick-start}

    {[
      open Completions

      let assistant ?(model = Gpt3) net ~dir ~prompt =
        let user : chat_message =
          { role = "user"; content = Some (Text prompt); name = None
          ; tool_call_id = None; function_call = None; tool_calls = None }
        in
        match
          post_chat_completion Default ~dir net ~inputs:[ user ] ~model
        with
        | { finish_reason = Some "stop"; message = { content = Some c; _ } } ->
          c
        | _ -> failwith "Unexpected reply"
    ]}

    {1 Error handling}

    Network failures, JSON decoding issues, or non-2xx HTTP responses
    are raised as exceptions.  Wrap calls with {!Io.to_res} or similar
    helpers if you prefer an error-returning style. *)

(* —————————————————————————————————————————————————————————   REQUEST  — *)

type function_call =
  { arguments : string (** Raw JSON string with the arguments payload. *)
  ; name : string (** Name of the server-side function to invoke. *)
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

(** Individual piece of content that can either be plain text or an
    image reference.  Used when the user or assistant sends mixed
    media.  For convenience, the helper {!type:chat_message_content}
    wraps this in a variant. *)

(* Next, define a sum type that can be either a string or a list of content items. *)
type chat_message_content =
  | Text of string
  | Items of content_item list
[@@deriving sexp, jsonaf]

(** Content of a single chat message – either a raw UTF-8 string
    ([Text]) or a structured list of {!content_item}s ([Items]). *)

type chat_message =
  { role : string
  ; content : chat_message_content option [@jsonaf.option]
  ; name : string option [@jsonaf.option]
  ; tool_call_id : string option [@jsonaf.option]
  ; function_call : function_call option [@jsonaf.option]
  ; tool_calls : tool_call_default list option [@jsonaf.option]
  }
[@@jsonaf.allow_extra_fields] [@@deriving sexp, jsonaf]

(** A complete chat message ready for transmission.  At minimum a
    message must specify a [role] ("user", "assistant", "system" …)
    and optionally carries text, image URLs, function calls, etc.  For
    most interactions you’ll build a small list of such records and
    pass them to {!post_chat_completion}. *)

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

(** JSON-schema description of a callable function that the model may
    invoke.  Follows OpenAI’s *function calling* specification. *)

type tool_func =
  { name : string
  ; description : string option
  ; parameters : Jsonaf.t
  ; strict : bool
  }
[@@deriving jsonaf, sexp]

(** Richer description used when ["type" = "tool"].  The additional
    [strict] flag enforces schema compliance on the server side. *)

type tool =
  { type_ : string [@key "type"]
  ; function_ : tool_func [@key "function"]
  }
[@@deriving jsonaf, sexp]

(** One entry in the [tools] array accepted by the API. *)

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

(** Desired format for the assistant reply.  For example setting
    [{type_ = "json_object"; _}] instructs the model to emit valid
    JSON. *)

(* Define the delta structure for streamed responses *)
type delta =
  { content : string option [@default None]
  ; function_call : function_call option [@jsonaf.option]
  ; refusal : string option [@default None]
  ; role : string option [@jsonaf.option]
  ; tool_calls : tool_call_chunk list option [@jsonaf.option]
  }
[@@deriving jsonaf, sexp, bin_io]

(** Partial update emitted when streaming is enabled.  The
    {!post_chat_completion} function converts each chunk to a
    {!stream_choice} and passes it to the callback supplied to
    {!constructor:Stream}. *)

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

(** Completed choice when the request is *not* streamed.  The field
    [message] contains the assistant answer.  [finish_reason] mirrors
    OpenAI’s reasons such as ["stop"], ["length"], etc. *)

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

(** Supported model identifiers (subset).  Use {!model_of_str_exn} to
    convert a raw string received from configuration or user input. *)

val model_of_str_exn : string -> model

(** [model_of_str_exn s] converts [s] to a concrete {!type:model}
    value.  @raise Failure if [s] is not a recognised model name. *)

(** [post_chat_completion res ?max_tokens ?temperature ?functions
    ?tools ?model ?reasoning_effort ~dir net ~inputs] sends the chat
    messages [inputs] to the OpenAI completions endpoint and returns a
    value that depends on [res]:

    • [Default] – returns a {!default_choice} containing the full
      assistant reply.
    • [Stream f] – enables server-side streaming and invokes the
      callback [f] for every {!stream_choice} chunk received.  The
      function then returns [unit].

    Parameters:
    @param max_tokens  Hard cap on tokens generated by the model
                       (default 600).
    @param temperature Sampling temperature in the range [0,2].
    @param functions   List of function descriptions the model may
                       call (legacy API).
    @param tools       Rich tool list (preferred over [functions]).
    @param model       Target model (default {!Gpt4}).
    @param reasoning_effort Hints the preferred quality/speed trade-off.
    @param dir         Directory capability used by the helper
                       {!Io.log} to dump debugging traces.
    @param net         Network capability obtained from
                       [Eio.Stdenv.net].
    @param inputs      Ordered list of user and system messages.

    Example – blocking call:
    {[
      let reply =
        Completions.post_chat_completion
          Completions.Default
          ~dir
          net
          ~inputs:[ (* build messages *) ]
      in
      Format.printf "Assistant said: %s\n"
        (Option.value ~default:"" reply.message.content)
    ]}

    Example – streaming call:
    {[
      let stdout_choice { Completions.delta = { content; _ }; _ } =
        Option.iter content ~f:print_endline in
      Completions.post_chat_completion
        (Completions.Stream stdout_choice)
        ~model:Completions.Gpt3
        ~dir net ~inputs
    ]} *)
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
