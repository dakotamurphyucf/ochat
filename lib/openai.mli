module Embeddings : sig
  type embeddings_input =
    { model : string
    ; input : string list
    }
  [@@deriving jsonaf, sexp, bin_io]

  (** Type definition for the response from the embeddings API. *)
  type response = { data : embedding list }

  (** Type definition for an individual embedding in the response. *)
  and embedding =
    { embedding : float list
    ; index : int
    }
  [@@deriving jsonaf, sexp, bin_io]

  (** [post_openai_embeddings ~input net] makes an HTTP POST request to the OpenAI API embeddings endpoint with the given [input] and [net].

      It returns the parsed response as a [response] record. *)
  val post_openai_embeddings : _ Eio.Net.t -> input:string list -> response
end

module Completions : sig
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
  type chat_completion = { choices : default_choice list }
  [@@deriving jsonaf, sexp, bin_io]

  type _ response_type =
    | Stream : (stream_choice -> unit) -> unit response_type
    | Default : default_choice response_type

  type model =
    | O3_Mini
    | Gpt4
    | Gpt4o
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
end

module Responses : sig
  module Input_message : sig
    type role =
      | User
      | Assistant
      | System
      | Developer
    [@@deriving jsonaf, sexp, bin_io]

    val role_to_string : role -> string
    val role_of_string : string -> role

    type text_input =
      { text : string
      ; _type : string
      }
    [@@deriving jsonaf, sexp, bin_io]

    type image_detail =
      | High
      | Low
      | Auto
    [@@deriving jsonaf, sexp, bin_io]

    type image_input =
      { image_url : string
      ; detail : string
      ; _type : string
      }
    [@@deriving jsonaf, sexp, bin_io]

    type content_item =
      | Text of text_input
      | Image of image_input
    [@@deriving jsonaf, sexp, bin_io]

    type content = content_item list [@@deriving jsonaf, sexp, bin_io]

    type t =
      { role : role
      ; content : content
      ; _type : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Output_message : sig
    type role = Assistant [@@deriving jsonaf, sexp, bin_io]

    type content =
      { annotations : string list
      ; text : string
      ; _type : string
      }
    [@@deriving jsonaf, sexp, bin_io]

    type t =
      { role : role
      ; id : string
      ; content : content list
      ; status : string
      ; _type : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Function_call : sig
    type t =
      { name : string
      ; arguments : string
      ; call_id : string
      ; _type : string
      ; id : string option
      ; status : string option
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Function_call_output : sig
    type t =
      { output : string
      ; call_id : string
      ; _type : string
      ; id : string option
      ; status : string option
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Reasoning : sig
    type summary =
      { text : string
      ; _type : string
      }
    [@@deriving jsonaf, sexp, bin_io]

    type t =
      { summary : summary list
      ; _type : string
      ; id : string
      ; status : string option
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Item : sig
    type t =
      | Input_message of Input_message.t
      | Output_message of Output_message.t
      | Function_call of Function_call.t
      | Function_call_output of Function_call_output.t
      | Reasoning of Reasoning.t
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Request : sig
    type model =
      | O3
      | Gpt4
      | Gpt4o
      | Gpt3
      | Gpt3_16k
    [@@deriving jsonaf, sexp, bin_io]

    val model_to_str : model -> string
    val model_of_str_exn : string -> model

    module Reasoning : sig
      module Effort : sig
        type t =
          | Low
          | Medium
          | High
        [@@deriving jsonaf, sexp, bin_io]

        val to_str : t -> string
        val of_str_exn : string -> t
      end

      module Summary : sig
        type t =
          | Auto
          | Consise
          | Detailed
        [@@deriving jsonaf, sexp, bin_io]

        val to_str : t -> string
        val of_str_exn : string -> t
      end

      type t =
        { effort : Effort.t option
        ; summary : Summary.t option
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Tool : sig
      type t =
        { name : string
        ; description : string option
        ; parameters : Jsonaf.t
        ; strict : bool
        ; type_ : string
        }
      [@@deriving jsonaf, sexp]
    end

    type t =
      { input : Item.t list
      ; model : model
      ; max_output_tokens : int option
      ; parallel_tool_calls : bool option
      ; reasoning : Reasoning.t option
      ; store : bool option
      ; stream : bool option
      ; temperature : float option
      ; tools : Tool.t list option
      ; top_p : float option
      }
    [@@deriving jsonaf, sexp]

    val create
      :  ?max_output_tokens:int
      -> ?parallel_tool_calls:bool
      -> ?store:bool
      -> ?stream:bool
      -> ?temperature:float
      -> ?top_p:float
      -> ?reasoning:Reasoning.t
      -> ?tools:Tool.t list
      -> model:model
      -> input:Item.t list
      -> unit
      -> t
  end

  module Status : sig
    type t =
      | Completed
      | In_progress
      | Failed
      | Incomplete
    [@@deriving jsonaf, sexp, bin_io]

    val to_str : t -> string
    val of_str_exn : string -> t
  end

  module Response : sig
    type t =
      { status : Status.t
      ; output : Item.t list
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Response_stream : sig
    module Item : sig
      type t =
        | Input_message of Input_message.t
        | Output_message of Output_message.t
        | Function_call of Function_call.t
        | Reasoning of Reasoning.t
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Output_item_added : sig
      type t =
        { item : Item.t
        ; output_index : int
        ; type_ : string
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Output_item_done : sig
      type t =
        { item : Item.t
        ; output_index : int
        ; type_ : string
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Output_text_delta : sig
      type t =
        { context_index : int
        ; delta : string
        ; item_id : string
        ; output_index : int
        ; type_ : string
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Function_call_arguments_delta : sig
      type t =
        { delta : string
        ; item_id : string
        ; output_index : int
        ; type_ : string
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Function_call_arguments_done : sig
      type t =
        { arguments : string
        ; item_id : string
        ; output_index : int
        ; type_ : string
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    type t =
      | Output_item_added of Output_item_added.t
      | Output_item_done of Output_item_done.t
      | Output_text_delta of Output_text_delta.t
      | Function_call_arguments_delta of Function_call_arguments_delta.t
      | Function_call_arguments_done of Function_call_arguments_done.t
    [@@deriving jsonaf, sexp, bin_io]
  end

  type _ response_type =
    | Stream : (Response_stream.t -> unit) -> unit response_type
    | Default : Response.t response_type

  val post_response
    :  'a response_type
    -> ?max_output_tokens:int
    -> ?temperature:float
    -> ?tools:Request.Tool.t list
    -> ?model:Request.model
    -> ?reasoning:Request.Reasoning.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> 'n Eio.Net.t
    -> inputs:Item.t list
    -> 'a
end
