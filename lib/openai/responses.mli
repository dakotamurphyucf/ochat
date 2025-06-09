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

module Annotation : sig
  module File_citation : sig
    type t =
      { title : string
      ; type_ : string
      ; start_index : int
      ; end_index : int
      ; file_id : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Url_citation : sig
    type t =
      { type_ : string
      ; index : int
      ; url : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  type t =
    | File_citation of File_citation.t
    | Url_citation of Url_citation.t
  [@@deriving jsonaf, sexp, bin_io]
end

module Annotation_added : sig
  type t =
    { type_ : string
    ; annotation : Annotation.t
    ; content_index : int
    ; item_id : string
    ; output_index : int
    ; annotation_index : int
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Output_message : sig
  type role = Assistant [@@deriving jsonaf, sexp, bin_io]

  type content =
    { annotations : Annotation.t list
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

module Web_search_call : sig
  type t =
    { _type : string [@key "type"]
    ; id : string
    ; status : string
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module File_search_call : sig
  module Result : sig
    module Attributes : sig
      type t = (string * string) list [@@deriving jsonaf, sexp, bin_io]
    end

    type t =
      { attributes : Attributes.t
      ; file_id : string
      ; filename : string
      ; score : int
      ; text : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  type t =
    { _type : string [@key "type"]
    ; id : string
    ; status : string
    ; queries : string list
    ; results : Result.t list option
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
    | Web_search_call of Web_search_call.t
    | File_search_call of File_search_call.t
    | Reasoning of Reasoning.t
  [@@deriving jsonaf, sexp, bin_io]
end

module Request : sig
  type model =
    | O3
    | O3_mini
    | Gpt4
    | O4_mini
    | Gpt4o
    | Gpt4_1
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
    module File_search : sig
      module Filter : sig
        module Value : sig
          type t =
            | String of string
            | Number of float
            | Boolean of bool
          [@@deriving jsonaf, sexp, bin_io]
        end

        module Comparison : sig
          module Type : sig
            type t =
              | Eq
              | Ne
              | Gt
              | Gte
              | Lt
              | Lte
            [@@deriving jsonaf, sexp, bin_io]
          end

          type t =
            { key : string
            ; type_ : Type.t
            ; value : Value.t
            }
          [@@deriving jsonaf, sexp, bin_io]
        end

        module Compound : sig
          module Type : sig
            type t =
              | And
              | Or
            [@@deriving jsonaf, sexp, bin_io]
          end

          type filters =
            | Comparison of Comparison.t
            | Compound of t
          [@@deriving jsonaf, sexp, bin_io]

          and t =
            { type_ : Type.t
            ; filters : filters list
            }
          [@@deriving jsonaf, sexp, bin_io]
        end

        type t =
          | Comparison of Comparison.t
          | Compound of Compound.t
        [@@deriving jsonaf, sexp, bin_io]
      end

      module Ranking_options : sig
        type t =
          { ranker : string option
          ; score_threshold : float option
          }
        [@@deriving jsonaf, sexp, bin_io]
      end

      type t =
        { type_ : string
        ; vector_store_ids : string list
        ; filters : Filter.t list option
        ; max_num_results : int option
        ; ranking_options : Ranking_options.t option
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Web_search : sig
      module User_location : sig
        type t =
          { type_ : string
          ; city : string option
          ; country : string option
          ; region : string option
          ; timezone : string option
          }
        [@@deriving jsonaf, sexp, bin_io]
      end

      type t =
        { type_ : string
        ; search_context_size : string option
        ; user_location : User_location.t option
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Function : sig
      type t =
        { name : string
        ; description : string option
        ; parameters : Jsonaf.t
        ; strict : bool
        ; type_ : string
        }
      [@@deriving jsonaf, bin_io, sexp]
    end

    type t =
      | File_search of File_search.t
      | Web_search of Web_search.t
      | Function of Function.t
    [@@deriving jsonaf, bin_io, sexp]
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

module Error : sig
  type t =
    { code : string option
    ; message : string
    ; param : string option
    ; type_ : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Incomplete_details : sig
  type t =
    { reason : string option
    ; model_output_start : int option
    ; tokens : int option
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Text_cfg : sig
  module Format : sig
    module Text : sig
      type t = { type_ : string } [@@deriving jsonaf, sexp, bin_io]
    end

    module Json_schema : sig
      type t =
        { type_ : string
        ; name : string
        ; schema : Jsonaf.t
        ; description : string
        ; strict : bool option
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Json_Object : sig
      type t = { type_ : string } [@@deriving jsonaf, sexp, bin_io]
    end

    type t =
      | Text of Text.t
      | Json_schema of Json_schema.t
      | Json_Object of Json_Object.t
    [@@deriving jsonaf, sexp, bin_io]
  end

  type t = { format : Format.t } [@@deriving jsonaf, sexp, bin_io]
end

module Tool_choice : sig
  module Hosted_tool : sig
    type t = { type_ : string } [@@deriving jsonaf, sexp, bin_io]
  end

  module Function_tool : sig
    type t =
      { name : string
      ; type_ : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  type t =
    | Mode of string
    | Hosted of Hosted_tool.t
    | Function of Function_tool.t
  [@@deriving jsonaf, sexp, bin_io]
end

module Usage : sig
  type t =
    { input_tokens : int
    ; input_tokens_details : Jsonaf.t
    ; output_tokens : int
    ; output_tokens_details : Jsonaf.t option
    ; total_tokens : int option
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Metadata : sig
  type t = (string * string) list [@@deriving jsonaf, sexp, bin_io]
end

module Response : sig
  type t =
    { id : string
    ; object_ : string
    ; created_at : int
    ; status : Status.t
    ; error : Error.t option
    ; incomplete_details : Incomplete_details.t option
    ; instructions : string option
    ; max_output_tokens : int option
    ; model : string
    ; output : Item.t list
    ; parallel_tool_calls : bool option
    ; previous_response_id : string option
    ; reasoning : Request.Reasoning.t option
    ; store : bool option
    ; temperature : float option
    ; text : Text_cfg.t option
    ; tool_choice : Tool_choice.t option
    ; tools : Request.Tool.t list option
    ; top_p : float option
    ; truncation : string option
    ; usage : Usage.t option
    ; user : string option
    ; metadata : Metadata.t option
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
      { content_index : int
      ; delta : string
      ; item_id : string
      ; output_index : int
      ; type_ : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Output_text_done : sig
    type t =
      { content_index : int
      ; text : string
      ; item_id : string
      ; output_index : int
      ; type_ : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Reasoning_summary_text_delta : sig
    type t =
      { summary_index : int
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

  module Response_created : sig
    type t =
      { type_ : string
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Response_in_progress : sig
    type t =
      { type_ : string
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Response_completed : sig
    type t =
      { type_ : string
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Response_incomplete : sig
    type t =
      { type_ : string
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Response_failed : sig
    type t =
      { type_ : string
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Part : sig
    module Output_text : sig
      type t =
        { type_ : string
        ; text : string
        ; annotations : Annotation.t list
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    module Refusal : sig
      type t =
        { type_ : string
        ; refusal : string
        }
      [@@deriving jsonaf, sexp, bin_io]
    end

    type t =
      | Output_text of Output_text.t
      | Refusal of Refusal.t
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Content_part_added : sig
    type t =
      { type_ : string
      ; content_index : int
      ; item_id : string
      ; output_index : int
      ; part : Part.t
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Content_part_done : sig
    type t =
      { type_ : string
      ; content_index : int
      ; item_id : string
      ; output_index : int
      ; part : Part.t
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Response_refusal_delta : sig
    type t =
      { content_index : int
      ; delta : string
      ; item_id : string
      ; output_index : int
      ; type_ : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Response_refusal_done : sig
    type t =
      { content_index : int
      ; refusal : string
      ; item_id : string
      ; output_index : int
      ; type_ : string
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module File_search_call_in_progress : sig
    type t =
      { type_ : string
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module File_search_call_searching : sig
    type t =
      { type_ : string
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module File_search_call_completed : sig
    type t =
      { type_ : string
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Web_search_call_in_progress : sig
    type t =
      { type_ : string
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Web_search_call_searching : sig
    type t =
      { type_ : string
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Web_search_call_completed : sig
    type t =
      { type_ : string
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io]
  end

  module Unknown : sig
    type t = Jsonaf.t [@@deriving sexp, bin_io]
  end

  type t =
    | Output_item_added of Output_item_added.t
    | Output_item_done of Output_item_done.t
    | Output_text_delta of Output_text_delta.t
    | Output_text_done of Output_text_done.t
    | Function_call_arguments_delta of Function_call_arguments_delta.t
    | Function_call_arguments_done of Function_call_arguments_done.t
    | Response_created of Response_created.t
    | Response_in_progress of Response_in_progress.t
    | Reasoning_summary_text_delta of Reasoning_summary_text_delta.t
    | Response_completed of Response_completed.t
    | Response_incomplete of Response_incomplete.t
    | Response_failed of Response_failed.t
    | Content_part_added of Content_part_added.t
    | Content_part_done of Content_part_done.t
    | Response_refusal_delta of Response_refusal_delta.t
    | Response_refusal_done of Response_refusal_done.t
    | Annotation_added of Annotation_added.t
    | File_search_call_in_progress of File_search_call_in_progress.t
    | File_search_call_searching of File_search_call_searching.t
    | File_search_call_completed of File_search_call_completed.t
    | Web_search_call_in_progress of Web_search_call_in_progress.t
    | Web_search_call_searching of Web_search_call_searching.t
    | Web_search_call_completed of Web_search_call_completed.t
    | Error of Error.t
    | Unknown of Unknown.t
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
