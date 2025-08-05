open Core
open Eio
open Io.Net
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

(** [api_key] is the API key for the OpenAI API. *)
let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:""

module Input_message = struct
  type role =
    | User [@name "user"]
    | Assistant [@name "assistant"]
    | System [@name "system"]
    | Developer [@name "developer"]
  [@@deriving sexp, bin_io]

  let jsonaf_of_role = function
    | User -> `String "user"
    | Assistant -> `String "assistant"
    | System -> `String "system"
    | Developer -> `String "developer"
  ;;

  let role_of_jsonaf = function
    | `String "user" -> User
    | `String "assistant" -> Assistant
    | `String "system" -> System
    | `String "developer" -> Developer
    | _ -> failwith "Invalid role"
  ;;

  let role_to_string = function
    | User -> "user"
    | Assistant -> "assistant"
    | System -> "system"
    | Developer -> "developer"
  ;;

  let role_of_string = function
    | "user" -> User
    | "assistant" -> Assistant
    | "system" -> System
    | "developer" -> Developer
    | _ -> failwith "Invalid role"
  ;;

  type text_input =
    { text : string
    ; _type : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io]

  type image_detail =
    | High [@name "high"]
    | Low [@name "low"]
    | Auto [@name "auto"]
  [@@deriving sexp, bin_io]

  let jsonaf_of_image_detail = function
    | High -> `String "high"
    | Low -> `String "low"
    | Auto -> `String "auto"
  ;;

  let image_detail_of_jsonaf = function
    | `String "high" -> High
    | `String "low" -> Low
    | `String "auto" -> Auto
    | _ -> failwith "Invalid image detail"
  ;;

  type image_input =
    { image_url : string
    ; detail : string
    ; _type : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  type content_item =
    | Text of text_input
    | Image of image_input
  [@@deriving sexp, bin_io]

  let jsonaf_of_content_item = function
    | Text text_input -> jsonaf_of_text_input text_input
    | Image image_input -> jsonaf_of_image_input image_input
  ;;

  let content_item_of_jsonaf json =
    match (json : Jsonaf.t) with
    | `Object obj ->
      (match Jsonaf.member "type" (`Object obj) with
       | Some (`String "input_text") -> Text (text_input_of_jsonaf (`Object obj))
       | Some (`String "input_image") -> Image (image_input_of_jsonaf (`Object obj))
       | _ -> failwith "Invalid content type")
    | `String str -> Text { text = str; _type = "input_text" }
    | _ -> failwith "Invalid content format"
  ;;

  type content = content_item list [@@deriving jsonaf_of, sexp, bin_io]

  let content_of_jsonaf = function
    | `Array arr -> List.map ~f:content_item_of_jsonaf arr
    | `String str -> [ Text { text = str; _type = "input_text" } ]
    | _ -> failwith "Invalid content format"
  ;;

  type t =
    { role : role
    ; content : content
    ; _type : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Annotation = struct
  module File_citation = struct
    type t =
      { title : string
      ; type_ : string [@key "type"]
      ; start_index : int
      ; end_index : int
      ; file_id : string
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Url_citation = struct
    type t =
      { type_ : string [@key "type"]
      ; index : int
      ; url : string
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  type t =
    | File_citation of File_citation.t
    | Url_citation of Url_citation.t
  [@@deriving sexp, bin_io]

  let jsonaf_of_t = function
    | File_citation file_citation -> File_citation.jsonaf_of_t file_citation
    | Url_citation url_citation -> Url_citation.jsonaf_of_t url_citation
  ;;

  let t_of_jsonaf json =
    match json with
    | `Object obj ->
      (match Jsonaf.member "type" (`Object obj) with
       | Some (`String "file_citation") ->
         File_citation (File_citation.t_of_jsonaf (`Object obj))
       | Some (`String "url_citation") ->
         Url_citation (Url_citation.t_of_jsonaf (`Object obj))
       | _ -> failwith "Invalid annotation type")
    | _ -> failwith "Invalid annotation format"
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  ;;
end

module Annotation_added = struct
  type t =
    { type_ : string [@key "type"]
    ; annotation : Annotation.t
    ; content_index : int
    ; item_id : string
    ; output_index : int
    ; annotation_index : int
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

module Output_message = struct
  type role = Assistant [@name "assistant"] [@@deriving sexp, bin_io]

  let jsonaf_of_role = function
    | Assistant -> `String "assistant"
  ;;

  let role_of_jsonaf = function
    | `String "assistant" -> Assistant
    | _ -> failwith "Invalid role"
  ;;

  (* annotations is usually empty which works for our purposes but if provided will fail to parse because it is not array of strings *)
  type content =
    { annotations : Annotation.t list
    ; text : string
    ; _type : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]

  type t =
    { role : role
    ; id : string
    ; content : content list
    ; status : string
    ; _type : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Function_call = struct
  type t =
    { name : string
    ; arguments : string
    ; call_id : string
    ; _type : string [@key "type"]
    ; id : string option [@jsonaf.option]
    ; status : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Function_call_output = struct
  type t =
    { output : string
    ; call_id : string
    ; _type : string [@key "type"]
    ; id : string option [@jsonaf.option]
    ; status : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Web_search_call = struct
  type t =
    { _type : string [@key "type"]
    ; id : string
    ; status : string
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module File_search_call = struct
  module Result = struct
    module Attributes = struct
      type t = (string * string) list [@@deriving sexp, bin_io]

      let t_of_jsonaf = function
        | `Object obj ->
          List.map obj ~f:(fun (k, v) ->
            match v with
            | `String s -> k, s
            | _ -> failwith "attributes expects string values")
        | `Null -> []
        | _ -> failwith "attributes_of_jsonaf"
      ;;

      let jsonaf_of_t lst = `Object (List.map lst ~f:(fun (k, v) -> k, `String v))
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

module Reasoning = struct
  type summary =
    { text : string
    ; _type : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io]

  type t =
    { summary : summary list
    ; _type : string [@key "type"]
    ; id : string
    ; status : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, bin_io]
end

module Item = struct
  type t =
    | Input_message of Input_message.t
    | Output_message of Output_message.t
    | Function_call of Function_call.t
    | Function_call_output of Function_call_output.t
    | Web_search_call of Web_search_call.t
    | File_search_call of File_search_call.t
    | Reasoning of Reasoning.t
  [@@deriving sexp, bin_io]

  let jsonaf_of_t = function
    | Input_message input_message -> Input_message.jsonaf_of_t input_message
    | Output_message output_message -> Output_message.jsonaf_of_t output_message
    | Function_call function_too_call -> Function_call.jsonaf_of_t function_too_call
    | Function_call_output function_too_call_output ->
      Function_call_output.jsonaf_of_t function_too_call_output
    | Web_search_call web_search_call -> Web_search_call.jsonaf_of_t web_search_call
    | File_search_call file_search_call -> File_search_call.jsonaf_of_t file_search_call
    | Reasoning reasoning -> Reasoning.jsonaf_of_t reasoning
  ;;

  let t_of_jsonaf json =
    match json with
    | `Object obj ->
      (match Jsonaf.member "type" (`Object obj) with
       | Some (`String "message") ->
         (match Jsonaf.member "role" (`Object obj) with
          | Some (`String "assistant") ->
            Output_message (Output_message.t_of_jsonaf (`Object obj))
          | _ -> Input_message (Input_message.t_of_jsonaf (`Object obj)))
       | Some (`String "function_call") ->
         Function_call (Function_call.t_of_jsonaf (`Object obj))
       | Some (`String "function_call_output") ->
         Function_call_output (Function_call_output.t_of_jsonaf (`Object obj))
       | Some (`String "web_search_call") ->
         Web_search_call (Web_search_call.t_of_jsonaf (`Object obj))
       | Some (`String "file_search_call") ->
         File_search_call (File_search_call.t_of_jsonaf (`Object obj))
       | Some (`String "reasoning") -> Reasoning (Reasoning.t_of_jsonaf (`Object obj))
       | _ -> failwith "Invalid content type")
    | _ -> failwith "Invalid content format"
  ;;
end

(*
   gpt-4.1
o4-mini
o3-mini

gpt-4o
*)
module Request = struct
  type model =
    | O3 [@name "o3"]
    | O3_mini [@name "o3-mini"]
    | Gpt4 [@name "gpt-4.5-preview"]
    | O4_mini [@name "o4-mini"]
    | Gpt_4_1_mini [@name "gpt-4.1-nano"]
    | Gpt4o [@name "gpt-4o"]
    | Gpt4_1 [@name "gpt-4.1"]
    | Gpt3 [@name "gpt-3.5-turbo"]
    | Gpt3_16k [@name "gpt-3.5-turbo-16k"]
  [@@deriving sexp, bin_io]

  let jsonaf_of_model = function
    | O3 -> `String "o3"
    | O3_mini -> `String "o3-mini"
    | O4_mini -> `String "o4-mini"
    | Gpt_4_1_mini -> `String "gpt-4.1-nano"
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
    | `String "gpt-4.1-nano" -> Gpt_4_1_mini
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
    | Gpt_4_1_mini -> "gpt-4.1-nano"
    | Gpt3 -> "gpt-3.5-turbo"
    | Gpt3_16k -> "gpt-3.5-turbo-16k"
    | Gpt4 -> "gpt-4.5-preview"
  ;;

  let model_of_str_exn = function
    | "o3" -> O3
    | "o3-mini" -> O3_mini
    | "o4-mini" -> O4_mini
    | "gpt-4.1" -> Gpt4_1
    | "gpt-4.1-nano" -> Gpt_4_1_mini
    | "gpt-3.5-turbo" -> Gpt3
    | "gpt-3.5-turbo-16k" -> Gpt3_16k
    | "gpt-4.5" -> Gpt4
    | "gpt-4o" -> Gpt4o
    | _ -> failwith "Invalid model"
  ;;

  module Reasoning = struct
    module Effort = struct
      type t =
        | Low [@name "low"]
        | Medium [@name "medium"]
        | High [@name "high"]
      [@@deriving sexp, bin_io]

      let jsonaf_of_t = function
        | Low -> `String "low"
        | Medium -> `String "medium"
        | High -> `String "high"
      ;;

      let t_of_jsonaf = function
        | `String "low" -> Low
        | `String "medium" -> Medium
        | `String "high" -> High
        | _ -> failwith "Invalid effort"
      ;;

      let to_str = function
        | Low -> "low"
        | Medium -> "medium"
        | High -> "high"
      ;;

      let of_str_exn = function
        | "low" -> Low
        | "medium" -> Medium
        | "high" -> High
        | _ -> failwith "Invalid effort"
      ;;
    end

    module Summary = struct
      type t =
        | Auto [@name "auto"]
        | Consise [@name "consise"]
        | Detailed [@name "detailed"]
      [@@deriving sexp, bin_io]

      let jsonaf_of_t = function
        | Auto -> `String "auto"
        | Consise -> `String "consise"
        | Detailed -> `String "detailed"
      ;;

      let t_of_jsonaf = function
        | `String "auto" -> Auto
        | `String "consise" -> Consise
        | `String "detailed" -> Detailed
        | _ -> failwith "Invalid summary"
      ;;

      let to_str = function
        | Auto -> "auto"
        | Consise -> "consise"
        | Detailed -> "detailed"
      ;;

      let of_str_exn = function
        | "auto" -> Auto
        | "consise" -> Consise
        | "detailed" -> Detailed
        | _ -> failwith "Invalid summary"
      ;;
    end

    type t =
      { effort : Effort.t option
      ; summary : Summary.t option
      }
    [@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io]
  end

  module Tool = struct
    module File_search = struct
      module Filter = struct
        module Value = struct
          type t =
            | String of string
            | Number of float
            | Boolean of bool
          [@@deriving sexp, bin_io]

          let jsonaf_of_t = function
            | String s -> `String s
            | Number n -> `Number (Float.to_string n)
            | Boolean b -> if b then `True else `False
          ;;

          let t_of_jsonaf = function
            | `String s -> String s
            | `Number n -> Number (Float.of_string n)
            | `False -> Boolean false
            | `True -> Boolean true
            | _ -> failwith "Invalid value type"
          ;;
        end

        module Comparison = struct
          module Type = struct
            type t =
              | Eq
              | Ne
              | Gt
              | Gte
              | Lt
              | Lte
            [@@deriving sexp, bin_io]

            let jsonaf_of_t = function
              | Eq -> `String "eq"
              | Ne -> `String "ne"
              | Gt -> `String "gt"
              | Gte -> `String "gte"
              | Lt -> `String "lt"
              | Lte -> `String "lte"
            ;;

            let t_of_jsonaf = function
              | `String "eq" -> Eq
              | `String "ne" -> Ne
              | `String "gt" -> Gt
              | `String "gte" -> Gte
              | `String "lt" -> Lt
              | `String "lte" -> Lte
              | _ -> failwith "Invalid comparison type"
            ;;
          end

          type t =
            { key : string
            ; type_ : Type.t [@key "type"]
            ; value : Value.t
            }
          [@@jsonaf.allow_extra_fields] [@@deriving jsonaf, bin_io, sexp]
        end

        module Compound = struct
          module Type = struct
            type t =
              | And
              | Or
            [@@deriving sexp, bin_io]

            let jsonaf_of_t = function
              | And -> `String "and"
              | Or -> `String "or"
            ;;

            let t_of_jsonaf = function
              | `String "and" -> And
              | `String "or" -> Or
              | _ -> failwith "Invalid comparison type"
            ;;
          end

          type filters =
            | Comparison of Comparison.t
            | Compound of t
          [@@deriving sexp, bin_io]

          and t =
            { type_ : Type.t
            ; filters : filters list
            }
          [@@deriving bin_io, sexp]

          let rec jsonaf_of_filters = function
            | Comparison comparison -> Comparison.jsonaf_of_t comparison
            | Compound compound -> jsonaf_of_t compound

          and filters_of_jsonaf = function
            | `Object obj ->
              (match Jsonaf.member "type" (`Object obj) with
               | Some (`String "eq")
               | Some (`String "ne")
               | Some (`String "gt")
               | Some (`String "gte")
               | Some (`String "lt")
               | Some (`String "lte") -> Comparison (Comparison.t_of_jsonaf (`Object obj))
               | Some (`String "or") | Some (`String "and") ->
                 Compound (t_of_jsonaf (`Object obj))
               | _ -> failwith "Invalid filter type")
            | _ -> failwith "Invalid filter format"

          and jsonaf_of_t t =
            let filters = List.map ~f:jsonaf_of_filters t.filters in
            `Object (("type", Type.jsonaf_of_t t.type_) :: [ "filters", `Array filters ])

          and t_of_jsonaf json =
            match json with
            | `Object obj ->
              (match Jsonaf.member "type" (`Object obj) with
               | Some (`String "and") | Some (`String "or") ->
                 let type_ = Type.t_of_jsonaf (Jsonaf.member_exn "type" (`Object obj)) in
                 let filters = Jsonaf.member_exn "filters" (`Object obj) in
                 (match filters with
                  | `Array arr ->
                    let filters = List.map ~f:filters_of_jsonaf arr in
                    { type_; filters }
                  | _ -> failwith "Invalid filter type")
               | _ -> failwith "Invalid filter type")
            | _ -> failwith "Invalid filter format"
          ;;
        end

        type t =
          | Comparison of Comparison.t
          | Compound of Compound.t
        [@@deriving bin_io, sexp]

        let jsonaf_of_t = function
          | Comparison comparison -> Comparison.jsonaf_of_t comparison
          | Compound compound -> Compound.jsonaf_of_t compound
        ;;

        let t_of_jsonaf = function
          | `Object obj ->
            (match Jsonaf.member "type" (`Object obj) with
             | Some (`String "eq")
             | Some (`String "ne")
             | Some (`String "gt")
             | Some (`String "gte")
             | Some (`String "lt")
             | Some (`String "lte") -> Comparison (Comparison.t_of_jsonaf (`Object obj))
             | Some (`String "or") | Some (`String "and") ->
               Compound (Compound.t_of_jsonaf (`Object obj))
             | _ -> failwith "Invalid filter type")
          | _ -> failwith "Invalid filter format"
        ;;
      end

      module Ranking_options = struct
        type t =
          { ranker : string option [@jsonaf.option]
          ; score_threshold : float option [@jsonaf.option]
          }
        [@@deriving jsonaf, sexp, bin_io]
      end

      type t =
        { type_ : string [@key "type"]
        ; vector_store_ids : string list
        ; filters : Filter.t list option [@jsonaf.option]
        ; max_num_results : int option [@jsonaf.option]
        ; ranking_options : Ranking_options.t option [@jsonaf.option]
        }
      [@@jsonaf.allow_extra_fields] [@@deriving jsonaf, bin_io, sexp]
    end

    module Web_search = struct
      module User_location = struct
        type t =
          { type_ : string [@key "type"]
          ; city : string option [@jsonaf.option]
          ; country : string option [@jsonaf.option]
          ; region : string option [@jsonaf.option]
          ; timezone : string option [@jsonaf.option]
          }
        [@@deriving jsonaf, sexp, bin_io]
      end

      type t =
        { type_ : string [@key "type"]
        ; search_context_size : string option [@jsonaf.option]
        ; user_location : User_location.t option
        }
      [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
    end
    (* need to update to add file search, computer use, web search *)

    module Function = struct
      type t =
        { name : string [@default "function"]
        ; description : string option
        ; parameters : Jsonaf.t [@default `Object []]
        ; strict : bool
        ; type_ : string [@key "type"]
        }
      [@@jsonaf.allow_extra_fields] [@@deriving jsonaf, bin_io, sexp]
    end

    type t =
      | File_search of File_search.t
      | Web_search of Web_search.t
      | Function of Function.t
    [@@deriving sexp, bin_io]

    let jsonaf_of_t = function
      | File_search file_search -> File_search.jsonaf_of_t file_search
      | Web_search web_search -> Web_search.jsonaf_of_t web_search
      | Function function_ -> Function.jsonaf_of_t function_
    ;;

    let t_of_jsonaf = function
      | `Object obj ->
        (match Jsonaf.member "type" (`Object obj) with
         | Some (`String "file_search") ->
           File_search (File_search.t_of_jsonaf (`Object obj))
         | Some (`String "web_search_preview")
         | Some (`String "web_search_preview_2025_03_11") ->
           Web_search (Web_search.t_of_jsonaf (`Object obj))
         | Some (`String "function") -> Function (Function.t_of_jsonaf (`Object obj))
         | _ -> failwith "Invalid tool type")
      | _ -> failwith "Invalid tool format"
    ;;
  end

  type t =
    { input : Item.t list
    ; model : model
    ; max_output_tokens : int option [@jsonaf.option]
    ; parallel_tool_calls : bool option [@jsonaf.option]
    ; reasoning : Reasoning.t option [@jsonaf.option]
    ; store : bool option [@jsonaf.option]
    ; stream : bool option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    ; tools : Tool.t list option [@jsonaf.option]
    ; top_p : float option [@jsonaf.option]
    }
  [@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp]

  let create
        ?(max_output_tokens = 600)
        ?(parallel_tool_calls = false)
        ?(store = true)
        ?(stream = false)
        ?temperature
        ?top_p
        ?reasoning
        ?tools
        ~model
        ~input
        ()
    =
    { input
    ; model
    ; max_output_tokens = Some max_output_tokens
    ; parallel_tool_calls = Some parallel_tool_calls
    ; store = Some store
    ; stream = Some stream
    ; temperature
    ; top_p
    ; reasoning
    ; tools
    }
  ;;
end

module Status = struct
  type t =
    | Completed [@name "completed"]
    | In_progress [@name "in_progress"]
    | Failed [@name "failed"]
    | Incomplete [@name "incomplete"]
  [@@deriving sexp, bin_io]

  let jsonaf_of_t = function
    | Completed -> `String "completed"
    | In_progress -> `String "in_progress"
    | Failed -> `String "failed"
    | Incomplete -> `String "incomplete"
  ;;

  let t_of_jsonaf = function
    | `String "completed" -> Completed
    | `String "in_progress" -> In_progress
    | `String "failed" -> Failed
    | `String "incomplete" -> Incomplete
    | _ -> failwith "Invalid status"
  ;;

  let to_str = function
    | Completed -> "completed"
    | In_progress -> "in_progress"
    | Failed -> "failed"
    | Incomplete -> "incomplete"
  ;;

  let of_str_exn = function
    | "completed" -> Completed
    | "in_progress" -> In_progress
    | "failed" -> Failed
    | "incomplete" -> Incomplete
    | _ -> failwith "Invalid status"
  ;;
end

(* ❶ Error object *)
module Error = struct
  type t =
    { code : string option
    ; message : string
    ; param : string option
    ; type_ : string [@key "type"]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(* ❷ “incomplete_details” object *)
module Incomplete_details = struct
  type t =
    { reason : string option
    ; model_output_start : int option
    ; tokens : int option
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(* ❸ “text” configuration *)
module Text_cfg = struct
  module Format = struct
    module Text = struct
      type t = { type_ : string [@key "type"] (* text | json_schema | json_object *) }
      [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
    end

    module Json_schema = struct
      type t =
        { type_ : string [@key "type"] (* text | json_schema | json_object *)
        ; name : string
        ; schema : Jsonaf.t
        ; description : string
        ; strict : bool option [@jsonaf.option]
        }
      [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
    end

    module Json_Object = struct
      type t = { type_ : string [@key "type"] (* text | json_schema | json_object *) }
      [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
    end

    type t =
      | Text of Text.t
      | Json_schema of Json_schema.t
      | Json_Object of Json_Object.t
    [@@deriving sexp, bin_io]

    let jsonaf_of_t = function
      | Text t -> Text.jsonaf_of_t t
      | Json_schema t -> Json_schema.jsonaf_of_t t
      | Json_Object t -> Json_Object.jsonaf_of_t t
    ;;

    let t_of_jsonaf = function
      | `Object obj ->
        (match Jsonaf.member "type" (`Object obj) with
         | Some (`String "text") -> Text (Text.t_of_jsonaf (`Object obj))
         | Some (`String "json_schema") ->
           Json_schema (Json_schema.t_of_jsonaf (`Object obj))
         | Some (`String "json_object") ->
           Json_Object (Json_Object.t_of_jsonaf (`Object obj))
         | _ -> failwith "Invalid format type")
      | _ -> failwith "Invalid format"
    ;;
  end

  type t = { format : Format.t }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(* ❹ “tool_choice” – can be a simple mode or a full object            *)
module Tool_choice = struct
  module Hosted_tool = struct
    type t = { type_ : string [@key "type"] }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Function_tool = struct
    type t =
      { name : string
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  type t =
    | Mode of string (* "none" | "auto" | "required" *)
    | Hosted of Hosted_tool.t (* e.g. { "type":"file_search" } *)
    | Function of Function_tool.t (* function forcing              *)
  [@@deriving sexp, bin_io]

  let t_of_jsonaf = function
    | `String s -> Mode s
    | `Object _ as obj ->
      (match Jsonaf.member "type" obj with
       | Some (`String "function") -> Function (Function_tool.t_of_jsonaf obj)
       | _ -> Hosted (Hosted_tool.t_of_jsonaf obj))
    | _ -> failwith "tool_choice_of_jsonaf: unexpected JSON"
  ;;

  let jsonaf_of_t = function
    | Mode s -> `String s
    | Hosted h -> Hosted_tool.jsonaf_of_t h
    | Function f -> Function_tool.jsonaf_of_t f
  ;;
end

(* ❺ Usage block *)
module Usage = struct
  type t =
    { input_tokens : int
    ; input_tokens_details : Jsonaf.t
    ; output_tokens : int
    ; output_tokens_details : Jsonaf.t option [@jsonaf.option]
    ; total_tokens : int option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

(* ❻ User-metadata: stored as an assoc-list (string,string) *)
module Metadata = struct
  type t = (string * string) list [@@deriving sexp, bin_io]

  let t_of_jsonaf = function
    | `Object obj ->
      List.map obj ~f:(fun (k, v) ->
        match v with
        | `String s -> k, s
        | _ -> failwith "metadata expects string values")
    | `Null -> []
    | _ -> failwith "metadata_of_jsonaf"
  ;;

  let jsonaf_of_t lst = `Object (List.map lst ~f:(fun (k, v) -> k, `String v))
end

(* ─────────────── 1.  Updated response object ─────────────────────── *)
module Response = struct
  type t =
    { id : string
    ; object_ : string [@key "object"]
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
    ; truncation : string option [@jsonaf.option]
    ; usage : Usage.t option
    ; user : string option
    ; metadata : Metadata.t option
    }
  [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
end

module Response_stream = struct
  module Item = struct
    type t =
      | Input_message of Input_message.t
      | Output_message of Output_message.t
      | Function_call of Function_call.t
      | Reasoning of Reasoning.t
    [@@deriving sexp, bin_io]

    let jsonaf_of_t = function
      | Input_message input_message -> Input_message.jsonaf_of_t input_message
      | Output_message output_message -> Output_message.jsonaf_of_t output_message
      | Function_call function_too_call -> Function_call.jsonaf_of_t function_too_call
      | Reasoning reasoning -> Reasoning.jsonaf_of_t reasoning
    ;;

    let t_of_jsonaf json =
      match json with
      | `Object obj ->
        (match Jsonaf.member "type" (`Object obj) with
         | Some (`String "message") ->
           (match Jsonaf.member "role" (`Object obj) with
            | Some (`String "assistant") ->
              Output_message (Output_message.t_of_jsonaf (`Object obj))
            | _ -> Input_message (Input_message.t_of_jsonaf (`Object obj)))
         | Some (`String "function_call") ->
           Function_call (Function_call.t_of_jsonaf (`Object obj))
         | Some (`String "reasoning") -> Reasoning (Reasoning.t_of_jsonaf (`Object obj))
         | _ -> failwith "Invalid content type")
      | _ -> failwith "Invalid content format"
    ;;
  end

  module Output_item_added = struct
    type t =
      { item : Item.t
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Output_item_done = struct
    type t =
      { item : Item.t
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Output_text_delta = struct
    type t =
      { content_index : int
      ; delta : string
      ; item_id : string
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Output_text_done = struct
    type t =
      { content_index : int
      ; text : string
      ; item_id : string
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Reasoning_summary_text_delta = struct
    type t =
      { summary_index : int
      ; delta : string
      ; item_id : string
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Function_call_arguments_delta = struct
    type t =
      { delta : string
      ; item_id : string
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Function_call_arguments_done = struct
    type t =
      { arguments : string
      ; item_id : string
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Response_created = struct
    type t =
      { type_ : string [@key "type"]
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Response_in_progress = struct
    type t =
      { type_ : string [@key "type"]
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Response_completed = struct
    type t =
      { type_ : string [@key "type"]
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Response_incomplete = struct
    type t =
      { type_ : string [@key "type"]
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Response_failed = struct
    type t =
      { type_ : string [@key "type"]
      ; response : Response.t
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Part = struct
    module Output_text = struct
      type t =
        { type_ : string [@key "type"]
        ; text : string
        ; annotations : Annotation.t list
        }
      [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
    end

    module Refusal = struct
      type t =
        { type_ : string [@key "type"]
        ; refusal : string
        }
      [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
    end

    type t =
      | Output_text of Output_text.t
      | Refusal of Refusal.t
    [@@deriving sexp, bin_io]

    let jsonaf_of_t = function
      | Output_text output_text -> Output_text.jsonaf_of_t output_text
      | Refusal refusal -> Refusal.jsonaf_of_t refusal
    ;;

    let t_of_jsonaf json =
      match json with
      | `Object obj ->
        (match Jsonaf.member "type" (`Object obj) with
         | Some (`String "output_text") ->
           Output_text (Output_text.t_of_jsonaf (`Object obj))
         | Some (`String "refusal") -> Refusal (Refusal.t_of_jsonaf (`Object obj))
         | _ -> failwith "Invalid content type")
      | _ -> failwith "Invalid content format"
    ;;
  end

  module Content_part_added = struct
    type t =
      { type_ : string [@key "type"]
      ; content_index : int
      ; item_id : string
      ; output_index : int
      ; part : Part.t
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Content_part_done = struct
    type t =
      { type_ : string [@key "type"]
      ; content_index : int
      ; item_id : string
      ; output_index : int
      ; part : Part.t
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Response_refusal_delta = struct
    type t =
      { content_index : int
      ; delta : string
      ; item_id : string
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Response_refusal_done = struct
    type t =
      { content_index : int
      ; refusal : string
      ; item_id : string
      ; output_index : int
      ; type_ : string [@key "type"]
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module File_search_call_in_progress = struct
    type t =
      { type_ : string [@key "type"]
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module File_search_call_searching = struct
    type t =
      { type_ : string [@key "type"]
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module File_search_call_completed = struct
    type t =
      { type_ : string [@key "type"]
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Web_search_call_in_progress = struct
    type t =
      { type_ : string [@key "type"]
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Web_search_call_searching = struct
    type t =
      { type_ : string [@key "type"]
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  module Web_search_call_completed = struct
    type t =
      { type_ : string [@key "type"]
      ; item_id : int
      ; output_index : int
      }
    [@@deriving jsonaf, sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  (* ❼ Unknown object *)
  (* This is a catch-all for any unknown object type. *)
  module Unknown = struct
    type t = Jsonaf.t [@@deriving sexp, bin_io] [@@jsonaf.allow_extra_fields]
  end

  (* todo: add
  *)
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
    (* ❼ Unknown object *)
    (* This is a catch-all for any unknown object type. *)
    | Unknown of Unknown.t
  [@@deriving sexp, bin_io]

  let jsonaf_of_t = function
    | Output_item_added output_item_added ->
      Output_item_added.jsonaf_of_t output_item_added
    | Output_item_done output_item_done -> Output_item_done.jsonaf_of_t output_item_done
    | Output_text_delta output_text_delta ->
      Output_text_delta.jsonaf_of_t output_text_delta
    | Output_text_done output_text_done -> Output_text_done.jsonaf_of_t output_text_done
    | Function_call_arguments_delta function_call_arguments_delta ->
      Function_call_arguments_delta.jsonaf_of_t function_call_arguments_delta
    | Function_call_arguments_done function_call_arguments_done ->
      Function_call_arguments_done.jsonaf_of_t function_call_arguments_done
    | Response_created response_created -> Response_created.jsonaf_of_t response_created
    | Response_in_progress response_in_progress ->
      Response_in_progress.jsonaf_of_t response_in_progress
    | Reasoning_summary_text_delta reasoning_summary_text_delta ->
      Reasoning_summary_text_delta.jsonaf_of_t reasoning_summary_text_delta
    | Response_completed response_completed ->
      Response_completed.jsonaf_of_t response_completed
    | Response_incomplete response_incomplete ->
      Response_incomplete.jsonaf_of_t response_incomplete
    | Response_failed response_failed -> Response_failed.jsonaf_of_t response_failed
    | Content_part_added content_part_added ->
      Content_part_added.jsonaf_of_t content_part_added
    | Content_part_done content_part_done ->
      Content_part_done.jsonaf_of_t content_part_done
    | Response_refusal_delta response_refusal_delta ->
      Response_refusal_delta.jsonaf_of_t response_refusal_delta
    | Response_refusal_done response_refusal_done ->
      Response_refusal_done.jsonaf_of_t response_refusal_done
    | Annotation_added annotation_added -> Annotation_added.jsonaf_of_t annotation_added
    | File_search_call_in_progress file_search_call_in_progress ->
      File_search_call_in_progress.jsonaf_of_t file_search_call_in_progress
    | File_search_call_searching file_search_call_searching ->
      File_search_call_searching.jsonaf_of_t file_search_call_searching
    | File_search_call_completed file_search_call_completed ->
      File_search_call_completed.jsonaf_of_t file_search_call_completed
    | Web_search_call_in_progress web_search_call_in_progress ->
      Web_search_call_in_progress.jsonaf_of_t web_search_call_in_progress
    | Web_search_call_searching web_search_call_searching ->
      Web_search_call_searching.jsonaf_of_t web_search_call_searching
    | Web_search_call_completed web_search_call_completed ->
      Web_search_call_completed.jsonaf_of_t web_search_call_completed
    | Error error -> Error.jsonaf_of_t error
    (* ❼ Unknown object *)
    (* This is a catch-all for any unknown object type. *)
    | Unknown unknown -> unknown
  ;;

  let t_of_jsonaf json =
    match json with
    | `Object obj ->
      (match Jsonaf.member "type" (`Object obj) with
       | Some (`String "response.output_item.added") ->
         Output_item_added (Output_item_added.t_of_jsonaf (`Object obj))
       | Some (`String "response.output_item.done") ->
         Output_item_done (Output_item_done.t_of_jsonaf (`Object obj))
       | Some (`String "response.output_text.delta") ->
         Output_text_delta (Output_text_delta.t_of_jsonaf (`Object obj))
       | Some (`String "response.output_text.done") ->
         Output_text_done (Output_text_done.t_of_jsonaf (`Object obj))
       | Some (`String "response.function_call_arguments.delta") ->
         Function_call_arguments_delta
           (Function_call_arguments_delta.t_of_jsonaf (`Object obj))
       | Some (`String "response.function_call_arguments.done") ->
         Function_call_arguments_done
           (Function_call_arguments_done.t_of_jsonaf (`Object obj))
       | Some (`String "response.created") ->
         Response_created (Response_created.t_of_jsonaf (`Object obj))
       | Some (`String "response.in_progress") ->
         Response_in_progress (Response_in_progress.t_of_jsonaf (`Object obj))
       | Some (`String "response.reasoning_summary_text.delta") ->
         Reasoning_summary_text_delta
           (Reasoning_summary_text_delta.t_of_jsonaf (`Object obj))
       | Some (`String "response.completed") ->
         Response_completed (Response_completed.t_of_jsonaf (`Object obj))
       | Some (`String "response.incomplete") ->
         Response_incomplete (Response_incomplete.t_of_jsonaf (`Object obj))
       | Some (`String "response.failed") ->
         Response_failed (Response_failed.t_of_jsonaf (`Object obj))
       | Some (`String "response.content_part.added") ->
         Content_part_added (Content_part_added.t_of_jsonaf (`Object obj))
       | Some (`String "response.content_part.done") ->
         Content_part_done (Content_part_done.t_of_jsonaf (`Object obj))
       | Some (`String "response.refusal.delta") ->
         Response_refusal_delta (Response_refusal_delta.t_of_jsonaf (`Object obj))
       | Some (`String "response.refusal.done") ->
         Response_refusal_done (Response_refusal_done.t_of_jsonaf (`Object obj))
       | Some (`String "response.output_text.annotation.added") ->
         Annotation_added (Annotation_added.t_of_jsonaf (`Object obj))
       | Some (`String "response.file_search_call.in_progress") ->
         File_search_call_in_progress
           (File_search_call_in_progress.t_of_jsonaf (`Object obj))
       | Some (`String "response.file_search_call.searching") ->
         File_search_call_searching (File_search_call_searching.t_of_jsonaf (`Object obj))
       | Some (`String "response.file_search_call.completed") ->
         File_search_call_completed (File_search_call_completed.t_of_jsonaf (`Object obj))
       | Some (`String "response.web_search_call.in_progress") ->
         Web_search_call_in_progress
           (Web_search_call_in_progress.t_of_jsonaf (`Object obj))
       | Some (`String "response.web_search_call.searching") ->
         Web_search_call_searching (Web_search_call_searching.t_of_jsonaf (`Object obj))
       | Some (`String "response.web_search_call.completed") ->
         Web_search_call_completed (Web_search_call_completed.t_of_jsonaf (`Object obj))
       | Some (`String "error") -> Error (Error.t_of_jsonaf (`Object obj))
       | _ -> Unknown (`Object obj))
    | _ -> failwith "Invalid content format"
  ;;
end

type _ response_type =
  | Stream : (Response_stream.t -> unit) -> unit response_type
  | Default : Response.t response_type

exception Response_stream_parsing_error of Jsonaf.t * exn
exception Response_parsing_error of Jsonaf.t * exn

let post_response
  : type a.
    a response_type
    -> ?max_output_tokens:int
    -> ?temperature:float
    -> ?tools:Request.Tool.t list
    -> ?model:Request.model
    -> ?parallel_tool_calls:bool
    -> ?reasoning:Request.Reasoning.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> _ Eio.Net.t
    -> inputs:Item.t list
    -> a
  =
  fun res_typ
    ?(max_output_tokens = 600)
    ?temperature
    ?tools
    ?(model = Request.Gpt4)
    ?parallel_tool_calls
    ?reasoning
    ~dir
    net
    ~inputs ->
  let host = "api.openai.com" in
  let stream =
    match res_typ with
    | Default -> false
    | Stream _ -> true
  in
  let content_type =
    match res_typ with
    | Default -> "application/json"
    | Stream _ -> "application/json"
  in
  let headers =
    Http.Header.of_list
      [ "Authorization", "Bearer " ^ api_key
      ; "Content-Type", content_type
      ; "Connection", "keep-alive"
      ; "Cache-Control", "no-cache"
      ]
  in
  let input =
    Request.create
      ~model
      ~input:inputs
      ~max_output_tokens
      ?parallel_tool_calls
      ?temperature
      ?tools
      ?reasoning
      ~stream
      ()
  in
  let json = Jsonaf.to_string @@ Request.jsonaf_of_t input in
  let post json f = post ~net ~host ~headers ~path:"/v1/responses" (Raw f) json in
  post json
  @@ fun res ->
  let _, reader = res in
  match res_typ with
  | Default ->
    let read_all flow = Eio.Buf_read.(parse_exn take_all) flow ~max_size:Int.max_value in
    let data = read_all reader in
    Io.log
      ~dir
      ~file:"raw-openai-response.txt"
      ((Jsonaf.to_string @@ Jsonaf.of_string @@ data) ^ "\n");
    (* print_endline "Received data:";
    print_endline data; *)
    let json_result =
      Jsonaf.parse data
      |> Result.bind ~f:(fun json ->
        match Jsonaf.member "error" json with
        | None -> Or_error.error_string ""
        | Some `Null -> Or_error.error_string ""
        | Some json -> Ok json)
    in
    (match json_result with
     | Ok _ ->
       raise (Response_parsing_error (Jsonaf.of_string data, Failure "Error in response"))
     | Error _ -> (Response.t_of_jsonaf @@ Jsonaf.of_string @@ data : a))
  | Stream cb ->
    let reader = Eio.Buf_read.of_flow reader ~max_size:Int.max_value in
    let lines = Buf_read.lines reader in
    Switch.run
    @@ fun sw ->
    let rec loop seq =
      match Seq.uncons seq with
      | None -> ()
      | Some (line, seq) ->
        Fiber.fork ~sw
        @@ fun () ->
        (* Log the raw response line to a file for debugging purposes *)
        Io.log ~dir ~file:"raw-openai-streaming-response.txt" (line ^ "\n");
        let json_result =
          Jsonaf.parse line
          |> Result.bind ~f:(fun json ->
            match Jsonaf.member "error" json with
            | None -> Or_error.error_string ""
            | Some json -> Ok json)
        in
        (match json_result with
         | Ok _ ->
           print_endline "Received error:";
           print_endline line;
           failwith line
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
                  (try
                     let event = Response_stream.t_of_jsonaf @@ json in
                     Some event
                   with
                   | ex ->
                     print_endline
                       (Printf.sprintf
                          "Error parsing JSON from line: %s"
                          (Core.Exn.to_string ex));
                     print_endline line;
                     raise (Response_stream_parsing_error (json, ex)))
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
