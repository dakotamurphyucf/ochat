open Core

open Jsonaf.Export

(*
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
[@@jsonaf.allow_extra_fields] [@@deriving jsonaf, sexp, bin_io] *)

type function_call =
  { name : string [@default ""]
  ; arguments : string [@default ""]
  }
[@@deriving jsonaf, sexp, bin_io]

type system_msg =
  { role : string
  ; content : string option
  ; name : string option [@jsonaf.option]
  }
[@@deriving jsonaf, sexp, bin_io]

type user_msg =
  { role : string
  ; content : string option
  ; name : string option [@jsonaf.option]
  }
[@@deriving jsonaf, sexp, bin_io]

type tool_call =
  { id : string
  ; type_ : string [@key "type"]
  ; func : function_call [@key "function"]
  }
[@@deriving jsonaf, sexp, bin_io]

type asst_msg =
  { role : string
  ; content : string option
  ; name : string option [@jsonaf.option]
  ; tool_calls : tool_call list option [@jsonaf.option]
  }
[@@deriving jsonaf, sexp, bin_io]

type tool_msg =
  { role : string
  ; content : string
  ; tool_call_id : string
  }
[@@deriving jsonaf, sexp, bin_io]

type msg =
  | System of system_msg
  | User of user_msg
  | Asst of asst_msg
  | Tool of tool_msg

type completion_msg =
  { content : string option
  ; tool_calls : tool_call list [@default []]
  ; role : string
  }
[@@deriving jsonaf, sexp, bin_io]

type choice =
  { finish_reason : string
  ; index : int
  ; message : completion_msg
  }
[@@deriving jsonaf, sexp, bin_io]

(*  so when parsing this I need to first convert to json object,
    check the role,
    and then convert to msg using the appropriate json converter and wrap in variant type *)

(* when converting just match on variant type and convert via json function for wrapped type*)
