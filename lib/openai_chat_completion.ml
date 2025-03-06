open Core

open Jsonaf.Export

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
