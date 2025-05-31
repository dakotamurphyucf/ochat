module Chat_markdown : sig
  type function_call =
    { name : string
    ; arguments : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool_call =
    { id : string
    ; function_ : function_call
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type image_url = { url : string } [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A single item of content, which can be text or an image. *)
  type basic_content_item =
    { type_ : string [@key "type"]
    ; text : string option [@jsonaf.option]
    ; image_url : image_url option [@jsonaf.option]
    ; document_url : string option [@jsonaf.option]
    ; is_local : bool [@default false]
    ; cleanup_html : bool [@default false]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Agent content: has a url, is_local, and sub-items. *)
  type agent_content =
    { url : string
    ; is_local : bool
    ; items : content_item list [@default []]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* content_item can be either a Basic variant or an Agent variant. *)
  and content_item =
    | Basic of basic_content_item
    | Agent of agent_content
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* The overall content can be either a single string or a list of items. *)
  type chat_message_content =
    | Text of string
    | Items of content_item list
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning_summary =
    { text : string
    ; _type : string (* usually "summary" *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning =
    { summary : reasoning_summary list
    ; id : string
    ; status : string option
    ; _type : string (* always "reasoning" *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A generic <msg role="…"> element.  Still used for legacy messages
     (e.g. roles other than the four specialised shorthands).
     NOTE:  The newer shorthand tags (<user/>, <assistant/>, <tool_call/>,
     <tool_response/>) are mapped to dedicated OCaml record types that are
     aliases of [msg].  This removes the need to inspect the [role] string
     when traversing the parse-tree, while keeping the underlying shape
     identical so existing logic can be reused. *)

  type msg =
    { role : string
    ; content : chat_message_content option [@jsonaf.option]
    ; name : string option [@jsonaf.option]
    ; id : string option [@jsonaf.option]
    ; status : string option [@jsonaf.option]
    ; function_call : function_call option [@jsonaf.option]
    ; tool_call : tool_call option [@jsonaf.option]
    ; tool_call_id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Dedicated message records for the new shorthand tags.  They are simple
     aliases so that the JSON / serialisation helpers generated for [msg]
     can be re-used without code duplication. *)

  type user_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type assistant_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_call_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_response_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type custom_tool =
    { name : string
    ; description : string option
    ; command : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type agent_tool =
    { name : string
    ; description : string option
    ; agent : string
    ; is_local : bool
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool =
    | Builtin of string
    | Custom of custom_tool
    | Agent of agent_tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type config =
    { max_tokens : int option [@jsonaf.option]
    ; model : string option [@jsonaf.option]
    ; reasoning_effort : string option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    ; show_tool_call : bool
    ; id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type top_level_elements =
    | Msg of msg (** Legacy <msg/> element (system, developer…) *)
    | User of user_msg (** <user/> *)
    | Assistant of assistant_msg (** <assistant/> *)
    | Tool_call of tool_call_msg (** <tool_call/> *)
    | Tool_response of tool_response_msg (** <tool_response/> *)
    | Config of config
    | Reasoning of reasoning
    | Tool of tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  val parse_chat_inputs
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> string
    -> top_level_elements list
end
