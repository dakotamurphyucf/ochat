type template_element =
  | System_message : string -> template_element
  | Variable : string -> template_element
  | Expression : string -> template_element
  | Text : string -> template_element

val insides : string Angstrom.t
val c : template_element Angstrom.t
val system_message_parser : string Angstrom.t
val expression_parser : 'a Angstrom.t -> 'a Angstrom.t
val text_parser : template_element Angstrom.t
val template_parser : template_element list Angstrom.t

module type TemplateHandler = sig
  type t

  val handle_variable : string -> t
  val handle_function : string -> t
  val handle_text : string -> t
  val to_string : t -> string
end

module MakeTemplateProcessor : functor (Handler : TemplateHandler) -> sig
  val process_template : template_element list -> Handler.t list
end

module MyTemplateHandler : TemplateHandler

module MyTemplateProcessor : sig
  val process_template : template_element list -> MyTemplateHandler.t list
end

val run : unit -> unit

module Chat_markdown : sig
  type function_call =
  { name : string
  ; arguments : string
  }
[@@deriving jsonaf, sexp]

type msg =
  { role : string
  ; content : string option
  ; name : string option [@jsonaf.option]
  ; function_call : function_call option [@jsonaf.option]
  }
[@@deriving jsonaf, sexp]
  val parse_chat_inputs : string -> msg list
end
