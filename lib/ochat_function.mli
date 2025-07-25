(** GPT function helpers for the OpenAI Chat Completions API.

    OpenAI models can invoke so-called [tools] (also called [functions]) when
    the user or the assistant requests a structured action.  Each tool is
    described by a JSON schema and identified by a unique [name].  At runtime
    the model responds with a *function-call request* – a record containing the
    tool name and a JSON blob with the arguments.  The host application then
    looks up the implementation, executes it, and supplies the textual result
    back to the model.

    This module provides a minimal abstraction to register such tools and to
    bridge between their declarative description (schema) and their concrete
    OCaml implementation.

    Typical workflow:

    {[
      open Core

      (* 1.  Declare the tool *)
      module Echo : Ochat_function.Def with type input = string = struct
        type input = string

        let name = "echo"
        let description = Some "Return the given string unchanged"

        let parameters : Jsonaf.t =
          `Object
            [ "type", `String "object"
            ; "properties", `Object [ "text", `Object [ "type", `String "string" ] ]
            ; "required", `Array [ `String "text" ]
            ; "additionalProperties", `False
            ]

        let input_of_string s =
          Jsonaf.of_string s |> Jsonaf.member_exn "text" |> Jsonaf.string_exn
      end

      (* 2.  Provide the implementation *)
      let echo_impl (text : string) = text

      (* 3.  Register *)
      let echo_tool : Ochat_function.t =
        Ochat_function.create_function (module Echo) echo_impl

      (* 4.  Bundle several tools for the API call *)
      let tools, dispatch_tbl = Ochat_function.functions [ echo_tool ]
    ]}
*)

module type Def = sig
  (** Declarative description of a tool.  The module is never instantiated –
      its values act as a compile-time record. *)

  (** OCaml representation of the decoded arguments. *)
  type input

  (** Unique identifier exposed to the model.  Must match the regexp
      ["^[a-zA-Z0-9_]{1,64}$"]. *)
  val name : string

  (** Short, human-readable summary presented to the model. *)
  val description : string option

  (** JSON Schema object describing the arguments.  The schema **must** follow
      the subset supported by OpenAI as described in
      https://platform.openai.com/docs/guides/function-calling/function-definitions.
  *)
  val parameters : Jsonaf.t

  (** Parse the [arguments] JSON received from the model into an [input]
      value.  Implementations typically call {!Jsonaf.of_string} and extract
      the required fields.  The function should raise an exception when the
      payload is ill-formed. *)
  val input_of_string : string -> input
end

(** Concrete handle to a registered tool.  [info] – JSON description passed to
    OpenAI.  [run] – OCaml implementation executed when the tool is invoked.

    The record is exposed – downstream code frequently needs direct access to
    [run], e.g. to dispatch the model’s callback.  Feel free to treat the type
    as mutable if necessary, but do **not** modify [info] fields after the
    value has been passed to OpenAI. *)
type t =
  { info : Openai.Completions.tool
  ; run : string -> string
  }

(** [create_function (module D) ?strict impl] couples the declarative module
    [D] with the OCaml implementation [impl].  The resulting [t] can be
    included in the tool list passed to
    {!Openai.Completions.post_chat_completion}.

    [strict] mirrors the field described in OpenAI docs: when [true] (the
    default) the model must supply exactly the schema; when [false] additional
    properties are permitted. *)
val create_function
  :  (module Def with type input = 'a)
  -> ?strict:bool (** default = [true] – controls OpenAI's argument parsing *)
  -> ('a -> string)
  -> t

(** [functions ts] converts a list of registered tools [ts] into:
    • the JSON metadata required by the API call; and
    • a lookup table mapping [name] → [implementation], convenient for serving
      the subsequent call. *)
val functions
  :  t list
  -> Openai.Completions.tool list * (string, string -> string) Core.Hashtbl.t
