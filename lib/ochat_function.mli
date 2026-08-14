(** Tool-definition and execution helpers.

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
        let type_ = "function"
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

module Progress : sig
  (** Transient display progress. It is not canonical history and does not
      replace the runner's single final output. Text must be valid UTF-8. *)
  type channel =
    [ `Assistant
    | `Reasoning
    | `Stdout
    | `Stderr
    | `Activity
    ]

  type update =
    | Append of string
    | Replace of string
    (** Requested display update: append channel text, or replace the latest
      replaceable update on that channel. *)

  type t =
    { channel : channel
    ; update : update
    }
end

module Trace : sig
  type outcome =
    | Returned
    | Raised
    | Cancelled

  type tool_kind =
    [ `Function
    | `Custom
    ]

  type t =
    | Tool_started of
        { call_id : string
        ; name : string
        ; kind : tool_kind
        ; payload : string
        }
    | Tool_progress of
        { call_id : string
        ; progress : Progress.t
        }
    | Tool_finished of
        { call_id : string
        ; outcome : outcome
        ; output : Openai.Responses.Tool_output.Output.t option
        }
    (** Structured transient activity produced by a tool running inside an
      observed Agent. Returned outputs are non-authoritative display
      projections; the runner return remains the canonical output. *)
end

module Invocation : sig
  type t

  (** [silent] discards progress and reports that the invocation is unobserved. *)
  val silent : t

  (** [create emit] creates an observed invocation.

      {!emit} calls the callback synchronously. Exceptions are suppressed so
      observers cannot affect tool execution. A callback shared by independent
      invocations must be concurrency-safe and return promptly. *)
  val create : (Progress.t -> unit) -> t

  (** [create_with_trace ~progress ~trace] observes textual progress and
      structured nested-tool activity. Observer exceptions are suppressed. *)
  val create_with_trace : progress:(Progress.t -> unit) -> trace:(Trace.t -> unit) -> t

  (** [emit t progress] synchronously sends transient [progress] to [t]'s
      observer. Silent invocations discard it; observer exceptions are
      suppressed. *)
  val emit : t -> Progress.t -> unit

  (** [emit_trace t trace] synchronously sends structured transient activity.
      Silent invocations discard it and observer exceptions are suppressed. *)
  val emit_trace : t -> Trace.t -> unit

  (** [is_observed t] is [true] when [t] was created with {!create}. *)
  val is_observed : t -> bool
end

type runner = invocation:Invocation.t -> string -> Openai.Responses.Tool_output.Output.t

module type Def = sig
  (** Declarative description of a tool.  The module is never instantiated –
      its values act as a compile-time record. *)

  (** OCaml representation of the decoded arguments. *)
  type input

  (** Unique identifier exposed to the model.  Must match the regexp
      ["^[a-zA-Z0-9_]{1,64}$"]. *)
  val name : string

  (** Tool kind exposed to the model.

      This value is forwarded to OpenAI as the wire-level [type] for the tool
      descriptor.

      Common values:
      - ["function"]: a regular tool call with JSON [arguments].
      - ["custom"]: a custom tool call with a plain string [input] validated
        server-side by a grammar / format.

      Downstream code can use this to distinguish how to interpret
      {!val:parameters} (JSON schema vs custom format). *)
  val type_ : string

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

(** Registered tool.

    [info] is the descriptor exposed to the model. [run] executes silently and
    returns only the canonical final output. [run_with_progress] additionally
    accepts an invocation for transient display progress. *)
type t =
  { info : Openai.Completions.tool
  ; run : string -> Openai.Responses.Tool_output.Output.t
  ; run_with_progress : runner
  }

(** [create_function (module D) ?strict impl] couples the declarative module
    [D] with the OCaml implementation [impl].  The resulting [t] can be
    included in a model request's tool list.

    [strict] mirrors the field described in OpenAI docs: when [true] (the
    default) the model must supply exactly the schema; when [false] additional
    properties are permitted. *)
val create_function
  :  (module Def with type input = 'a)
  -> ?strict:bool (** default = [true] – controls OpenAI's argument parsing *)
  -> ('a -> Openai.Responses.Tool_output.Output.t)
  -> t

(** [create_streaming_function (module D) ?strict impl] registers [impl] as a
    progress-capable tool.

    Arguments are decoded before [impl] is called. [impl] may emit transient
    display progress and must return exactly one canonical final output. Every
    progress payload must be independently valid UTF-8. [run] is equivalent to
    [run_with_progress ~invocation:Invocation.silent], and observer failures do
    not alter the returned output. *)
val create_streaming_function
  :  (module Def with type input = 'a)
  -> ?strict:bool
  -> (invocation:Invocation.t -> 'a -> Openai.Responses.Tool_output.Output.t)
  -> t

(** [functions ts] converts a list of registered tools [ts] into:
    • the JSON metadata required by the API call; and
    • a lookup table mapping [name] → [implementation], convenient for serving
      the subsequent call.

    Tool names must be unique.

    @raise Core.Hashtbl.Duplicate_key if two tools expose the same name *)
val functions : t list -> Openai.Completions.tool list * (string, runner) Core.Hashtbl.t
