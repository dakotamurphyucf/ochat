open! Core

module JT = Mcp_types

(** Handler type for executing a tool.  Takes the JSON arguments sent in the
    [tools/call] request and returns either a JSON result (on success) or an
    error message string. *)
type tool_handler = Jsonaf.t -> (Jsonaf.t, string) Result.t

(** A prompt as exposed by the server.  The representation follows the MCP
    schema at a very high level – for now we only need a textual description
    and the already-rendered chat messages that will be sent back to the
    client.  The concrete JSON structure of [messages] is left entirely to the
    caller so that we can round-trip it without interpretation. *)
type prompt =
  { description : string option
  ; messages : Jsonaf.t
  }

type t

(** [create ()] yields a fresh, empty registry.  All operations are
    thread-safe – internally a mutex protects the two registries so that the
    server can register new artefacts concurrently with running requests.  *)
val create : unit -> t

(** Hooks let the transport layer (e.g. the HTTP server) be informed when the
    list of tools or prompts changes so it can emit
    [notifications/*/list_changed] messages to connected clients. *)
val add_tools_changed_hook : t -> (unit -> unit) -> unit

val add_prompts_changed_hook : t -> (unit -> unit) -> unit

(** [register_tool t spec handler] registers a new tool with metadata [spec]
    and an OCaml [handler] implementation.  If another tool with the same name
    already exists it is silently replaced. *)
val register_tool : t -> JT.Tool.t -> tool_handler -> unit

(** [register_prompt t ~name prompt] registers a new named prompt.  Any prompt
    previously stored under that [name] is replaced. *)
val register_prompt : t -> name:string -> prompt -> unit

(** [list_tools t] returns all known tools in undefined order. *)
val list_tools : t -> JT.Tool.t list

(** [get_tool t name] looks up [name] and returns both the metadata and the
    execution handler. *)
val get_tool : t -> string -> (tool_handler * JT.Tool.t) option

(** [list_prompts t] returns [(name, prompt)] pairs for all registered prompts
    in undefined order. *)
val list_prompts : t -> (string * prompt) list

(** [get_prompt t name] fetches a single prompt by [name]. *)
val get_prompt : t -> string -> prompt option

