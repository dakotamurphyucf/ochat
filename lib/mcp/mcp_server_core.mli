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

(* ------------------------------------------------------------------ *)
(* Logging – servers can emit structured log messages via hooks so that the
   HTTP transport can forward them to connected clients.                *)

type t

type log_level =
  [ `Debug
  | `Info
  | `Notice
  | `Warning
  | `Error
  | `Critical
  | `Alert
  | `Emergency
  ]

(** [log t ~level ?logger data] emits a structured log entry that is forwarded
    to every registered logging hook.  [data] should be an arbitrary JSON
    object/value describing the event. *)
val log : t -> level:log_level -> ?logger:string -> Jsonaf.t -> unit

(** Register a callback that is invoked for each log event.  The HTTP server
    uses this to broadcast [notifications/message] SSE events to clients. *)
val add_logging_hook
  :  t
  -> (level:log_level -> logger:string option -> Jsonaf.t -> unit)
  -> unit

(* ------------------------------------------------------------------ *)
(* Progress notifications *)
(* ------------------------------------------------------------------ *)

(** Structured payload for progress updates as per MCP
    [notifications/progress].  The [progress] field is a monotonically
    increasing value (typically 0–1 when [total] is omitted).  When a
    concrete [total] is known the sender SHOULD supply it so that UIs can
    derive a percentage.  The [message] is intended for human readable
    status updates (e.g. "Reticulating splines…"). *)
type progress_payload =
  { progress_token : string
  ; progress : float
  ; total : float option
  ; message : string option
  }

val add_progress_hook : t -> (progress_payload -> unit) -> unit
val notify_progress : t -> progress_payload -> unit

(* ------------------------------------------------------------------ *)
(* Cancellation                                                        *)
(* ------------------------------------------------------------------ *)

(** Mark a request as cancelled.  Servers invoke this from the router when a
    [notifications/cancelled] message is received. *)
val cancel_request : t -> id:Mcp_types.Jsonrpc.Id.t -> unit

(** Test whether a request has been cancelled.  Long-running handlers can
    poll this cooperatively and abort early to free resources. *)
val is_cancelled : t -> id:Mcp_types.Jsonrpc.Id.t -> bool

(** [create ()] yields a fresh, empty registry.  All operations are
    thread-safe – internally a mutex protects the two registries so that the
    server can register new artefacts concurrently with running requests.  *)
val create : unit -> t

(** Hooks let the transport layer (e.g. the HTTP server) be informed when the
    list of tools or prompts changes so it can emit
    [notifications/*/list_changed] messages to connected clients. *)
val add_tools_changed_hook : t -> (unit -> unit) -> unit

val add_prompts_changed_hook : t -> (unit -> unit) -> unit

(* Hooks for server-driven resource list updates (e.g. when new files appear
   on disk).  Transport layers can register a callback that will be invoked
   whenever [notify_resources_changed] is called. *)
val add_resources_changed_hook : t -> (unit -> unit) -> unit
val notify_resources_changed : t -> unit

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
