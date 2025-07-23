(** Runtime-shared server registry – implementation.  See
    {!module:Mcp_server_core} for the interface and high-level semantics. *)

open! Core
module JT = Mcp_types

(* Internal helper – a record bundling the tool metadata and the OCaml handler
   closure so that we can fetch both in a single lookup. *)

(* progress payload type needs to be defined early so that the main state
   record can reference it. *)

type progress_payload =
  { progress_token : string
  ; progress : float
  ; total : float option
  ; message : string option
  }

type tool_entry =
  { spec : JT.Tool.t
  ; handler : Jsonaf.t -> (Jsonaf.t, string) Result.t
  }

type prompt =
  { description : string option
  ; messages : Jsonaf.t
  }

(* ------------------------------------------------------------------ *)
(* Logging                                                             *)
(* ------------------------------------------------------------------ *)

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

type t =
  { tools : tool_entry String.Table.t
  ; prompts : prompt String.Table.t
  ; resources_changed_hooks : (unit -> unit) list ref
  ; tools_changed_hooks : (unit -> unit) list ref
  ; prompts_changed_hooks : (unit -> unit) list ref
  ; logging_hooks : (level:log_level -> logger:string option -> Jsonaf.t -> unit) list ref
  ; progress_hooks : (progress_payload -> unit) list ref
  ; cancelled : String.Hash_set.t
  }

let create () =
  { tools = String.Table.create ()
  ; prompts = String.Table.create ()
  ; resources_changed_hooks = ref []
  ; tools_changed_hooks = ref []
  ; prompts_changed_hooks = ref []
  ; logging_hooks = ref []
  ; progress_hooks = ref []
  ; cancelled = Hash_set.create (module String) ~size:32
  }
;;

(* ------------------------------------------------------------------ *)
(* Cancellation helpers                                                *)
(* ------------------------------------------------------------------ *)

let id_to_key (id : JT.Jsonrpc.Id.t) : string =
  match id with
  | JT.Jsonrpc.Id.String s -> "s:" ^ s
  | JT.Jsonrpc.Id.Int i -> "i:" ^ Int.to_string i
;;

let cancel_request t ~id = Hash_set.add t.cancelled (id_to_key id)
let is_cancelled t ~id = Hash_set.mem t.cancelled (id_to_key id)

let run_hooks l =
  List.iter !l ~f:(fun f ->
    try f () with
    | _ -> ())
;;

let register_tool t spec handler =
  let key =
    let { JT.Tool.name; _ } = spec in
    name
  in
  Hashtbl.set t.tools ~key ~data:{ spec; handler };
  (* Fire hooks *)
  run_hooks t.tools_changed_hooks
;;

let register_prompt t ~name prompt =
  Hashtbl.set t.prompts ~key:name ~data:prompt;
  run_hooks t.prompts_changed_hooks
;;

(* ------------------------------------------------------------------ *)
(* Resources change notification                                        *)
(* ------------------------------------------------------------------ *)

let add_resources_changed_hook t f =
  t.resources_changed_hooks := f :: !(t.resources_changed_hooks)
;;

let notify_resources_changed t = run_hooks t.resources_changed_hooks
let add_tools_changed_hook t f = t.tools_changed_hooks := f :: !(t.tools_changed_hooks)

let add_prompts_changed_hook t f =
  t.prompts_changed_hooks := f :: !(t.prompts_changed_hooks)
;;

let add_logging_hook t f = t.logging_hooks := f :: !(t.logging_hooks)

let log t ~level ?logger data =
  List.iter !(t.logging_hooks) ~f:(fun f ->
    try f ~level ~logger data with
    | _ -> ())
;;

(* ------------------------------------------------------------------ *)
(* Progress helpers                                                    *)
(* ------------------------------------------------------------------ *)

let add_progress_hook t f = t.progress_hooks := f :: !(t.progress_hooks)

let notify_progress t payload =
  List.iter !(t.progress_hooks) ~f:(fun f ->
    try f payload with
    | _ -> ())
;;

let list_tools t = List.map (Hashtbl.data t.tools) ~f:(fun { spec; _ } -> spec)

let get_tool t name =
  Option.map (Hashtbl.find t.tools name) ~f:(fun { spec; handler } -> handler, spec)
;;

let list_prompts t = Hashtbl.to_alist t.prompts
let get_prompt t name = Hashtbl.find t.prompts name

(* Expose alias types from interface *)
type tool_handler = Jsonaf.t -> (Jsonaf.t, string) Result.t
