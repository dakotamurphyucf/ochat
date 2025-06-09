open! Core

module JT = Mcp_types

(* Internal helper – a record bundling the tool metadata and the OCaml handler
   closure so that we can fetch both in a single lookup. *)
type tool_entry =
  { spec : JT.Tool.t
  ; handler : Jsonaf.t -> (Jsonaf.t, string) Result.t
  }

type prompt =
  { description : string option
  ; messages : Jsonaf.t
  }

type t =
  { tools : tool_entry String.Table.t
  ; prompts : prompt String.Table.t
  ; tools_changed_hooks : (unit -> unit) list ref
  ; prompts_changed_hooks : (unit -> unit) list ref
  }

let create () =
  { tools = String.Table.create ()
  ; prompts = String.Table.create ()
  ; tools_changed_hooks = ref []
  ; prompts_changed_hooks = ref []
  }

let run_hooks l = List.iter !l ~f:(fun f ->
    try f () with _ -> ())

let register_tool t spec handler =
  let key =
    let { JT.Tool.name; _ } = spec in
    name
  in
  Hashtbl.set t.tools ~key ~data:{ spec; handler };
  (* Fire hooks *)
  run_hooks t.tools_changed_hooks

let register_prompt t ~name prompt =
  Hashtbl.set t.prompts ~key:name ~data:prompt;
  run_hooks t.prompts_changed_hooks

let add_tools_changed_hook t f = t.tools_changed_hooks := f :: !(t.tools_changed_hooks)

let add_prompts_changed_hook t f =
  t.prompts_changed_hooks := f :: !(t.prompts_changed_hooks)

let list_tools t = List.map (Hashtbl.data t.tools) ~f:(fun { spec; _ } -> spec)

let get_tool t name =
  Option.map (Hashtbl.find t.tools name) ~f:(fun { spec; handler } -> handler, spec)

let list_prompts t = Hashtbl.to_alist t.prompts

let get_prompt t name = Hashtbl.find t.prompts name

(* Expose alias types from interface *)
type tool_handler = Jsonaf.t -> (Jsonaf.t, string) Result.t



