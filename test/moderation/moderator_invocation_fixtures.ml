open Core
module M = Chat_response.Moderator_manager
module MI = Chat_response.Moderator_invocation
module EC = Chat_response.Extension_compiler
module Cap = Chat_response.Tool_capability
module P = Agent_protocol
module I = P.Invocation
module L = Chatml.Chatml_lang
module R = Chatml_host_runtime

let ok = Result.ok_or_failwith

let diags result =
  result
  |> Result.map_error ~f:(fun ds ->
    String.concat ~sep:"; " (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string))
  |> ok
;;

let protocol result = result |> Result.map_error ~f:(fun e -> e.P.Error.message) |> ok

let expect prefix = function
  | Error message when String.is_substring message ~substring:prefix -> ()
  | Error message -> failwith ("expected " ^ prefix ^ ", got " ^ message)
  | Ok _ -> failwith ("expected " ^ prefix)
;;

let generator =
  let counter = ref 0 in
  P.Id.Generator.create ~bytes:(fun n ->
    incr counter;
    String.make n (Char.of_int_exn (!counter mod 255)))
;;

let setup
      env
      ?(schema = "true")
      ?(initial = "0")
      ?(capabilities = Chat_response.Moderation.Capabilities.default)
      ?(events = "| _ -> Task.pure(state)")
      ?(script_limits = "")
      body
  =
  let dir = Eio.Stdenv.cwd env in
  let source =
    "let initial_state = "
    ^ initial
    ^ "\nlet on_event = fun ctx state event -> match event with\n| `Tool_invoked(p) -> "
    ^ body
    ^ "\n"
    ^ events
  in
  let loader =
    Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", schema ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader:loader
      ("<script id=\"handler\" language=\"chatml\" kind=\"moderator\" \
        api=\"extensibility-v1\" "
       ^ script_limits
       ^ ">"
       ^ source
       ^ "</script><tool name=\"counter\" type=\"moderator\" moderator=\"handler\" \
          input_schema=\"schema.json\" output_schema=\"schema.json\"/>")
  in
  let caps =
    Cap.create
      ~owner:"fixture"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
      []
    |> Result.map_error ~f:(fun e -> e.Cap.message)
    |> ok
  in
  let definition =
    EC.prepare_definition_in_domain ~env ~capabilities:caps elements |> diags
  in
  let _, artifact = M.Registry.of_definition M.Registry.empty definition |> ok in
  let artifact = Option.value_exn artifact in
  let allocator =
    History_entry.Allocator.create ~namespace:"invocation-fixture" ~next_sequence:0 |> ok
  in
  let manager = M.create_entries ~env ~artifact ~capabilities ~allocator () |> ok in
  let prepared = List.hd_exn (EC.prepared_tools definition) in
  let make ?(input = `Null) () =
    I.create
      I.
        { id = P.Id.Invocation.create_with generator
        ; session_id = P.Id.Session.create_with generator
        ; generation = 0
        ; origin = Model
        ; provider_call_id = Some "provider-call"
        ; call_entry_id = None
        ; parent_invocation = None
        ; parent_job = None
        ; tool_name = "counter"
        ; implementation_revision = EC.fingerprint prepared
        ; capability_fingerprint = Cap.fingerprint (EC.capabilities prepared)
        ; input
        ; created_at = P.Timestamp.now ()
        ; deadline = None
        }
    |> protocol
    |> I.dispatch
    |> protocol
  in
  manager, prepared, make
;;

let call
      ?(validate_work = fun _ -> Error "invocation.invalid_work: not owned")
      ?(prepare_resolution = fun ~resolved:_ ~outcome:_ ~snapshot:_ -> Ok ignore)
      manager
      invocation
  =
  M.handle_invocation_entries
    manager
    ~invocation
    ~history:[]
    ~available_tools:[]
    ~session_meta:`Null
    ~now_ms:0
    ~validate_work
    ~prepare_resolution:(fun ~resolved ~outcome ~snapshot ->
      Result.map (prepare_resolution ~resolved ~outcome ~snapshot) ~f:M.memory_commit)
;;

let state manager = (M.identity_snapshot manager |> ok).current_state
