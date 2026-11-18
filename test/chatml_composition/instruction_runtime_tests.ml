open! Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module Selection = Agent_session.Instruction_selection

(* This connection is actual authenticated ingress. The guard is retained by the
   actor; a principal-only fixture connection would not qualify source use. *)
let authenticated_connection ?actor ?(is_current = fun () -> true) daemon =
  let notifications = Eio.Stream.create 256 in
  let actor =
    Option.value_or_thunk actor ~default:(fun () ->
      Operator_authorization.guarded ~principal:(principal ()) ~is_current)
  in
  let context =
    Agent_server.Connection_context.create_authenticated
      ~actor
      ~connection_id:(P.Id.Attachment.create () |> P.Id.Attachment.to_string)
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:64
  in
  Agent_client.In_memory.create
    ~request:(fun command ->
      Agent_server.Dispatcher.dispatch_command
        (Agent_server.Daemon.dispatcher daemon)
        ~context
        command)
    ~notifications
    ~close:(fun () -> Agent_server.Daemon.close_connection daemon context)
;;

let configuration =
  {|(version 1)
(server ((data_dir "./data") (unix_socket "./agent.sock")
         (project_sources ((external_roots ("./workspace"))))
         (authoring_project_access
          ((read_grants (((root "./workspace") (principal_id pri_restart_test)
                          (authentication_kind test))))
           (selections ())))))
(workspaces (((id restart.workspace) (source (physical "./workspace")) (access shared_write)
              (prompt_limits (((prompt restart.prompt) (max_root_agents 2) (overflow reject)))))))
(prompts (((id restart.prompt) (path "./agent.chatmd") (allowed_workspaces (restart.workspace))
           (permission_profile restart.permission) (enabled true))))
(permission_profiles (((id restart.permission) (tool_default allow) (approval_timeout none)
                       (approval_fallback deny) (manifest_authorization assume_authorized))))|}
;;

let agent =
  {|<developer>AUTHORED-HIGHER-PRIORITY</developer>
<tool name="instructions_refresh"/>
<tool name="instructions_read"/>
<tool name="instructions_activate"/>
<tool name="instructions_deactivate"/>
<tool name="instructions_status"/>|}
;;

let root_guidance = "ROOT-AGENTS-ORIGINAL\n"
let leaf_guidance = "LEAF-AGENTS-ORIGINAL\n"

let skill name =
  sprintf
    "---\nname: %s\ndescription: Exercise %s guidance\n---\nSKILL-%s-ORIGINAL\n"
    name
    name
    name
;;

let response inputs id =
  let json =
    List.find_map_exn inputs ~f:(function
      | Openai.Responses.Item.Function_call_output { call_id; output = Text text; _ }
        when String.equal call_id id -> Some (Jsonaf.of_string text)
      | _ -> None)
  in
  match P.Invocation.outcome_of_json json with
  | Ok outcome -> outcome
  | Error error ->
    raise_s [%sexp "native output did not use Invocation_v1", (error : P.Error.t)]
;;

let complete inputs id =
  match response inputs id with
  | Complete value -> value
  | outcome ->
    raise_s [%sexp "instruction native failed", (id : string), (outcome : I.outcome)]
;;

let fields = function
  | `Object fields -> fields
  | json -> raise_s [%sexp "expected object", (json : Jsonaf.t)]
;;

let member name json = Jsonaf.member_exn name json
let string name json = member name json |> Jsonaf.string_exn
let array name json = member name json |> Jsonaf.list_exn
let object_ fields = `Object fields
let empty = object_ []
let refresh selection = object_ [ "selection", selection ]

let selection ~kind ~root ~scope =
  object_ [ "kind", `String kind; "roots", `Array [ root ]; "scope", scope ]
;;

let descriptor selection discovery =
  object_
    [ "selection", selection
    ; ( "members"
      , `Array
          (List.map (array "members" discovery) ~f:(fun observed ->
             object_
               (List.map [ "root"; "member"; "sha256" ] ~f:(fun name ->
                  name, member name observed)))) )
    ]
;;

let activate descriptor capture_id ~revision ~key =
  object_
    (fields descriptor
     @ [ "capture_id", `String capture_id
       ; "expected_revision", `String revision
       ; "key", `String key
       ])
;;

let clear ~revision ~key =
  object_
    [ "kind", `String "all"
    ; "members", `Null
    ; "expected_revision", `String revision
    ; "key", `String key
    ]
;;

let wire_text inputs =
  Jsonaf.to_string (`Array (List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t))
;;

let guidance_inputs inputs =
  List.filter_map inputs ~f:(fun input ->
    let json = Openai.Responses.Item.jsonaf_of_t input in
    match Jsonaf.member "role" json with
    | Some (`String "user")
      when String.is_substring
             (Jsonaf.to_string json)
             ~substring:"OChat session guidance from explicitly selected files." ->
      Some json
    | _ -> None)
;;

let%expect_test "actual native CAS, retained retry, inert refresh and exact User guidance"
  =
  let inputs = ref [] in
  let project = ref empty in
  let skills = ref empty in
  let root = ref `Null in
  let project_capture = ref "" in
  let skill_capture = ref "" in
  let first_receipt = ref empty in
  let workspace = ref None in
  let seen_guided = ref 0 in
  let native_only_children = ref 0 in
  let prepared_guidance = ref false in
  let selected = ref None in
  let project_selection () =
    selection ~kind:"project" ~root:!root ~scope:(`String "sub")
  in
  let skill_selection () = selection ~kind:"skills" ~root:!root ~scope:`Null in
  let call id name value = [ id, name, value ] in
  with_daemon
    ~request_guidance:true
    ~inspect_preparation:(fun ~selection ~request ->
      prepared_guidance := Option.is_some (Inference.Request.guidance request);
      selected := selection)
    ~config_file:"server.sexp"
    ~sources:
      [ ( "agent.chatmd"
        , agent ^ {|<tool name="native_review" agent="native-child.chatmd" local/>|} )
      ; "server.sexp", configuration
      ; "native-child.chatmd", "<developer>GUIDANCE-NATIVE-ONLY-CHILD</developer>"
      ]
    ~workspace_files:
      [ "AGENTS.md", root_guidance
      ; "sub/AGENTS.md", leaf_guidance
      ; "alpha/SKILL.md", skill "alpha"
      ; "beta/SKILL.md", skill "beta"
      ]
    ~connect:(fun ~sw:_ ~env ~root daemon ->
      workspace := Some Eio.Path.(Eio.Stdenv.fs env / root / "workspace");
      authenticated_connection daemon)
    ~calls:[ "roots", "instructions_refresh", refresh `Null ]
    ~completion_timeout:90.
    ~initial_requests:17
    ~expected_requests:17
    ~inspect_request:(fun number request ->
      inputs := request;
      if Int.equal number 6 then ignore (complete request "activate" : Jsonaf.t);
      if Int.equal number 9 then ignore (complete request "activate-skills" : Jsonaf.t);
      assert (
        String.is_substring (wire_text request) ~substring:"AUTHORED-HIGHER-PRIORITY");
      let guidance = guidance_inputs request in
      if number >= 6 && number <= 15
      then (
        incr seen_guided;
        if List.length guidance <> 1
        then
          raise_s
            [%sexp
              "missing or duplicated root request guidance"
            , (number : int)
            , (List.length guidance : int)
            , (!prepared_guidance : bool)
            , (!selected : P.Instruction_status.t option)];
        let text = Jsonaf.to_string (List.hd_exn guidance) in
        let position marker = String.substr_index_exn text ~pattern:marker in
        assert (position "ROOT-AGENTS-ORIGINAL" < position "LEAF-AGENTS-ORIGINAL");
        if number >= 9
        then (
          assert (position "LEAF-AGENTS-ORIGINAL" < position "SKILL-beta-ORIGINAL");
          assert (position "SKILL-beta-ORIGINAL" < position "SKILL-alpha-ORIGINAL"))
        else assert (not (String.is_substring text ~substring:"SKILL-")))
      else [%test_eq: int] 0 (List.length guidance))
    ~auxiliary_response:(fun request ->
      if String.is_substring (wire_text request) ~substring:"GUIDANCE-NATIVE-ONLY-CHILD"
      then (
        incr native_only_children;
        [%test_eq: int] 1 (List.length (guidance_inputs request));
        assert (
          String.is_substring (wire_text request) ~substring:(String.strip root_guidance));
        Some (call_events []))
      else None)
    ~followup_calls:(fun number ->
      match number with
      | 2 ->
        root := List.hd_exn (array "roots" (complete !inputs "roots"));
        call "discover" "instructions_refresh" (refresh (project_selection ()))
      | 3 ->
        let discovery = complete !inputs "discover" in
        [%test_eq: string list]
          [ "AGENTS.md"; "sub/AGENTS.md" ]
          (List.map (array "members" discovery) ~f:(string "member"));
        project := descriptor (project_selection ()) discovery;
        call "read" "instructions_read" !project
      | 4 ->
        project_capture := string "capture_id" (complete !inputs "read");
        call
          "bad-cas"
          "instructions_activate"
          (activate !project !project_capture ~revision:"99" ~key:"bad-cas")
      | 5 ->
        (match response !inputs "bad-cas" with
         | Fail _ -> ()
         | outcome -> raise_s [%sexp (outcome : I.outcome)]);
        call
          "activate"
          "instructions_activate"
          (activate !project !project_capture ~revision:"0" ~key:"project-v1")
      | 6 ->
        first_receipt := complete !inputs "activate";
        call "skills" "instructions_refresh" (refresh (skill_selection ()))
      | 7 ->
        let discovery = complete !inputs "skills" in
        (* Explicit skill order is author selection, not directory iteration. *)
        let members = List.rev (array "members" discovery) in
        let discovery = object_ [ "members", `Array members ] in
        skills := descriptor (skill_selection ()) discovery;
        call "read-skills" "instructions_read" !skills
      | 8 ->
        skill_capture := string "capture_id" (complete !inputs "read-skills");
        call
          "activate-skills"
          "instructions_activate"
          (activate !skills !skill_capture ~revision:"1" ~key:"skills-v1")
      | 9 ->
        ignore (complete !inputs "activate-skills" : Jsonaf.t);
        call "status" "instructions_status" empty
      | 10 ->
        let status =
          complete !inputs "status" |> P.Instruction_status.of_json |> protocol_ok
        in
        [%test_eq: int64] 2L status.revision;
        [%test_eq: int] 2 status.project_files;
        [%test_eq: int] 2 status.skills;
        let workspace = Option.value_exn !workspace in
        List.iter
          [ "AGENTS.md"; "sub/AGENTS.md"; "alpha/SKILL.md"; "beta/SKILL.md" ]
          ~f:(fun name -> Eio.Path.unlink Eio.Path.(workspace / name));
        call
          "retry"
          "instructions_activate"
          (activate !project !project_capture ~revision:"0" ~key:"project-v1")
      | 11 ->
        assert (Jsonaf.exactly_equal !first_receipt (complete !inputs "retry"));
        call
          "fresh-missing"
          "instructions_activate"
          (activate !project !project_capture ~revision:"2" ~key:"project-v2")
      | 12 ->
        (match response !inputs "fresh-missing" with
         | Fail _ -> ()
         | outcome -> raise_s [%sexp (outcome : I.outcome)]);
        call "refresh-missing" "instructions_refresh" (refresh (project_selection ()))
      | 13 ->
        [%test_eq: int]
          0
          (List.length (array "members" (complete !inputs "refresh-missing")));
        call "status-retained" "instructions_status" empty
        @ call
            "native-only-child"
            "native_review"
            (object_ [ "input", `String "Inspect captured guidance." ])
      | 14 ->
        ignore (complete !inputs "native-only-child" : Jsonaf.t);
        let status =
          complete !inputs "status-retained"
          |> P.Instruction_status.of_json
          |> protocol_ok
        in
        [%test_eq: int64] 2L status.revision;
        call
          "stale-clear"
          "instructions_deactivate"
          (clear ~revision:"1" ~key:"stale-clear")
      | 15 ->
        (match response !inputs "stale-clear" with
         | Fail _ -> ()
         | outcome -> raise_s [%sexp (outcome : I.outcome)]);
        call "clear" "instructions_deactivate" (clear ~revision:"2" ~key:"clear")
      | 16 ->
        ignore (complete !inputs "clear" : Jsonaf.t);
        call "status-cleared" "instructions_status" empty
      | 17 ->
        let status =
          complete !inputs "status-cleared" |> P.Instruction_status.of_json |> protocol_ok
        in
        [%test_eq: int64] 3L status.revision;
        assert (not status.project_selected);
        [%test_eq: int] 0 status.skills;
        []
      | _ -> failwith "unexpected instruction transcript stage")
    (fun state ->
       [%test_eq: int] 10 !seen_guided;
       [%test_eq: int] 1 !native_only_children;
       let selected = Option.value_exn state.instruction_selection in
       [%test_eq: int] 3 (List.length (Selection.receipts selected));
       [%test_eq: int] 2 (List.length (Selection.captures selected));
       assert (List.is_empty (Selection.project selected));
       assert (List.is_empty (Selection.skills selected));
       assert (
         not
           (List.exists state.conversation.canonical_history ~f:(fun entry ->
              String.is_substring
                (Sexp.to_string (P.History.sexp_of_entry entry))
                ~substring:"OChat session guidance from explicitly selected files.")));
       print_endline
         "genuine selected natives; CAS and retry ACKs; captured root/leaf/ordered \
          skills once at User level; refresh inert; Clear removes guidance");
  [%expect
    {| genuine selected natives; CAS and retry ACKs; captured root/leaf/ordered skills once at User level; refresh inert; Clear removes guidance |}]
;;

let%expect_test
    "agent children and claimed jobs inherit guidance; independent recipe reentry stays \
     excluded"
  =
  let inputs = ref [] in
  let selected = ref empty in
  let descriptor_ = ref empty in
  let preparations = ref [] in
  let checked_complete inputs id =
    match response inputs id with
    | Complete value -> value
    | outcome ->
      raise_s
        [%sexp
          "agent scope native failed"
        , (id : string)
        , (outcome : I.outcome)
        , (List.rev !preparations
           : (bool * P.Instruction_status.t option * int * int) list)]
  in
  let nested = ref 0
  and forks = ref 0
  and recipes = ref 0
  and reentries = ref 0
  and background = ref 0 in
  let has_developer inputs marker =
    List.exists inputs ~f:(fun input ->
      let json = Openai.Responses.Item.jsonaf_of_t input in
      match Jsonaf.member "role" json with
      | Some (`String "developer") ->
        String.is_substring (Jsonaf.to_string json) ~substring:marker
      | _ -> false)
  in
  let check_guidance inputs expected =
    [%test_eq: int] expected (List.length (guidance_inputs inputs))
  in
  let root_agent =
    agent
    ^ {|
<tool name="review" agent="nested.chatmd" local/>
<tool name="background_review" agent="background.chatmd" local/>
<tool name="recipe_source" agent="recipe.chatmd" local/>
<tool name="fork"/>
<script id="routing" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  if p.context.tool_name == "recipe_probe" then
    let* result = Model.call("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("recipe.chatmd")},
      {key = "input"; value = `String("Run the independent recipe.")},
      {key = "is_local"; value = `Bool(true)}
    ])) in
    (match result with
    | `Ok(value) ->
      let* ignored = Invocation.resolve(p.context.invocation_id, `Complete(value)) in
      Task.pure(state + 1)
    | `Refused(message) -> Task.fail(message)
    | `Error(message) -> Task.fail(message))
  else
    let* job = Job.start_tool("background_review", `Object([
      {key = "input"; value = `String("Run the claimed background agent.")}
    ])) in
    let* ignored = Invocation.resolve(p.context.invocation_id,
      `Pending(`Job(job), `String("accepted"))) in
    Task.pure(state + 1)
| _ -> Task.pure(state)
</script>
<tool name="recipe_probe" type="moderator" moderator="routing" input_schema="object.json" output_schema="any.json"/>
<tool name="background_probe" type="moderator" moderator="routing" input_schema="object.json" output_schema="any.json"/>|}
  in
  let call id name value = [ id, name, value ] in
  with_daemon
    ~request_guidance:true
    ~inspect_preparation:(fun ~selection ~request ->
      preparations
      := ( Option.is_some (Inference.Request.guidance request)
         , selection
         , List.length (Inference.Request.history request)
         , List.length (Inference.Request.tools request) )
         :: !preparations)
    ~config_file:"server.sexp"
    ~completion_timeout:90.
    ~expect_moderator:true
    ~settle:Job_launch_tests.settle
    ~sources:
      [ "agent.chatmd", root_agent
      ; "server.sexp", configuration
      ; "any.json", "true"
      ; "object.json", {|{"type":"object"}|}
      ; "nested.chatmd", "<developer>GUIDANCE-JOINED-AGENT</developer>"
      ; "background.chatmd", "<developer>GUIDANCE-BACKGROUND-AGENT</developer>"
      ; ( "recipe.chatmd"
        , {|<developer>GUIDANCE-INDEPENDENT-RECIPE</developer>
<tool name="recipe_reentry" agent="reentry.chatmd" local/>|}
        )
      ; "reentry.chatmd", "<developer>GUIDANCE-RECIPE-REENTRY</developer>"
      ]
    ~workspace_files:[ "AGENTS.md", root_guidance; "sub/AGENTS.md", leaf_guidance ]
    ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon -> authenticated_connection daemon)
    ~calls:[ "roots", "instructions_refresh", refresh `Null ]
    ~initial_requests:9
    ~expected_requests:9
    ~inspect_request:(fun number request ->
      inputs := request;
      check_guidance request (if number >= 5 then 1 else 0))
    ~auxiliary_response:(fun request ->
      if has_developer request "GUIDANCE-JOINED-AGENT"
      then (
        incr nested;
        check_guidance request 1;
        Some (call_events []))
      else if has_developer request "GUIDANCE-BACKGROUND-AGENT"
      then (
        incr background;
        check_guidance request 1;
        Some (call_events []))
      else if has_developer request "GUIDANCE-RECIPE-REENTRY"
      then (
        incr reentries;
        check_guidance request 0;
        Some (call_events []))
      else if has_developer request "GUIDANCE-INDEPENDENT-RECIPE"
      then (
        incr recipes;
        check_guidance request 0;
        Some
          (call_events
             (if !recipes = 1
              then
                call
                  "recipe-child"
                  "recipe_reentry"
                  (object_ [ "input", `String "Reenter Agent from excluded recipe." ])
              else [])))
      else if
        String.is_substring (wire_text request) ~substring:"Primary task inside the fork"
      then (
        incr forks;
        check_guidance request 1;
        Some (call_events []))
      else None)
    ~followup_calls:(function
      | 2 ->
        let root = List.hd_exn (array "roots" (complete !inputs "roots")) in
        selected := selection ~kind:"project" ~root ~scope:(`String "sub");
        call "discover" "instructions_refresh" (refresh !selected)
      | 3 ->
        descriptor_ := descriptor !selected (complete !inputs "discover");
        call "read" "instructions_read" !descriptor_
      | 4 ->
        call
          "activate"
          "instructions_activate"
          (activate
             !descriptor_
             (string "capture_id" (complete !inputs "read"))
             ~revision:"0"
             ~key:"scope-selection")
      | 5 ->
        ignore (complete !inputs "activate" : Jsonaf.t);
        call "joined" "review" (object_ [ "input", `String "Inspect as joined Agent." ])
      | 6 ->
        ignore (checked_complete !inputs "joined" : Jsonaf.t);
        call
          "forked"
          "fork"
          (object_ [ "command", `String "Inspect in fork"; "arguments", `Array [] ])
      | 7 ->
        ignore (checked_complete !inputs "forked" : Jsonaf.t);
        call "recipe" "recipe_probe" empty
      | 8 ->
        ignore (checked_complete !inputs "recipe" : Jsonaf.t);
        call "background" "background_probe" empty
      | 9 -> []
      | _ -> failwith "unexpected agent scope transcript")
    (fun state ->
       [%test_eq: int] 1 !nested;
       [%test_eq: int] 1 !forks;
       [%test_eq: int] 2 !recipes;
       [%test_eq: int] 1 !reentries;
       [%test_eq: int] 1 !background;
       let job = List.hd_exn state.jobs in
       assert (Option.is_some job.launch);
       match P.Job.terminal_completion job |> protocol_ok with
       | Some (Succeeded _) ->
         print_endline
           "root, joined Agent, Fork and claimed background Agent guided; independent \
            recipe and native Agent reentry excluded"
       | completion -> raise_s [%sexp (completion : P.Completion.t option)]);
  [%expect
    {| root, joined Agent, Fork and claimed background Agent guided; independent recipe and native Agent reentry excluded |}]
;;

let%expect_test
    "physical daemon reopen retains selected bytes and needs fresh original admission"
  =
  let session_id = ref None in
  let selected = ref empty in
  let descriptor_ = ref empty in
  let inputs = ref [] in
  let current = Atomic.make true in
  let old_actor =
    Operator_authorization.guarded ~principal:(principal ()) ~is_current:(fun () ->
      Atomic.get current)
  in
  let call id name value = [ id, name, value ] in
  with_daemon
    ~request_guidance:true
    ~config_file:"server.sexp"
    ~sources:[ "agent.chatmd", agent; "server.sexp", configuration ]
    ~workspace_files:[ "AGENTS.md", root_guidance; "sub/AGENTS.md", leaf_guidance ]
    ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon ->
      authenticated_connection ~actor:old_actor daemon)
    ~calls:[ "roots", "instructions_refresh", refresh `Null ]
    ~initial_requests:5
    ~expected_requests:5
    ~inspect_request:(fun number request ->
      inputs := request;
      [%test_eq: int]
        (if number = 5 then 1 else 0)
        (List.length (guidance_inputs request)))
    ~followup_calls:(function
      | 2 ->
        let root = List.hd_exn (array "roots" (complete !inputs "roots")) in
        selected := selection ~kind:"project" ~root ~scope:(`String "sub");
        call "discover" "instructions_refresh" (refresh !selected)
      | 3 ->
        descriptor_ := descriptor !selected (complete !inputs "discover");
        call "read" "instructions_read" !descriptor_
      | 4 ->
        call
          "activate"
          "instructions_activate"
          (activate
             !descriptor_
             (string "capture_id" (complete !inputs "read"))
             ~revision:"0"
             ~key:"reopen-selection")
      | 5 ->
        ignore (complete !inputs "activate" : Jsonaf.t);
        []
      | _ -> failwith "unexpected reopen setup request")
    ~after_turn:(fun env handle entry ->
      H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
      unload_idle_runtime env entry.runtime;
      assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime)))
    ~after_shutdown:(fun ~sw ~env ~root ~configuration ->
      (* The first host has actually joined and released its locks. Source bytes
         are gone; only immutable admitted captures can supply the next request. *)
      Atomic.set current false;
      List.iter [ "AGENTS.md"; "sub/AGENTS.md" ] ~f:(fun member ->
        Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / "workspace" / member));
      let calls = ref 0 in
      let failure = ref None in
      let daemon =
        Agent_server.Daemon.start
          ~sw
          ~env
          ~config:configuration
          ~tool_dir:root
          ~home:root
          ~process_start_identity:None
          ~options:
            { Agent_server.Daemon.default_options with
              inference_policy =
                guidance_inference_policy
                  ~default_model:"fixture-model"
                  ~post_stream:(fun ~sw:_ ~inputs ->
                    try
                      incr calls;
                      [%test_eq: int] 1 (List.length (guidance_inputs inputs));
                      let text = wire_text inputs in
                      assert (String.is_substring text ~substring:"ROOT-AGENTS-ORIGINAL");
                      assert (String.is_substring text ~substring:"LEAF-AGENTS-ORIGINAL");
                      call_events []
                    with
                    | exn ->
                      failure := Some exn;
                      raise exn)
                  ()
            }
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
        ~f:(fun () ->
          let id = Option.value_exn !session_id in
          let entry =
            Agent_server.Session_registry.load (Agent_server.Daemon.registry daemon) id
            |> protocol_ok
          in
          assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime));
          let before = A.state entry.actor |> protocol_ok in
          let retained = Option.value_exn before.instruction_selection in
          [%test_eq: int] 2 (List.length (Selection.project retained));
          (match
             Agent_server.Runtime_owner.ensure_loaded_authenticated
               entry.runtime
               ~actor:old_actor
           with
           | Error { code = Permission_denied; _ } -> ()
           | Error error ->
             raise_s [%sexp "unexpected expired reopen admission", (error : P.Error.t)]
           | Ok () -> failwith "expired original reopened runtime");
          [%test_eq: int] 0 !calls;
          assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime));
          let client = authenticated_connection daemon in
          initialize client;
          let handle =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:id
              ~mode:Read_write
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () ->
              H.close handle;
              Agent_client.Connection.close client)
            ~f:(fun () ->
              H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
              H.send_message
                handle
                { kind = Plain_text
                ; text = "Continue from retained guidance."
                ; attachments = []
                }
              |> protocol_ok
              |> ignore;
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let rec wait () =
                  Option.iter !failure ~f:raise;
                  let state = A.state entry.actor |> protocol_ok in
                  if !calls = 1 && Option.is_none state.active_operation
                  then state
                  else (
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                    wait ())
                in
                let after = wait () in
                assert (
                  List.equal
                    (fun a b ->
                       String.equal
                         (Instruction_content.Loaded.contents a)
                         (Instruction_content.Loaded.contents b))
                    (Selection.project retained)
                    (Selection.project (Option.value_exn after.instruction_selection))))));
      print_endline
        "joined host reopened without source files; expired original denied; fresh \
         admission uses exact captured guidance")
    (fun state -> session_id := Some state.identity.session_id);
  [%expect
    {| joined host reopened without source files; expired original denied; fresh admission uses exact captured guidance |}]
;;

let%expect_test "normal completed agent turn transfers guidance to automatic compaction" =
  let inputs = ref [] in
  let selected = ref empty in
  let descriptor_ = ref empty in
  let summaries = ref 0 in
  let before_generation = ref 0 in
  let root_agent =
    agent
    ^ {|
<script id="compaction" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* ignored = Invocation.resolve(p.context.invocation_id, `Complete(`Null)) in
  Task.pure(1)
| `Turn_end ->
  if state == 1 then
    let* () = Runtime.request_compaction() in
    Task.pure(2)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>
<tool name="compact_probe" type="moderator" moderator="compaction" input_schema="object.json" output_schema="any.json"/>|}
  in
  let call id name value = [ id, name, value ] in
  with_daemon
    ~request_guidance:true
    ~runtime_policy:
      { Chat_response.Runtime_semantics.default_policy with
        honor_request_compaction = true
      }
    ~expect_moderator:true
    ~config_file:"server.sexp"
    ~sources:
      [ "agent.chatmd", root_agent
      ; "server.sexp", configuration
      ; "object.json", {|{"type":"object"}|}
      ; "any.json", "true"
      ]
    ~workspace_files:[ "AGENTS.md", root_guidance; "sub/AGENTS.md", leaf_guidance ]
    ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon -> authenticated_connection daemon)
    ~calls:[ "roots", "instructions_refresh", refresh `Null ]
    ~initial_requests:5
    ~expected_requests:7
    ~inspect_request:(fun number request ->
      inputs := request;
      [%test_eq: int]
        (if number >= 5 then 1 else 0)
        (List.length (guidance_inputs request)))
    ~auxiliary_response:(fun request ->
      let text = wire_text request in
      if
        String.is_substring
          text
          ~substring:"your task is to compact it to save on context space"
      then (
        incr summaries;
        [%test_eq: int] 1 (List.length (guidance_inputs request));
        let guidance = Jsonaf.to_string (List.hd_exn (guidance_inputs request)) in
        assert (String.is_substring guidance ~substring:"ROOT-AGENTS-ORIGINAL");
        assert (String.is_substring guidance ~substring:"LEAF-AGENTS-ORIGINAL");
        assert (String.is_substring text ~substring:"<conversation>");
        Some (Authoring_compaction_tests.summary_events ()))
      else None)
    ~followup_calls:(function
      | 2 ->
        let root = List.hd_exn (array "roots" (complete !inputs "roots")) in
        selected := selection ~kind:"project" ~root ~scope:(`String "sub");
        call "discover" "instructions_refresh" (refresh !selected)
      | 3 ->
        descriptor_ := descriptor !selected (complete !inputs "discover");
        call "read" "instructions_read" !descriptor_
      | 4 ->
        call
          "activate"
          "instructions_activate"
          (activate
             !descriptor_
             (string "capture_id" (complete !inputs "read"))
             ~revision:"0"
             ~key:"automatic-compaction-selection")
      | 5 ->
        ignore (complete !inputs "activate" : Jsonaf.t);
        []
      | 6 -> call "compact" "compact_probe" empty
      | 7 ->
        ignore (complete !inputs "compact" : Jsonaf.t);
        []
      | _ -> failwith "unexpected automatic compaction foreground request")
    ~after_turn:(fun env handle entry ->
      let before = A.state entry.actor |> protocol_ok in
      before_generation := before.conversation.compaction_generation;
      H.send_message
        handle
        { kind = Plain_text
        ; text = "Request normal moderator compaction."
        ; attachments = []
        }
      |> protocol_ok
      |> ignore;
      (* This is a real foreground Turn_worker result followed by the Factory's
         compactor. No manual compact command or worker override participates. *)
      let module Log = Agent_session.Durable_event_log in
      let terminals = ref [] in
      let snapshot () = A.state entry.actor |> protocol_ok in
      let rec wait () =
        let changed = Log.changed entry.durable_events in
        let state = snapshot () in
        Option.iter state.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
        let events =
          match
            Log.replay
              entry.durable_events
              ~after_sequence:before.counters.event_sequence
              ~through_sequence:state.counters.event_sequence
          with
          | Snapshot_required -> failwith "automatic compaction event evidence expired"
          | Available events -> events
        in
        terminals
        := List.filter_map events ~f:(fun (event : P.Event.Durable.t) ->
             match
               P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
               |> protocol_ok
             with
             | Operation_completed operation -> Some (`Completed, operation)
             | Operation_failed operation
             | Operation_interrupted operation
             | Operation_cancelled operation -> Some (`Rejected, operation)
             | _ -> None);
        List.iter !terminals ~f:(function
          | `Rejected, operation ->
            raise_s
              [%sexp
                "automatic compaction operation rejected", (operation : P.Operation.t)]
          | `Completed, _ -> ());
        let completed =
          List.exists !terminals ~f:(fun (outcome, operation) ->
            match outcome with
            | `Completed -> P.Operation.equal_kind operation.kind Compaction
            | `Rejected -> false)
        in
        if
          completed
          && Int.equal state.conversation.compaction_generation (!before_generation + 1)
          && Option.is_none state.active_operation
        then ()
        else (
          Eio.Promise.await changed;
          wait ())
      in
      match Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. wait with
      | () -> ()
      | exception exn ->
        let state = snapshot () in
        raise_s
          [%sexp
            "automatic compaction did not reach its actual terminal"
          , (Exn.to_string exn : string)
          , (!summaries : int)
          , (state.active_operation : P.Operation.t option)
          , (state.moderator : Jsonaf.t option)
          , (!terminals : ([ `Completed | `Rejected ] * P.Operation.t) list)])
    ~settle:Authoring_compaction_tests.wait_idle
    (fun state ->
       [%test_eq: int] 1 !summaries;
       [%test_eq: int] (!before_generation + 1) state.conversation.compaction_generation;
       assert (not (List.is_empty state.conversation.compaction_archives));
       [%test_eq: int]
         2
         (List.length (Selection.project (Option.value_exn state.instruction_selection)));
       print_endline
         "normal Turn_worker completion launches one guided compactor and commits its \
          archive");
  [%expect
    {| normal Turn_worker completion launches one guided compactor and commits its archive |}]
;;
