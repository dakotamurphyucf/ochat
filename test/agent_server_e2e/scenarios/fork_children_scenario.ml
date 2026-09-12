open Core
module F = Shell_security_scenario
module P = Agent_protocol
module I = P.Invocation

let extensions ~deny =
  sprintf
    {|
<script id="deterministic" language="chatml" kind="tool">
let run ctx input = Task.pure(`Complete(`String("script-result")))
</script>
<tool name="deterministic" type="chatml" script="deterministic" entrypoint="run" input_schema="fork-input.json" output_schema="fork-output.json"/>
<script id="coordinator" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(call) ->
  let* () = if %s then
    (if Tool_call.is_named(call, "fixed_echo") then Tool.reject("child denied by parent")
     else if Tool_call.is_named(call, "stateful") then Tool.reject("handler denied by parent") else Tool.approve())
    else Tool.approve() in
  Task.pure(state)
| `Tool_invoked(p) ->
  let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("ghost-result"))) in
  Task.pure(state + 1)
| _ -> Task.pure(state)
</script>
<tool name="stateful" type="moderator" moderator="coordinator" input_schema="fork-input.json" output_schema="fork-output.json"/>
|}
    (Bool.to_string deny)
;;

let test env environment ~deny =
  let name = if deny then "recursive-denied" else "recursive-allowed" in
  let fixture = F.live_fixture env environment name in
  let prompt_path = Support.Config_fixture.prompt_path fixture in
  F.save environment prompt_path (F.live_prompt () ^ extensions ~deny);
  List.iter
    [ "fork-input.json", "true"; "fork-output.json", {|{"type":"string"}|} ]
    ~f:(fun (name, contents) ->
      F.save environment (Filename.concat (Filename.dirname prompt_path) name) contents);
  let calls = ref 0 in
  let requests = ref [] in
  let model_post_stream ~sw:_ ~inputs =
    let index = !calls in
    incr calls;
    requests := inputs :: !requests;
    let events =
      match index with
      | 0 | 1 ->
        F.live_tool_stream "fork" {|{"command":"nested work","arguments":[]}|} index
      | 2 -> F.live_tool_stream "deterministic" "{}" index
      | 3 -> F.live_tool_stream "stateful" "{}" index
      | 4 ->
        F.live_tool_stream "fixed_echo" {|{"arguments":["child-native-result"]}|} index
      | _ -> F.message_stream index
    in
    Stdlib.List.to_seq events
  in
  let options =
    { Agent_server.Daemon.default_options with
      model_post_stream = Some model_post_stream
    }
  in
  Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
    F.with_client ~sw env fixture (fun client ->
      let session = F.create_session client (name ^ ":create") in
      let session = F.start_session client session (name ^ ":start") in
      let events = F.capture_live ~sw env client session ~nested:false in
      let entry =
        Agent_server.Session_registry.find
          (Agent_server.Daemon.registry daemon)
          session.summary.id
        |> Option.value_exn
      in
      let state = Agent_session.Session_actor.state entry.actor |> F.protocol_ok in
      if not (Int.equal !calls 8)
      then
        raise_s
          [%sexp
            "recursive child did not resume both parents"
          , (!calls : int)
          , (List.map state.invocations ~f:(fun invocation ->
               invocation.context.tool_name, invocation.status)
             : (string * I.status) list)];
      F.require
        (Int.equal (List.length state.invocations) 5)
        "fork descendant invocation records missing or duplicated";
      let root =
        List.find_exn state.invocations ~f:(fun invocation ->
          I.equal_origin invocation.context.origin Model)
      in
      List.iter state.invocations ~f:(fun invocation ->
        match invocation.context.origin with
        | Model ->
          F.require
            (String.equal invocation.context.tool_name "fork")
            "child call leaked into model-owned history"
        | Delegated_agent ->
          F.require
            (Option.is_none invocation.context.provider_call_id
             && Option.is_none invocation.context.call_entry_id)
            "child borrowed root provider history";
          let parent = Option.value_exn invocation.context.parent_invocation in
          F.require
            (List.exists state.invocations ~f:(fun i ->
               P.Id.Invocation.equal i.context.id parent
               && String.equal i.context.tool_name "fork"))
            "child lost its immediate fork owner"
        | _ -> F.fail "unexpected fork child invocation origin");
      List.iter
        [ "deterministic", "script-result"; "stateful", "ghost-result" ]
        ~f:(fun (name, expected) ->
          let invocation =
            List.find_exn state.invocations ~f:(fun i ->
              String.equal i.context.tool_name name)
          in
          match invocation.status with
          | Resolved (Fail { code = "invocation.pre_tool_rejected"; _ })
            when deny && String.equal name "stateful" -> ()
          | Resolved (Complete (`String actual))
            when not (deny && String.equal name "stateful") ->
            F.require (String.equal actual expected) "managed child result changed"
          | _ ->
            raise_s [%sexp "managed fork child failed", (invocation.status : I.status)]);
      let native =
        List.find_exn state.invocations ~f:(fun i ->
          String.equal i.context.tool_name "fixed_echo")
      in
      (match deny, native.status with
       | false, Resolved (Complete (`String text)) ->
         F.require
           (String.is_substring text ~substring:"child-native-result")
           "native child output missing"
       | true, Resolved (Fail { code = "invocation.pre_tool_rejected"; _ }) -> ()
       | _ ->
         raise_s
           [%sexp "fork native permission/result mismatch", (native.status : I.status)]);
      (match root.status with
       | Published (Complete _) -> ()
       | _ -> F.fail "root fork did not publish its final result");
      let root_request =
        List.hd_exn !requests
        |> List.map ~f:Openai.Responses.Item.jsonaf_of_t
        |> fun items -> Jsonaf.to_string (`Array items)
      in
      F.require
        (not (String.is_substring root_request ~substring:"script-result"))
        "child transcript leaked into parent model request";
      match deny with
      | false -> F.require_live_tool_events events true
      | true ->
        F.require
          (not (List.exists events ~f:F.is_nested_tool_start))
          "rejected child reached native driver"))
;;

let run env ~case =
  Support.Temporary_environment.with_ ~scenario:"fork-children" ~env (fun environment ->
    let modes =
      match case with
      | None -> [ false; true ]
      | Some "recursive-allowed" -> [ false ]
      | Some "recursive-denied" -> [ true ]
      | Some _ -> invalid_arg "unknown fork-children case"
    in
    List.iter modes ~f:(fun deny -> test env environment ~deny);
    Eio.Flow.copy_string "fork-children: passed\n" (Eio.Stdenv.stdout env))
;;
