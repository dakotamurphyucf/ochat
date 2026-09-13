open Core
module F = Crash_recovery_fixture
module B = Support.Background_fixture
module C = Support.Config_fixture
module P = Agent_protocol
module I = P.Invocation
module Process = Support.Process_manager

let source =
  {|<developer>Call watch once.</developer>
<tool name="append_to_file"><write path="${workspace}"/></tool>
<script id="watch" language="chatml" kind="tool">
let run ctx input =
  let* result = Tool.call("append_to_file", `Object([
    { key = "path"; value = `String("EFFECT_PATH") },
    { key = "content"; value = `String("executed") }
  ])) in
  match result with
  | `Ok(_) -> Task.pure(`Complete(`String("retained outcome")))
  | `Error(code) -> Task.fail(code)
</script>
<tool name="watch" type="chatml" script="watch" entrypoint="run" input_schema="any.json" output_schema="string.json"><uses tool="append_to_file"/></tool>
|}
;;

let with_host env environment fixture boundary f =
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"invocation-publication"
        ~arguments:[ "notification"; C.config_path fixture; boundary ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "notification-host-ready";
        F.with_client ~sw env fixture (fun client -> f child client)))
;;

let state env fixture session =
  B.checkpoint env fixture session
  |> Option.value_exn ~message:"invocation checkpoint missing"
;;

let root state =
  List.find_exn state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    I.equal_origin invocation.context.origin Model
    && String.equal invocation.context.tool_name "watch")
;;

let outputs entries =
  List.filter entries ~f:(fun entry ->
    P.History.equal_kind entry.P.History.kind Tool_output)
;;

let run env environment boundary =
  let fixture = F.fixture env environment boundary in
  let marker = Filename.concat (C.physical_workspace fixture) "invocation.effect" in
  F.write
    env
    (C.prompt_path fixture)
    (String.substr_replace_all
       source
       ~pattern:"\"EFFECT_PATH\""
       ~with_:(Jsonaf.to_string (`String marker)));
  let directory = Filename.dirname (C.prompt_path fixture) in
  F.write env (Filename.concat directory "any.json") "true";
  F.write env (Filename.concat directory "string.json") {|{"type":"string"}|};
  F.write
    env
    (C.config_path fixture)
    (C.configuration fixture ()
     |> String.substr_replace_all
          ~pattern:"(tool_default deny)"
          ~with_:
            (if String.equal boundary "invocation-permission"
             then "(tool_default ask)"
             else "(tool_default allow)"));
  let expected_effect =
    if String.equal boundary "invocation-resolved" then "\nexecuted" else ""
  in
  let check_effect () =
    let actual = if Eio.Path.is_file (F.path env marker) then F.read env marker else "" in
    F.require_equal
      "exact original handler effect"
      [%sexp_of: string]
      expected_effect
      actual
  in
  let session, before =
    with_host env environment fixture boundary (fun child client ->
      let session = B.create client "invocation:create" in
      ignore
        (F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content = { kind = Plain_text; text = "Call watch."; attachments = [] }
              ; idempotency_key = F.key "invocation:send"
              })
         : P.Method_result.t);
      (match boundary with
       | "invocation-permission" ->
         let waiting =
           B.await_snapshot env client session "outer handler approval" (fun snapshot ->
             List.exists snapshot.permissions ~f:(fun permission ->
               String.equal permission.tool_name "watch"
               && P.Permission.equal_state permission.state Pending))
         in
         let permission =
           List.find_exn waiting.permissions ~f:(fun permission ->
             String.equal permission.tool_name "watch"
             && P.Permission.equal_state permission.state Pending)
         in
         ignore
           (F.request
              client
              (Permission_respond
                 { session_id = session.summary.id
                 ; attachment_id = session.attachment_id
                 ; permission_id = permission.id
                 ; permission_generation = permission.generation
                 ; choice = Approve_once
                 ; reason = Some "Allow handler to reach nested approval"
                 ; idempotency_key = F.key "invocation:approve-owner"
                 })
            : P.Method_result.t)
       | _ -> ());
      F.await_marker env child ("notification-boundary " ^ boundary);
      F.kill env child;
      let before = state env fixture session in
      let invocation = root before in
      (match boundary, invocation.status with
       | "invocation-admitted", Admitted -> ()
       | "invocation-resolved", Resolved (Complete (`String "retained outcome")) -> ()
       | "invocation-permission", Dispatching ->
         F.require
           (List.exists before.permissions ~f:(fun permission ->
              String.equal permission.tool_name "append_to_file"
              && P.Permission.equal_state permission.state Pending))
           "nested approval was not durably pending"
       | _ -> F.fail "selected invocation crash boundary was missed");
      F.require
        (List.is_empty (outputs before.conversation.canonical_history))
        "initial response was published before crash";
      F.require
        (Option.is_none invocation.output_entry_id)
        "unpublished invocation has an output identity";
      check_effect ();
      session, before)
  in
  let original = root before in
  let previous = ref None in
  for reopen = 1 to 2 do
    with_host env environment fixture "recover" (fun child client ->
      let snapshot =
        B.await_snapshot
          env
          client
          session
          "recovered invocation publication"
          (fun snapshot ->
             Option.is_none snapshot.session.active_operation
             && List.length (outputs snapshot.canonical_history.entries) = 1)
      in
      (match boundary with
       | "invocation-permission" ->
         let pending =
           List.find_exn before.permissions ~f:(fun permission ->
             String.equal permission.tool_name "append_to_file")
         in
         let cancelled =
           List.find_exn snapshot.permissions ~f:(fun permission ->
             P.Id.Permission.equal pending.id permission.id)
         in
         F.require
           (P.Permission.equal_state cancelled.state Cancelled)
           "restart retained a live approval continuation";
         let attached, _ =
           B.attach client session.summary (sprintf "late-approval:%d" reopen)
         in
         (match
            Support.Http_driver.request
              client
              (Permission_respond
                 { session_id = session.summary.id
                 ; attachment_id = attached.attachment_id
                 ; permission_id = pending.id
                 ; permission_generation = pending.generation
                 ; choice = Approve_once
                 ; reason = None
                 ; idempotency_key = F.key (sprintf "late-approval:%d" reopen)
                 })
          with
          | Error _ -> ()
          | Ok _ -> F.fail "late approval revived an interrupted native call")
       | _ -> ());
      for _ = 1 to 10 do
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.03;
        F.require
          (not
             (String.is_substring
                (Process.stdout child).contents
                ~substring:"notification-provider "))
          "invocation recovery called the provider";
        check_effect ()
      done;
      F.kill env child;
      let recovered = state env fixture session in
      let invocation = root recovered in
      F.require
        (I.equal_context original.context invocation.context)
        "recovery changed the invocation context";
      let outcome =
        match invocation.status with
        | Published outcome -> outcome
        | _ -> F.fail "recovery did not durably publish the initial response"
      in
      (match boundary, outcome with
       | ("invocation-admitted" | "invocation-permission"), Cancelled reason ->
         F.require (not (String.is_empty reason)) "interruption lacks reason"
       | "invocation-resolved", Complete (`String "retained outcome") -> ()
       | _ -> F.fail "recovery changed the stored outcome or invented success");
      F.require
        (List.length before.invocations = List.length recovered.invocations)
        "recovery ran a new handler invocation";
      F.require
        (List.is_empty recovered.deliveries && List.is_empty recovered.jobs)
        "invocation recovery fabricated background work";
      let entries = recovered.conversation.canonical_history in
      let output = List.hd_exn (outputs entries) in
      F.require
        (Option.equal P.History.Id.equal invocation.output_entry_id (Some output.id))
        "publication receipt refers to another history item";
      F.require_equal
        "live and saved response"
        [%sexp_of: P.History.entry list]
        snapshot.canonical_history.entries
        entries;
      (match !previous with
       | None -> previous := Some entries
       | Some expected ->
         F.require_equal
           "stable response after second daemon"
           [%sexp_of: P.History.entry list]
           expected
           entries);
      Agent_session.Invocation_history.validate_retained ~history:entries invocation
      |> F.protocol_ok;
      check_effect ())
  done
;;

let test env environment =
  List.iter
    [ "invocation-admitted"; "invocation-resolved"; "invocation-permission" ]
    ~f:(run env environment)
;;
