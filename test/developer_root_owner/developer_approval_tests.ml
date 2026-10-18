open! Core
module P = Agent_protocol
module S = Agent_server
module R = Agent_store.Developer_command_receipt
module D = S.Daemon
module Pending = D.Developer.Approval.Pending

let ok = Developer_host_tests.ok
let checked = Developer_host_tests.checked

let principal name =
  let scopes = Set.add Agent_server_test_support.scopes P.Scope.Developer_live_run in
  Agent_server_test_support.principal_with_scopes name scopes
;;

let same_actor actor (principal : P.Principal.t) =
  P.Id.Principal.equal (Operator_authorization.principal actor).id principal.id
;;

let limits () =
  (* The serialized request includes the bounded command and manifest context. *)
  D.Developer_configuration.Shell_approvals.Limits.create
    ~max_pending:2
    ~max_request_bytes:(32 * 1024)
    ~max_display_bytes:512
    ~max_decision_reason_bytes:128
    ~wait_seconds:10.
  |> ok
;;

let shell_prompt =
  {|<developer>Approval boundary fixture.</developer>
<shell_access id="approval" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <write path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny" scopes="once" durable="false"/>
  <audit format="none"/>
</shell_access>
<tool name="approval_touch" type="shell" mode="fixed" runtime="approval"
      result="stdout">
  <command program="/usr/bin/touch">
    <path_arg base="workspace" path="approval-marker"/>
  </command>
  <arguments mode="none"/>
</tool>|}
;;

let script =
  {|let main input =
  Task.bind(Tool.call("approval_touch", `Object([])), fun result ->
    match result with
    | `Ok(value) -> Task.pure(value)
    | `Error(message) -> Task.fail(message))|}
;;

let ready_artifact daemon =
  let entry =
    Agent_session.Prompt_catalog.find_by_name (D.prompts daemon) "restart.prompt"
    |> Option.value_exn
  in
  match entry.availability with
  | Ready revision ->
    let artifact = Agent_session.Prompt_revision.artifact revision in
    artifact.root_sha256, Option.value_exn artifact.shell_manifest_sha256
  | Disabled | Unavailable _ -> failwith "approval prompt did not compile"
;;

let start_daemon ~sw ~env ~root ~config ~developer =
  D.start
    ~sw
    ~env
    ~config
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ~options:
      { D.default_options with
        authoring = Some (Developer_host_tests.semantic_authoring_policy env)
      ; developer
      }
    ()
  |> ok
;;

let wait_pending env daemon ~actor ~run_id ~key ~marker ~authorization_attempts =
  let last_error = ref None in
  let last_phase = ref None in
  let inspect_command () =
    match D.Developer.observe daemon ~actor ~run_id ~key with
    | Error error ->
      raise_s [%sexp "approval command observation failed", (error : P.Error.t)]
    | Ok None -> failwith "approval command disappeared while awaiting reviewer"
    | Ok (Some current) ->
      let phase = D.Developer.View.phase current in
      last_phase
      := Some
           (match phase with
            | Agent_store.Developer_command_row.Phase.Reserved -> "reserved"
            | Waiting_permission -> "waiting_permission"
            | Execution_started -> "execution_started"
            | Terminal _ -> "terminal");
      (match phase with
       | Agent_store.Developer_command_row.Phase.Terminal _ ->
         let receipt = Option.map (D.Developer.View.receipt current) ~f:R.value in
         let marker_exists = Eio.Path.is_file marker in
         raise_s
           [%sexp
             "approval command ended before reviewer"
           , (receipt : R.value option)
           , (marker_exists : bool)
           , (!authorization_attempts : int)]
       | Reserved | Waiting_permission | Execution_started -> ())
  in
  try
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      let rec loop () =
        match D.Developer.Approval.pending daemon ~actor ~run_id with
        | Ok [ pending ] -> pending
        | Ok [] ->
          inspect_command ();
          Eio.Fiber.yield ();
          loop ()
        | Error ({ P.Error.code = Invalid_request; _ } as error) ->
          last_error := Some error;
          inspect_command ();
          Eio.Fiber.yield ();
          loop ()
        | Ok _ -> failwith "approval fixture has more than one pending request"
        | Error error -> raise_s [%sexp "approval pending failed", (error : P.Error.t)]
      in
      loop ())
  with
  | Eio.Time.Timeout ->
    raise_s
      [%sexp
        "approval pending timed out"
      , (!last_error : P.Error.t option)
      , (!last_phase : string option)]
;;

let request ~expected ~input =
  let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
  let bounds =
    Chatml_developer_capture.Json_bounds.create
      ~max_bytes:4096
      ~max_depth:16
      ~max_nodes:128
    |> Or_error.ok_exn
  in
  D.Developer.Request.create
    ~route:(Document_script { expected; selector })
    ~kind:Live_run
    ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
    ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
    ~workspace:(Some (S.Catalog_identity.workspace_definition "restart.workspace"))
    ~entrypoint:"main"
    ~input:(`String input)
    ~fixtures:None
    ~requested_execution:None
    ~requested_max_output_bytes:None
    ~bounds
    ~checkpoint:ignore
  |> ok
;;

let with_shell_daemon f =
  Eio_main.run (fun env ->
    let root =
      Filename.concat
        "/tmp"
        ("ochat-developer-approval-" ^ P.Id.Transaction.(create () |> to_string))
    in
    let root_path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 root_path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
      ~f:(fun () ->
        let prompt_file = Filename.concat root "prompt.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          shell_prompt;
        let owner_principal = principal "pri_developer_approval_owner" in
        let reader_principal =
          let scopes = Set.remove owner_principal.scopes P.Scope.Answer_approvals in
          Agent_server_test_support.principal_with_scopes
            "pri_developer_approval_owner"
            scopes
        in
        let foreign_principal = principal "pri_developer_approval_foreign" in
        let owner_current = ref true in
        let owner =
          Operator_authorization.guarded ~principal:owner_principal ~is_current:(fun () ->
            !owner_current)
        in
        let reader = Operator_authorization.trusted_local reader_principal in
        let foreign = Operator_authorization.trusted_local foreign_principal in
        let profile =
          S.Config.Permission_profile.
            { Agent_server_test_support.permission_profile with
              tool_default = Ask
            ; manifest_authorization = Require_grant
            }
        in
        let base_config =
          Agent_server_test_support.config ~profile root root prompt_file
        in
        let source_sha256, manifest_sha256 =
          Eio.Switch.run (fun sw ->
            let daemon =
              start_daemon ~sw ~env ~root ~config:base_config ~developer:None
            in
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> ok)
              ~f:(fun () -> ready_artifact daemon))
        in
        let grant =
          S.Config.Manifest_grant.
            { id = "developer-approval-fixture"
            ; prompt = "restart.prompt"
            ; workspaces = [ "restart.workspace" ]
            ; manifest_sha256
            ; source_sha256
            ; principals = [ P.Id.Principal.to_string owner_principal.id ]
            }
        in
        let config = { base_config with manifest_grants = [ grant ] } in
        let authorization_attempts = ref 0 in
        let authorize_native invocation binding =
          let reference = Chat_response.Tool_capability.reference binding in
          [%test_eq: string] reference.name "approval_touch";
          assert (
            P.Invocation.equal_origin
              (P.Host_invocation.context invocation).origin
              P.Invocation.Script);
          Int.incr authorization_attempts;
          Ok ()
        in
        let developer =
          Developer_host_tests.developer_configuration
            ~authorize_native
            ~prepare_output:(function
              | Openai.Responses.Tool_output.Output.Text text ->
                (* This host policy turns a shell Text error envelope into a Tool error. *)
                (match Jsonaf.parse text with
                 | Ok (`Object fields)
                   when List.exists fields ~f:(fun (key, _) -> String.equal key "error")
                   -> Error (P.Error.invalid_request "shell tool reported an error")
                 | Ok _ | Error _ -> Ok (`String text))
              | _ -> Error (P.Error.invalid_request "unexpected approval output"))
            ~receipt_read:(fun actor _ -> same_actor actor owner_principal)
            ~command_control:(fun actor _ ->
              same_actor actor owner_principal
              && P.Principal.has_scope
                   (Operator_authorization.principal actor)
                   P.Scope.Answer_approvals)
            ()
        in
        let developer =
          D.Developer_configuration.with_shell_approvals
            developer
            (D.Developer_configuration.Shell_approvals.create
               ~limits:(limits ())
               ~job_read:(fun _ _ -> false)
               ~job_respond:(fun _ _ -> false))
        in
        let run_phase g =
          Eio.Switch.run (fun sw ->
            let daemon =
              start_daemon ~sw ~env ~root ~config ~developer:(Some developer)
            in
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> ok)
              ~f:(fun () -> g daemon))
        in
        f
          ~env
          ~root
          ~owner
          ~reader
          ~foreign
          ~owner_current
          ~manifest_sha256
          ~authorization_attempts
          ~run_phase))
;;

let assert_succeeded view expected =
  let actual = Option.map (D.Developer.View.receipt view) ~f:R.value in
  match actual with
  | Some (Live (Succeeded (`String value))) when String.equal value expected -> ()
  | _ -> raise_s [%sexp "unexpected approval success", (actual : R.value option)]
;;

let assert_cancelled view =
  let actual = Option.map (D.Developer.View.receipt view) ~f:R.value in
  match actual with
  | Some (Live Cancelled) -> ()
  | _ -> raise_s [%sexp "unexpected approval cancellation", (actual : R.value option)]
;;

let assert_native_pending ~manifest_sha256 ~submitted pending =
  (match Pending.owner pending with
   | Command receipt_id -> assert (R.Id.equal receipt_id (D.Developer.View.id submitted))
   | Claimed_job _ -> failwith "ordinary Live approval belonged to a claimed Job");
  let invocation =
    match Pending.execution pending with
    | Native_invocation id -> id
    | Claimed_model_job _ -> failwith "ordinary Live approval claimed Model origin"
  in
  [%test_eq: string] (Pending.runtime_id pending) "approval";
  [%test_eq: string] (Pending.manifest_sha256 pending) manifest_sha256;
  assert (not (String.is_empty (Pending.redacted_display pending)));
  invocation
;;

let%expect_test "hosted Developer shell approval waits keep exact native custody" =
  with_shell_daemon
    (fun
        ~env
         ~root
         ~owner
         ~reader
         ~foreign
         ~owner_current
         ~manifest_sha256
         ~authorization_attempts
         ~run_phase
       ->
       let marker = Eio.Path.(Eio.Stdenv.fs env / root / "approval-marker") in
       let run_id = P.Id.Developer_run.create () in
       let revoked_run_id = P.Id.Developer_run.create () in
       let native_invocations = ref [] in
       let record_pending submitted pending =
         let invocation = assert_native_pending ~manifest_sha256 ~submitted pending in
         assert (
           not
             (List.exists !native_invocations ~f:(fun prior ->
                P.Id.Invocation.equal prior invocation)));
         native_invocations := invocation :: !native_invocations
       in
       let revoked_key =
         P.Idempotency_key.of_string "approval-revoked-original" |> checked
       in
       let submit daemon ~expected ~connection ~name =
         let key = P.Idempotency_key.of_string name |> checked in
         let submitted =
           D.Developer.submit
             daemon
             ~actor:owner
             ~run_id
             ~key
             ~request:(request ~expected ~input:"approval")
             ~fresh_source:(Some (Document_script { connection }))
           |> ok
         in
         let pending =
           wait_pending
             env
             daemon
             ~actor:owner
             ~run_id
             ~key
             ~marker
             ~authorization_attempts
         in
         record_pending submitted pending;
         let current =
           D.Developer.observe daemon ~actor:owner ~run_id ~key |> ok |> Option.value_exn
         in
         assert (R.Id.equal (D.Developer.View.id submitted) (D.Developer.View.id current));
         (match D.Developer.View.phase current with
          | Agent_store.Developer_command_row.Phase.Execution_started -> ()
          | _ -> failwith "approval did not retain durable command Start");
         key, pending, current
       in
       let revoked_receipt =
         run_phase (fun daemon ->
           let connection, _transport, expected =
             Developer_host_tests.open_document ~source:script daemon owner
           in
           let approve_key, approve_pending, approved_start =
             submit daemon ~expected ~connection ~name:"approval-approve-once"
           in
           let read_only =
             D.Developer.Approval.pending daemon ~actor:reader ~run_id |> ok
           in
           assert (List.length read_only = 1);
           assert (
             P.Id.Permission.equal
               (Pending.id (List.hd_exn read_only))
               (Pending.id approve_pending));
           (match
              D.Developer.Approval.respond
                daemon
                ~actor:reader
                ~run_id
                ~id:(Pending.id approve_pending)
                Approve_once
            with
            | Error { P.Error.code = Permission_denied; _ } -> ()
            | Ok () | Error _ -> failwith "reader could answer approval");
           (match D.Developer.Approval.pending daemon ~actor:foreign ~run_id with
            | Error { P.Error.code = Permission_denied; _ } -> ()
            | Ok _ | Error _ -> failwith "foreign actor could read approval");
           D.Developer.Approval.respond
             daemon
             ~actor:owner
             ~run_id
             ~id:(Pending.id approve_pending)
             Approve_once
           |> ok;
           let approved =
             D.Developer.await_terminal daemon ~actor:owner ~run_id ~key:approve_key |> ok
           in
           assert_succeeded approved "";
           assert (Eio.Path.is_file marker);
           Eio.Path.unlink marker;
           assert (
             List.is_empty (D.Developer.Approval.pending daemon ~actor:owner ~run_id |> ok));
           (match
              D.Developer.Approval.respond
                daemon
                ~actor:owner
                ~run_id
                ~id:(Pending.id approve_pending)
                Approve_once
            with
            | Error { P.Error.code = Lease_stale; _ } -> ()
            | Ok () | Error _ -> failwith "approval answer was accepted twice");
           let deny_key, deny_pending, denied_start =
             submit daemon ~expected ~connection ~name:"approval-denied"
           in
           assert (
             P.Execution_owner.equal_developer
               (D.Developer.View.owner approved_start)
               (D.Developer.View.owner denied_start));
           D.Developer.Approval.respond
             daemon
             ~actor:owner
             ~run_id
             ~id:(Pending.id deny_pending)
             (Deny "fixture denial")
           |> ok;
           let denied =
             D.Developer.await_terminal daemon ~actor:owner ~run_id ~key:deny_key |> ok
           in
           (match Option.map (D.Developer.View.receipt denied) ~f:R.value with
            | Some (Live (Failed error)) ->
              [%test_eq: P.Error.code] error.code Invalid_request
            | result ->
              raise_s [%sexp "unexpected denied approval", (result : R.value option)]);
           assert (not (Eio.Path.is_file marker));
           let cancel_key, _cancel_pending, cancelled_start =
             submit daemon ~expected ~connection ~name:"approval-cancelled"
           in
           assert (
             P.Execution_owner.equal_developer
               (D.Developer.View.owner approved_start)
               (D.Developer.View.owner cancelled_start));
           let cancelled =
             D.Developer.cancel_and_wait daemon ~actor:owner ~run_id ~key:cancel_key |> ok
           in
           assert_cancelled cancelled;
           assert (
             List.is_empty (D.Developer.Approval.pending daemon ~actor:owner ~run_id |> ok));
           assert (not (Eio.Path.is_file marker));
           let submitted =
             D.Developer.submit
               daemon
               ~actor:owner
               ~run_id:revoked_run_id
               ~key:revoked_key
               ~request:(request ~expected ~input:"approval")
               ~fresh_source:(Some (Document_script { connection }))
             |> ok
           in
           let revoked_pending =
             wait_pending
               env
               daemon
               ~actor:owner
               ~run_id:revoked_run_id
               ~key:revoked_key
               ~marker
               ~authorization_attempts
           in
           record_pending submitted revoked_pending;
           D.Developer.Approval.respond
             daemon
             ~actor:owner
             ~run_id:revoked_run_id
             ~id:(Pending.id revoked_pending)
             Approve_once
           |> ok;
           (* Promise resolution and this revocation have no intervening yield. *)
           owner_current := false;
           let stopped =
             D.Developer.await_terminal
               daemon
               ~actor:reader
               ~run_id:revoked_run_id
               ~key:revoked_key
             |> ok
           in
           (match Option.map (D.Developer.View.receipt stopped) ~f:R.value with
            | Some (Live (Failed error)) ->
              [%test_eq: P.Error.code] error.code Invalid_state
            | result ->
              raise_s
                [%sexp "unexpected post-approval revocation", (result : R.value option)]);
           assert (
             R.Id.equal (D.Developer.View.id submitted) (D.Developer.View.id stopped));
           assert (not (Eio.Path.is_file marker));
           D.Developer.View.id stopped)
       in
       run_phase (fun reopened ->
         let refreshed =
           Operator_authorization.trusted_local (Operator_authorization.principal owner)
         in
         let observed =
           D.Developer.observe
             reopened
             ~actor:refreshed
             ~run_id:revoked_run_id
             ~key:revoked_key
           |> ok
           |> Option.value_exn
         in
         assert (R.Id.equal revoked_receipt (D.Developer.View.id observed));
         (match Option.map (D.Developer.View.receipt observed) ~f:R.value with
          | Some (Live (Failed error)) ->
            [%test_eq: P.Error.code] error.code Invalid_state
          | result ->
            raise_s
              [%sexp "unexpected reopened approval receipt", (result : R.value option)]);
         assert (
           match
             D.Developer.Approval.pending reopened ~actor:refreshed ~run_id:revoked_run_id
           with
           | Error { P.Error.code = Invalid_request; _ } -> true
           | Ok _ | Error _ -> false);
         assert (not (Eio.Path.is_file marker));
         D.Developer.View.id observed)
       |> ignore;
       [%test_eq: int] (List.length !native_invocations) 4;
       printf "approved=1 denied=1 cancelled=1 post-wait-revoked=1\n");
  [%expect {| approved=1 denied=1 cancelled=1 post-wait-revoked=1 |}]
;;
