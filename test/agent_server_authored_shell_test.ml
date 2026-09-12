open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module Revision = Agent_session.Prompt_revision
module Source = Agent_session.Authored_agent_source
module H = Agent_client.Session_handle
module Res = Openai.Responses

let parent_text =
  {|<developer>AUTHORED_SHELL_PARENT</developer><tool name="researcher" agent="child.chatmd" local persistence="optional"/>|}
;;

let child_text =
  {|<developer>AUTHORED_SHELL_CHILD</developer>
<shell_access id="direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false" child_processes="false" arbitrary_code="false" privilege_change="false">
    <read path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny" scopes="once,exact_session"/>
  <audit format="none"/>
</shell_access>
<tool name="fixed_echo" type="shell" mode="fixed" runtime="direct" command="/bin/echo authored-shell" result="stdout"/>|}
;;

let with_sources f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        List.iter
          [ "parent.chatmd", parent_text; "child.chatmd", child_text ]
          ~f:(fun (name, text) ->
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / root / name)
              text);
        f env root))
;;

let with_daemon env root configuration principal provider f =
  Eio.Switch.run (fun sw ->
    let daemon =
      D.start
        ~sw
        ~env
        ~config:configuration
        ~tool_dir:root
        ~home:root
        ~process_start_identity:None
        ~options:
          { D.default_options with
            qualify_chatml_extensions = true
          ; model_post_stream = Some provider
          }
        ()
      |> protocol_ok
    in
    Exn.protect
      ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
      ~f:(fun () ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
          let client = connection daemon principal in
          Exn.protect
            ~finally:(fun () -> Agent_client.Connection.close client)
            ~f:(fun () ->
              initialize client;
              f sw daemon client))))
;;

let revision daemon =
  let entry =
    Agent_session.Prompt_catalog.find_by_name (D.prompts daemon) "restart.prompt"
    |> Option.value_exn
  in
  match entry.availability with
  | Ready revision -> revision
  | _ -> failwith "authored shell prompt was unavailable"
;;

let function_call serial name arguments =
  let call_id = sprintf "authored-shell-%d" serial in
  let open Res.Response_stream in
  [ Output_item_added
      { item =
          Function_call
            { name
            ; arguments = ""
            ; call_id
            ; _type = "function_call"
            ; id = Some call_id
            ; status = None
            }
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = Jsonaf.to_string arguments
      ; item_id = call_id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let%expect_test
    "authored shell approval belongs to the child and cannot borrow a parent's responder"
  =
  with_sources (fun env root ->
    let profile = { permission_profile with tool_default = Ask } in
    let configuration =
      config ~profile root root (Filename.concat root "parent.chatmd")
    in
    let requests = ref 0 in
    let mode = ref "persistent" in
    let preflight = ref (fun () -> ()) in
    let provider ~sw:_ ~inputs =
      Int.incr requests;
      match List.last inputs with
      | Some (Res.Item.Function_call_output _) -> Stdlib.Seq.empty
      | _ ->
        let child =
          List.exists inputs ~f:(function
            | Res.Item.Input_message message ->
              let json = Res.Item.jsonaf_of_t (Input_message message) in
              String.equal
                (Jsonaf.member_exn "role" json |> Jsonaf.string_exn)
                "developer"
              && String.is_substring
                   (Jsonaf.to_string json)
                   ~substring:"AUTHORED_SHELL_CHILD"
            | _ -> false)
        in
        (match child with
         | true ->
           let before = !preflight in
           (preflight := fun () -> ());
           before ();
           function_call !requests "fixed_echo" (`Object [])
         | false ->
           function_call
             !requests
             "researcher"
             (`Object [ "input", `String "Run the shell tool."; "mode", `String !mode ]))
    in
    let principal = principal () in
    with_daemon env root configuration principal provider (fun sw daemon client ->
      let state entry = A.state entry.R.actor |> protocol_ok in
      let parent, _ = create_session ~start_immediately:true client in
      let parent_entry = R.load (D.registry daemon) parent.id |> protocol_ok in
      let attach connection session_id =
        H.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection
          ~session_id
          ~mode:Read_write
          ~subscribe:false
          ()
        |> protocol_ok
      in
      let parent_handle = attach client parent.id in
      let send handle =
        H.send_message
          handle
          { kind = Plain_text; text = "Run the tool."; attachments = [] }
        |> protocol_ok
        |> ignore
      in
      let rec pending entry =
        let current = state entry in
        match
          List.find current.permissions ~f:(fun p ->
            P.Permission.equal_state p.state Pending)
        with
        | Some permission -> permission
        | None ->
          (match current.active_operation with
           | None -> failwith "authored call ended without expected approval"
           | Some _ ->
             Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
             pending entry)
      in
      let approve handle permission choice =
        H.respond_permission
          handle
          ~permission_id:permission.P.Permission.id
          ~permission_generation:permission.generation
          ~choice
          ~reason:None
      in
      let rec idle entry =
        match (state entry).active_operation with
        | None -> state entry
        | Some _ ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          idle entry
      in
      let records () =
        Agent_store.Delegation_store.with_records
          (Agent_store.Session_store.delegations (D.store daemon))
          ~max_records:128
          ~max_bytes:1048576
          ~f:(fun records -> Ok records)
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
        |> protocol_ok
      in
      let create () =
        let before = records () in
        let reached, reached_u = Eio.Promise.create () in
        let ready, ready_u = Eio.Promise.create () in
        (preflight
         := fun () ->
              Eio.Promise.resolve reached_u ();
              Eio.Promise.await ready);
        send parent_handle;
        let permission = pending parent_entry in
        [%test_eq: string] "researcher" permission.tool_name;
        approve parent_handle permission Approve_once |> protocol_ok |> ignore;
        Eio.Promise.await reached;
        let record =
          records ()
          |> List.filter ~f:(fun record ->
            not
              (List.exists before ~f:(fun old ->
                 P.Id.Session.equal
                   old.Agent_store.Delegation_store.admission.child_session_id
                   record.admission.child_session_id)))
          |> function
          | [ record ] -> record
          | _ -> failwith "expected one new authored child"
        in
        let entry =
          R.load (D.registry daemon) record.admission.child_session_id |> protocol_ok
        in
        entry, fun () -> Eio.Promise.resolve ready_u ()
      in
      let child, release = create () in
      let child_id = (state child).identity.session_id in
      let handle = attach client child_id in
      release ();
      let permission = pending child in
      assert (P.Id.Session.equal permission.session_id child_id);
      [%test_eq: string] "shell:direct" permission.tool_name;
      (match permission.owner with
       | Invocation id ->
         assert (
           List.exists (state child).invocations ~f:(fun invocation ->
             P.Id.Invocation.equal invocation.context.id id))
       | _ -> failwith "authored approval lost invocation ownership");
      [%test_eq: int] 1 (List.length (state parent_entry).permissions);
      let limited =
        principal_with_scopes
          (P.Id.Principal.to_string principal.id)
          (Set.remove scopes Answer_approvals)
      in
      let limited_client = connection daemon limited in
      initialize limited_client;
      let limited_handle = attach limited_client child_id in
      (match approve limited_handle permission Approve_once with
       | Error { code = Permission_denied; _ } -> ()
       | _ -> failwith "management authority granted shell approval authority");
      H.close limited_handle;
      Agent_client.Connection.close limited_client;
      approve handle permission Approve_session |> protocol_ok |> ignore;
      let completed = idle child in
      ignore (idle parent_entry : Agent_session.Session_state.t);
      let outcome =
        List.find_exn completed.invocations ~f:(fun invocation ->
          P.Invocation.equal_origin invocation.context.origin Model)
      in
      (match outcome.status with
       | Published (Complete (`String output)) ->
         [%test_eq: string] "authored-shell\n" output
       | status -> raise_s [%sexp "authored shell failed", (status : P.Invocation.status)]);
      [%test_eq: int] 1 (List.length completed.shell.approval_grants);
      assert (List.is_empty (state parent_entry).shell.approval_grants);
      send handle;
      let repeated = idle child in
      [%test_eq: int] 1 (List.length repeated.permissions);
      H.close handle;
      List.iter [ "persistent"; "one_off" ] ~f:(fun requested_mode ->
        mode := requested_mode;
        let unattended, release = create () in
        release ();
        ignore (idle unattended : Agent_session.Session_state.t);
        ignore (idle parent_entry : Agent_session.Session_state.t);
        let rejected = state unattended in
        assert (List.is_empty rejected.shell.approval_grants);
        List.iter rejected.invocations ~f:(fun invocation ->
          match invocation.P.Invocation.status with
          | Published (Complete (`String result)) ->
            let error = Jsonaf.of_string result |> Jsonaf.member_exn "error" in
            [%test_eq: string]
              "denied"
              (Jsonaf.member_exn "code" error |> Jsonaf.string_exn);
            [%test_eq: string]
              "command denied: no shell permission responder is available"
              (Jsonaf.member_exn "message" error |> Jsonaf.string_exn)
          | status ->
            raise_s
              [%sexp "unattended shell was not denied", (status : P.Invocation.status)]);
        assert (not (List.is_empty rejected.invocations));
        match requested_mode with
        | "one_off" ->
          (match rejected.lifecycle.desired, rejected.lifecycle.observed with
           | Stopped, Stopped -> ()
           | _ -> failwith "one-off shell child was not joined and stopped")
        | _ -> ());
      [%test_eq: int] 3 (List.length (state parent_entry).permissions);
      H.close parent_handle;
      print_endline
        "child approval/grant; management cannot approve; exact grant reuse; unattended \
         child denial"));
  [%expect
    {| child approval/grant; management cannot approve; exact grant reuse; unattended child denial |}]
;;

let%expect_test "private authored shell manifests need an exact operator grant" =
  with_sources (fun env root ->
    let base = config root root (Filename.concat root "parent.chatmd") in
    let principal = principal () in
    let provider ~sw:_ ~inputs:_ = failwith "manifest admission called the model" in
    let source_sha256, root_manifest, private_manifest =
      with_daemon env root base principal provider (fun _ daemon _ ->
        let parent = revision daemon in
        let source = Source.capture ~parent ~tool_name:"researcher" |> protocol_ok in
        let child = Source.resource_revision ~parent source |> protocol_ok in
        let inspection =
          Chat_response.Agent_runtime.inspect_shell
            ~env
            ~platform:(Chat_response.Agent_runtime.platform ())
            ~prompt_elements:(Revision.elements child)
          |> Result.map_error ~f:(fun errors ->
            List.map errors ~f:Chat_response.Agent_runtime.diagnostic_to_string
            |> String.concat ~sep:"\n")
          |> Result.ok_or_failwith
        in
        let artifact = Revision.artifact parent in
        ( artifact.root_sha256
        , Option.value_exn artifact.shell_manifest_sha256
        , inspection.manifest.sha256 ))
    in
    assert (not (String.equal root_manifest private_manifest));
    let grant ~id manifest_sha256 =
      Agent_server.Config.Manifest_grant.
        { id
        ; prompt = "restart.prompt"
        ; workspaces = [ "restart.workspace" ]
        ; manifest_sha256
        ; source_sha256
        ; principals = [ P.Id.Principal.to_string principal.id ]
        }
    in
    let configuration grants =
      { base with
        permission_profiles =
          [ { permission_profile with manifest_authorization = Require_grant } ]
      ; manifest_grants = grants
      }
    in
    with_daemon
      env
      root
      (configuration [ grant ~id:"root-only" root_manifest ])
      principal
      provider
      (fun _ _ client ->
         let result =
           Agent_client.Connection.request
             client
             (Session_create (create_request ~key:"wrong-private-manifest" ()))
         in
         assert (Result.is_error result));
    let good = configuration [ grant ~id:"private" private_manifest ] in
    let parent_id =
      with_daemon env root good principal provider (fun _ daemon client ->
        let parent, _ =
          create_session ~key:"private-granted" ~start_immediately:true client
        in
        let entry = R.load (D.registry daemon) parent.id |> protocol_ok in
        let state = A.state entry.actor |> protocol_ok in
        [%test_eq: int] 1 (List.length state.shell.manifest_grants);
        assert (List.is_empty state.shell.approval_grants);
        assert (List.is_empty state.permissions);
        parent.id)
    in
    with_daemon env root good principal provider (fun _ daemon _ ->
      let parent = R.load (D.registry daemon) parent_id |> protocol_ok in
      let state = A.state parent.actor |> protocol_ok in
      [%test_eq: int] 1 (List.length state.shell.manifest_grants);
      assert (List.is_empty state.shell.approval_grants));
    print_endline
      "root manifest rejected; exact private grant admitted and retained; execution \
       approval remains separate");
  [%expect
    {| root manifest rejected; exact private grant admitted and retained; execution approval remains separate |}]
;;
