open Core
open Agent_server_test_support
module F = Crash_recovery_fixture
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module D = Agent_store.Delegation_store
module S = Agent_store.Session_store
module H = Agent_client.Session_handle
module Res = Openai.Responses

let parent_text =
  {|<developer>AUTHORED_CRASH_PARENT</developer><tool name="researcher" agent="child.chatmd" local persistence="optional"/>|}
;;

let child_text =
  {|<developer>AUTHORED_CRASH_SPECIALIST</developer><tool name="read_file"><read id="private" path="${workspace}"/></tool>|}
;;

let state entry = A.state entry.R.actor |> F.protocol_ok

let record daemon =
  D.with_records
    (S.delegations (Daemon.store daemon))
    ~max_records:8
    ~max_bytes:1048576
    ~f:(fun records -> Ok records)
  |> F.store_ok
  |> function
  | [ record ] -> record
  | _ -> F.fail "authored creation was lost or duplicated"
;;

let tool_call serial name args =
  let call_id = sprintf "authored-crash-%d" serial in
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
      { arguments = Jsonaf.to_string args
      ; item_id = call_id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let run_child ?(lose_ack = false) env ~root ~boundary ~mode ~recover =
  let one_off = String.equal mode "one_off" in
  F.require (one_off || String.equal mode "persistent") "invalid authored crash mode";
  let armed = ref false in
  let writes = ref 0 in
  let ledger_prefix = Filename.concat root "data/delegations/" in
  let wrapped =
    if recover
    then env
    else
      Support.Crash_fault_io.wrap
        env
        ~boundary:
          (match boundary with
           | "artifact-partial" | "snapshot-partial" -> After_bytes 3
           | _ -> After_rename)
        ~matches:(fun path ->
          !armed
          &&
          match boundary with
          | "artifact-partial" ->
            String.is_prefix
              path
              ~prefix:(Filename.concat root "data/prompt-artifacts/.install-")
            && String.is_suffix path ~suffix:".chatmd"
          | "snapshot-partial" ->
            String.is_prefix
              path
              ~prefix:(Filename.concat root "data/sessions/.creating-")
            && String.is_suffix path ~suffix:".bin"
          | _ -> String.is_prefix path ~prefix:ledger_prefix)
        ~reached:(fun _ ->
          Int.incr writes;
          let target =
            match boundary with
            | "reserved" | "artifact-partial" | "snapshot-partial" -> 1
            | "artifact-record" -> 2
            | "child-record" -> 3
            | "linked" -> 4
            | "active" -> 0
            | _ -> F.fail "invalid authored crash boundary"
          in
          if Int.equal target !writes
          then (
            if lose_ack then failwith "injected authored creation acknowledgement loss";
            Eio.Flow.copy_string "authored-creation-boundary\n" (Eio.Stdenv.stdout env);
            Eio.Fiber.await_cancel ()))
  in
  let calls = ref 0 in
  let child_waiting, child_waiting_u = Eio.Promise.create () in
  let provider ~sw:_ ~inputs =
    F.require (not recover) "authored crash recovery replayed a model request";
    Int.incr calls;
    let child =
      List.exists inputs ~f:(function
        | Res.Item.Input_message message ->
          let json = Res.Item.jsonaf_of_t (Input_message message) in
          String.equal (Jsonaf.member_exn "role" json |> Jsonaf.string_exn) "developer"
          && String.is_substring
               (Jsonaf.to_string json)
               ~substring:"AUTHORED_CRASH_SPECIALIST"
        | _ -> false)
    in
    match child, List.last inputs with
    | true, Some (Res.Item.Function_call_output _) ->
      Eio.Promise.resolve child_waiting_u ();
      Eio.Fiber.await_cancel ()
    | true, _ ->
      tool_call
        !calls
        "read_file"
        (`Object [ "root", `String "private"; "file", `String "value.txt" ])
    | false, Some (Res.Item.Function_call_output _) when lose_ack -> Stdlib.Seq.empty
    | false, _ ->
      tool_call
        !calls
        "researcher"
        (`Object [ "input", `String "Read the fixture."; "mode", `String mode ])
  in
  let retained_reference = ref None in
  let retained_history = ref None in
  let retained_parent_history = ref None in
  List.iter
    (if recover then [ 1; 2 ] else [ 0 ])
    ~f:(fun _ ->
      Eio.Switch.run (fun sw ->
        let daemon =
          Daemon.start
            ~sw
            ~env:wrapped
            ~config:(config root root (Filename.concat root "parent.chatmd"))
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options:
              { Daemon.default_options with
                qualify_chatml_extensions = true
              ; model_post_stream = Some provider
              }
            ()
          |> F.protocol_ok
        in
        Exn.protect
          ~finally:(fun () -> Daemon.shutdown daemon |> F.protocol_ok)
          ~f:(fun () ->
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () -> Agent_client.Connection.close client)
              ~f:(fun () ->
                initialize client;
                let attach id =
                  H.attach
                    ~sw
                    ~clock:(Eio.Stdenv.clock env)
                    ~connection:client
                    ~session_id:id
                    ~mode:Read_write
                    ~subscribe:false
                    ()
                  |> F.protocol_ok
                in
                if not recover
                then (
                  let parent, _ = create_session ~start_immediately:true client in
                  F.write
                    env
                    (Filename.concat root "parent-id")
                    (P.Id.Session.to_string parent.id);
                  let handle = attach parent.id in
                  armed := true;
                  H.send_message
                    handle
                    { kind = Plain_text; text = "Call the specialist."; attachments = [] }
                  |> F.protocol_ok
                  |> ignore;
                  if String.equal boundary "active"
                  then (
                    Eio.Promise.await child_waiting;
                    let admitted = record daemon in
                    let child =
                      R.load (Daemon.registry daemon) admitted.admission.child_session_id
                      |> F.protocol_ok
                    in
                    let child_state = state child in
                    F.require
                      (Option.is_some child_state.active_operation)
                      "child was not active at crash";
                    F.require
                      (List.exists child_state.invocations ~f:(fun invocation ->
                         match invocation.P.Invocation.status with
                         | Published (Complete value) ->
                           String.is_substring
                             (Jsonaf.to_string value)
                             ~substring:"authored-crash-read"
                         | _ -> false))
                      "child had no persisted tool result at crash";
                    Eio.Flow.copy_string
                      "authored-creation-boundary\n"
                      (Eio.Stdenv.stdout env));
                  if lose_ack
                  then (
                    let parent =
                      R.load (Daemon.registry daemon) parent.id |> F.protocol_ok
                    in
                    let rec settled () =
                      let current = state parent in
                      match current.active_operation with
                      | None -> current
                      | Some _ ->
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        settled ()
                    in
                    let current = settled () in
                    (match current.invocations with
                     | [ { status = Published (Fail error); _ } ] ->
                       F.require
                         (String.equal error.code "agent.authored.persistence_error")
                         "lost acknowledgement did not report persistence failure"
                     | _ ->
                       raise_s
                         [%sexp
                           "lost acknowledgement outcome"
                         , (current.invocations : P.Invocation.t list)]);
                    F.require
                      (Int.equal !calls 2)
                      "failed creation executed the specialist";
                    let admitted = record daemon in
                    (match
                       R.find (Daemon.registry daemon) admitted.admission.child_session_id
                     with
                     | None -> ()
                     | Some child ->
                       let child_state = state child in
                       F.require
                         (Option.is_none child_state.active_operation
                          && List.is_empty child_state.managed_submissions)
                         "failed creation submitted child work";
                       if one_off
                       then
                         F.require
                           (not (Agent_server.Runtime_owner.is_loaded child.runtime))
                           "failed one-off creation retained executing child resources");
                    H.close handle)
                  else Eio.Fiber.await_cancel ())
                else (
                  let admitted = record daemon in
                  let reference = D.reference admitted in
                  (match !retained_reference with
                   | None -> retained_reference := Some reference
                   | Some expected ->
                     F.require
                       (D.Reference.equal expected reference)
                       "authored identity changed across recovery");
                  (match admitted.admission.authored_tool with
                   | Some origin ->
                     F.require
                       (String.equal origin.name "researcher")
                       "wrong authored declaration recovered"
                   | None -> F.fail "authored admission became generated");
                  let parent_id =
                    F.read env (Filename.concat root "parent-id")
                    |> P.Id.Session.of_string
                    |> F.protocol_ok
                  in
                  let parent =
                    R.load (Daemon.registry daemon) parent_id |> F.protocol_ok
                  in
                  let parent_state = state parent in
                  let invocations =
                    List.filter parent_state.invocations ~f:(fun invocation ->
                      P.Invocation.equal_origin invocation.context.origin Model)
                  in
                  let invocation =
                    match invocations with
                    | [ invocation ] -> invocation
                    | _ -> F.fail "parent invocation duplicated"
                  in
                  (match invocation.status with
                   | Published (Fail error) when lose_ack ->
                     F.require
                       (String.equal error.code "agent.authored.persistence_error")
                       "recovery changed the acknowledged creation failure"
                   | Published (Cancelled reason) when not lose_ack ->
                     F.require
                       (String.equal
                          reason
                          "daemon restarted before the invocation recorded an outcome")
                       "incorrect interruption reason"
                   | status ->
                     raise_s
                       [%sexp
                         "interrupted authored invocation was left executable"
                       , (status : P.Invocation.status)]);
                  (match !retained_parent_history with
                   | None ->
                     retained_parent_history
                     := Some parent_state.conversation.canonical_history
                   | Some expected ->
                     F.require_equal
                       "parent interruption was published twice"
                       [%sexp_of: P.History.entry list]
                       expected
                       parent_state.conversation.canonical_history);
                  (match admitted.admission.lifetime with
                   | Invocation_owned { invocation_id } when one_off ->
                     F.require
                       (P.Id.Invocation.equal invocation_id invocation.context.id)
                       "one-off owner changed"
                   | Owned when not one_off -> ()
                   | _ -> F.fail "authored lifetime changed");
                  let installed =
                    List.mem
                      [ "child-record"; "linked"; "active" ]
                      boundary
                      ~equal:String.equal
                  in
                  F.require
                    (List.length (S.list_sessions (Daemon.store daemon))
                     = if installed then 2 else 1)
                    "recovery duplicated or silently created a child";
                  (match one_off, boundary, admitted.revocation, admitted.stage with
                   | ( true
                     , ( "reserved"
                       | "artifact-partial"
                       | "artifact-record"
                       | "snapshot-partial"
                       | "child-record" )
                     , Some Admission_failed
                     , _ ) -> ()
                   | true, ("linked" | "active"), None, Linked -> ()
                   | false, ("reserved" | "artifact-partial"), None, Reserved -> ()
                   | ( false
                     , ("artifact-record" | "snapshot-partial")
                     , None
                     , Artifact_installed ) -> ()
                   | false, ("child-record" | "linked" | "active"), None, Linked -> ()
                   | _ ->
                     raise_s [%sexp "unexpected authored recovery", (admitted : D.record)]);
                  let transaction =
                    P.Id.Transaction.to_string admitted.admission.transaction_id
                  in
                  List.iter
                    [ "data/prompt-artifacts/.install-"; "data/sessions/.creating-" ]
                    ~f:(fun prefix ->
                      F.require
                        (match
                           Eio.Path.kind
                             ~follow:false
                             (F.path env (Filename.concat root (prefix ^ transaction)))
                         with
                         | `Not_found -> true
                         | _ -> false)
                        "recovery retained an incomplete creation directory");
                  if installed
                  then (
                    let child =
                      R.load (Daemon.registry daemon) admitted.admission.child_session_id
                      |> F.protocol_ok
                    in
                    let rec settled () =
                      let current = state child in
                      match current.lifecycle.observed, current.pending_initial_start with
                      | (Idle | Stopped), false -> current
                      | Failed error, _ -> raise_s [%sexp (error : P.Error.t)]
                      | _ ->
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        settled ()
                    in
                    let current = settled () in
                    F.require
                      (Option.is_none current.active_operation)
                      "recovery retained active child operation";
                    (match
                       one_off, current.lifecycle.desired, current.lifecycle.observed
                     with
                     | true, Stopped, Stopped | false, Running, Idle -> ()
                     | _ ->
                       F.fail "incorrect authored child execution lifetime after crash");
                    let history = current.conversation.canonical_history in
                    F.require
                      (List.exists history ~f:(fun entry ->
                         String.is_substring
                           (Jsonaf.to_string entry.P.History.payload)
                           ~substring:"AUTHORED_CRASH_SPECIALIST"))
                      "pinned authored history was lost";
                    (match !retained_history with
                     | None -> retained_history := Some history
                     | Some expected ->
                       F.require_equal
                         "repeated authored recovery changed history"
                         [%sexp_of: P.History.entry list]
                         expected
                         history);
                    if String.equal boundary "active"
                    then
                      F.require
                        (List.exists history ~f:(fun entry ->
                           String.is_substring
                             (Jsonaf.to_string entry.P.History.payload)
                             ~substring:"authored-crash-read"))
                        "crash lost acknowledged private tool output";
                    if one_off
                    then (
                      F.require
                        (not (Agent_server.Runtime_owner.is_loaded child.runtime))
                        "ended one-off loaded resources";
                      let handle = attach current.identity.session_id in
                      (match H.start handle ~queue_if_limited:false with
                       | Error { code = Permission_denied; _ } -> ()
                       | _ -> F.fail "ended one-off was restartable");
                      H.close handle));
                  F.require (Int.equal !calls 0) "recovery ran a provider")))));
  Eio.Flow.copy_string "authored-creation-recovered\n" (Eio.Stdenv.stdout env)
;;

let run_cases ~lose_ack env environment =
  List.iter [ "persistent"; "one_off" ] ~f:(fun mode ->
    List.iter
      (if lose_ack
       then [ "reserved"; "artifact-record"; "child-record"; "linked" ]
       else
         [ "reserved"
         ; "artifact-partial"
         ; "artifact-record"
         ; "snapshot-partial"
         ; "child-record"
         ; "linked"
         ; "active"
         ])
      ~f:(fun boundary ->
        let root =
          Filename.concat
            (Support.Temporary_environment.roots environment).temporary
            ((if lose_ack then "authored-ack-" else "authored-") ^ mode ^ "-" ^ boundary)
        in
        Eio.Path.mkdir ~perm:0o700 (F.path env root);
        F.write env (Filename.concat root "parent.chatmd") parent_text;
        F.write env (Filename.concat root "child.chatmd") child_text;
        F.write env (Filename.concat root "value.txt") "authored-crash-read";
        Eio.Switch.run (fun sw ->
          let child =
            F.child
              ~sw
              env
              environment
              ~case:"authored-create"
              ~arguments:
                [ (if lose_ack then "authored-ack" else "authored-create")
                ; root
                ; boundary
                ; mode
                ]
          in
          Exn.protect
            ~finally:(fun () -> F.terminate env child)
            ~f:(fun () ->
              if lose_ack
              then (
                let result =
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                    Support.Process_manager.await child)
                in
                match result.exit with
                | Exited 0
                  when String.is_substring
                         result.stdout.contents
                         ~substring:"authored-creation-recovered" -> ()
                | _ ->
                  raise_s
                    [%sexp
                      "authored acknowledgement failure"
                    , (mode : string)
                    , (boundary : string)
                    , (result : Support.Process_manager.result)])
              else (
                F.await_marker env child "authored-creation-boundary";
                F.kill env child));
          F.write
            env
            (Filename.concat root "child.chatmd")
            "<developer>Edited live source without private tools.</developer>";
          let recovery =
            F.child
              ~sw
              env
              environment
              ~case:"authored-recover"
              ~arguments:
                [ (if lose_ack then "authored-ack-recover" else "authored-recover")
                ; root
                ; boundary
                ; mode
                ]
          in
          Exn.protect
            ~finally:(fun () -> F.terminate env recovery)
            ~f:(fun () ->
              let result =
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                  Support.Process_manager.await recovery)
              in
              match result.exit with
              | Exited 0
                when String.is_substring
                       result.stdout.contents
                       ~substring:"authored-creation-recovered" -> ()
              | _ ->
                raise_s
                  [%sexp
                    "authored crash recovery failed"
                  , (mode : string)
                  , (boundary : string)
                  , (result : Support.Process_manager.result)]))))
;;

let test = run_cases ~lose_ack:false
let test_lost_ack = run_cases ~lose_ack:true
