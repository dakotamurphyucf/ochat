open Core
open Agent_server_test_support
module E = Agent_server.Embedded
module P = Agent_protocol
module Surface = Embedded_extension_tests

let agent =
  {|
<shell_access id="inspection" extends="builtin:workspace-readonly@1">
  <policy default="deny" merge="replace">
    <rule id="echo" action="allow"><basename value="echo"/></rule>
    <rule id="deny" action="deny"><argument value="denied"/></rule>
    <rule id="review" action="ask"><argument value="review"/></rule>
  </policy>
</shell_access>
<tool name="echo" type="shell" mode="fixed" runtime="inspection"
      command="/bin/echo" result="structured"/>
|}
;;

let%expect_test
    "native shell opt-in permits startup but preserves hard denial and approval"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          agent;
        let options authorize_shell_manifest : E.options =
          { prompt_file
          ; workspace = root
          ; tool_dir = root
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile =
              E.interactive_permission_profile ~authorize_shell_manifest
          ; attachment_mode = Read_write
          ; event_capacity = 512
          }
        in
        Eio.Switch.run (fun sw ->
          (match E.start ~sw ~env (options false) with
           | Error error ->
             if not (String.is_substring error.message ~substring:"manifest")
             then raise_s [%sexp (error : P.Error.t)];
             print_endline "without authorization: startup rejected"
           | Ok host ->
             E.close host;
             failwith "unapproved shell manifest started");
          let requests = ref 0 in
          let calls =
            List.map [ "allowed"; "denied"; "review" ] ~f:(fun value ->
              value, "echo", `Object [ "arguments", `Array [ `String value ] ])
          in
          let post_stream ~sw:_ ~inputs:_ =
            incr requests;
            match !requests with
            | 1 -> Fixtures.call_events calls
            | 2 -> Fixtures.call_events []
            | _ -> failwith "unexpected model request"
          in
          let daemon_options =
            { Agent_server.Daemon.default_options with
              model_post_stream = Some post_stream
            }
          in
          let host = E.start ~sw ~env ~daemon_options (options true) |> protocol_ok in
          Eio.Fiber.fork_daemon ~sw (fun () ->
            let rec receive () =
              match Agent_client.Connection.next_notification (E.connection host) with
              | None -> `Stop_daemon
              | Some _ -> receive ()
            in
            receive ());
          Exn.protect
            ~finally:(fun () -> E.close host)
            ~f:(fun () ->
              Surface.send host "Exercise shell policy.";
              let pending = ref None in
              Background_shell_tests.wait env (fun () ->
                pending
                := List.find (Surface.snapshot host).permissions ~f:(fun permission ->
                     P.Permission.equal_state permission.state Pending);
                Option.is_some !pending);
              let permission = Option.value_exn !pending in
              (* The other two results must be publishable while this request is
                 still pending. Approval must not serialize an entire tool batch. *)
              Background_shell_tests.wait env (fun () ->
                List.count
                  (Surface.snapshot host).canonical_history.entries
                  ~f:(fun entry -> P.History.equal_kind entry.kind Tool_output)
                = 2);
              Surface.request
                host
                (Permission_respond
                   { session_id = E.session_id host
                   ; attachment_id = (E.attachment host).id
                   ; permission_id = permission.id
                   ; permission_generation = permission.generation
                   ; choice = Approve_once
                   ; reason = Some "native shell regression"
                   ; idempotency_key =
                       P.Idempotency_key.of_string "native-shell:approve" |> protocol_ok
                   })
              |> ignore;
              (try
                 Background_shell_tests.wait env (fun () ->
                   !requests = 2
                   && Option.is_none (Surface.snapshot host).session.active_operation)
               with
               | Eio.Time.Timeout ->
                 let snapshot = Surface.snapshot host in
                 raise_s
                   [%sexp
                     "native shell did not finish"
                   , (!requests : int)
                   , (snapshot : P.Snapshot.t)]);
              let snapshot = Surface.snapshot host in
              List.iter [ "allowed"; "review" ] ~f:(fun id ->
                match Surface.initial_outcome snapshot id with
                | Complete (`String text) ->
                  let result = Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string text) in
                  assert (Shell_runtime.Result.equal_status result.status (Exited 0));
                  [%test_eq: string] (id ^ "\n") result.stdout;
                  assert (
                    List.mem
                      [ "macos-seatbelt"; "linux-bubblewrap" ]
                      result.backend
                      ~equal:String.equal)
                | other -> raise_s [%sexp (other : P.Invocation.outcome)]);
              (match Surface.initial_outcome snapshot "denied" with
               | Complete (`String text) ->
                 let error = Jsonaf.member_exn "error" (Jsonaf.of_string text) in
                 [%test_eq: string]
                   "denied"
                   (Jsonaf.member_exn "code" error |> Jsonaf.string_exn);
                 print_endline "hard denial: command rejected by shell policy"
               | other -> raise_s [%sexp (other : P.Invocation.outcome)]);
              [%test_eq: int] 1 (List.length snapshot.permissions);
              print_endline
                "authorized native host: allowed command executes; reviewed command \
                 waits for approval; required sandbox retained"))));
  [%expect
    {|
    without authorization: startup rejected
    hard denial: command rejected by shell policy
    authorized native host: allowed command executes; reviewed command waits for approval; required sandbox retained
    |}]
;;
