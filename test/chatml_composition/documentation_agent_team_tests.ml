open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle
module Res = Openai.Responses

let sources =
  [ ( "authored.chatmd"
    , [%blob "../../docs-src/examples/learning/agent-teams/authored.chatmd"] )
  ; ( "generated.chatmd"
    , [%blob "../../docs-src/examples/learning/agent-teams/generated.chatmd"] )
  ; ( "project-tools.chatmd"
    , [%blob "../../docs-src/examples/learning/agent-teams/project-tools.chatmd"] )
  ; ( "agents/reviewer.chatmd"
    , [%blob "../../docs-src/examples/learning/agent-teams/agents/reviewer.chatmd"] )
  ; ( "generated/reviewer.chatmd"
    , [%blob "../../docs-src/examples/learning/agent-teams/generated/reviewer.chatmd"] )
  ; ( "generated/tools.chatmd"
    , [%blob "../../docs-src/examples/learning/agent-teams/generated/tools.chatmd"] )
  ; ( "generated/create.json"
    , [%blob "../../docs-src/examples/learning/agent-teams/generated/create.json"] )
  ; ( "generated/validate.json"
    , [%blob "../../docs-src/examples/learning/agent-teams/generated/validate.json"] )
  ; "server.sexp", [%blob "../../docs-src/examples/learning/agent-teams/server.sexp"]
  ; "sample-project/docs/setup.md", Documentation_shell_tests.setup
  ; "sample-project/docs/reference.md", Documentation_shell_tests.reference
  ; "sample-project/scripts/check-docs.sh", Documentation_shell_tests.checker
  ; "sample-project/expected-report.json", Documentation_shell_tests.expected_report
  ]
;;

let field json name = Jsonaf.member_exn name json
let text json name = field json name |> Jsonaf.string_exn
let source path = List.Assoc.find_exn sources path ~equal:String.equal
let contains json substring = String.is_substring (Jsonaf.to_string json) ~substring

let answer ~id text =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id
    ; status = "completed"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Output_text_delta
      { item_id = id
      ; output_index = 0
      ; content_index = 0
      ; delta = text
      ; type_ = "response.output_text.delta"
      }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

(* Exercise the exact public bundle and its read-only daemon configuration.
   Only model responses are simulated: file tools, admission, durable sessions,
   ownership and receipt/output publication all use the production paths. *)
let with_team ?(sources = sources) ?reviewer_provider f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        List.iter sources ~f:(fun (name, contents) ->
          let path = Eio.Path.(Eio.Stdenv.fs env / root / name) in
          Eio.Path.mkdirs
            ~exists_ok:true
            ~perm:0o700
            (fst (Eio.Path.split path |> Option.value_exn));
          Eio.Path.save ~create:(`Or_truncate 0o600) path contents);
        let config =
          Agent_server.Config_parser.load ~env ~path:(Filename.concat root "server.sexp")
          |> Result.bind ~f:(Agent_server.Config_validator.validate ~env)
          |> Result.map_error ~f:(fun ds ->
            Sexp.to_string_hum [%sexp (ds : Agent_server.Config.Diagnostic.t list)])
          |> Result.ok_or_failwith
        in
        let queued = ref None in
        let calls = ref 0 in
        let continuations = ref 0 in
        let default_provider ~sw:_ ~inputs =
          Int.incr calls;
          let id suffix = sprintf "lesson-%d-%s" !calls suffix in
          let transcript = `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) in
          let child =
            List.exists inputs ~f:(function
              | Res.Item.Input_message message ->
                let json = Res.Item.jsonaf_of_t (Input_message message) in
                String.equal (text json "role") "developer"
                && (contains json "You are Lantern's documentation reviewer."
                    || contains json "Review Lantern's setup tutorial for a reader")
              | _ -> false)
          in
          match child with
          | false ->
            (match !queued with
             | None -> Stdlib.Seq.empty
             | Some (name, args) ->
               queued := None;
               Fixtures.call_events [ id "parent", name, args ])
          | true ->
            if contains transcript "Refine the proposed verification wording."
            then (
              assert (
                contains
                  transcript
                  "Recorded verification failure; propose a Verification section.");
              Int.incr continuations;
              answer
                ~id:(id "followup")
                "Refined verification wording; still needs a new checker run.")
            else if contains transcript "function_call_output"
            then
              answer
                ~id:(id "review")
                "Recorded verification failure; propose a Verification section."
            else
              Fixtures.call_events
                (List.map
                   [ "setup", "docs/setup.md"
                   ; "report", "expected-report.json"
                   ; "escape", "../server.sexp"
                   ]
                   ~f:(fun (label, path) ->
                     ( id label
                     , "read_file"
                     , `Object [ "root", `String "project"; "file", `String path ] )))
        in
        let provider ~sw ~inputs =
          match reviewer_provider with
          | None -> default_provider ~sw ~inputs
          | Some review ->
            (match review ~sw ~inputs with
             | Some output -> output
             | None -> default_provider ~sw ~inputs)
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            D.start
              ~sw
              ~env
              ~config
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:{ D.default_options with model_post_stream = Some provider }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              let client = connection daemon (principal ()) in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close client)
                ~f:(fun () ->
                  initialize client;
                  let state id =
                    let entry =
                      Agent_server.Session_registry.load (D.registry daemon) id
                      |> protocol_ok
                    in
                    A.state entry.actor |> protocol_ok
                  in
                  let create prompt =
                    let spec =
                      P.Session.Spec.create
                        ~execution_host:Daemon
                        ~prompt:
                          (Catalog
                             (Agent_server.Catalog_identity.prompt_definition prompt))
                        ~workspace:
                          (Configured
                             (Agent_server.Catalog_identity.workspace_definition
                                "lantern"))
                        ~liveness:Detached
                        ~persistence:Durable
                        ~permission_profile:"reader"
                        ~start_immediately:true
                        ~labels:[]
                        ()
                      |> protocol_ok
                    in
                    H.create
                      ~sw
                      ~clock:(Eio.Stdenv.clock env)
                      ~connection:client
                      ~spec
                      ~mode:Read_write
                      ()
                    |> protocol_ok
                  in
                  let invoke handle name args =
                    let parent = H.session_id handle in
                    let before = (state parent).invocations in
                    queued := Some (name, args);
                    H.send_message
                      handle
                      { kind = Plain_text
                      ; text = "Perform the requested review operation."
                      ; attachments = []
                      }
                    |> protocol_ok
                    |> ignore;
                    let outcome = ref None in
                    Background_shell_tests.wait env (fun () ->
                      let current = state parent in
                      outcome
                      := List.find_map current.invocations ~f:(fun invocation ->
                           if
                             List.exists before ~f:(fun old ->
                               P.Id.Invocation.equal
                                 old.P.Invocation.context.id
                                 invocation.context.id)
                           then None
                           else (
                             match invocation.status with
                             | Published result
                               when P.Invocation.equal_origin
                                      invocation.context.origin
                                      Model -> Some result
                             | _ -> None));
                      Option.is_none current.active_operation && Option.is_some !outcome);
                    Option.value_exn !outcome
                  in
                  f create invoke state continuations)))))
;;

let complete = function
  | P.Invocation.Complete value -> value
  | outcome -> raise_s [%sexp "lesson tool failed", (outcome : P.Invocation.outcome)]
;;

let native_json outcome =
  match complete outcome with
  | `String value -> Jsonaf.of_string value
  | value -> value
;;

let check_reads current =
  let outputs =
    List.filter_map current.Agent_session.Session_state.invocations ~f:(fun invocation ->
      match invocation.status with
      | P.Invocation.Published (Complete value) -> Some value
      | _ -> None)
  in
  assert (List.exists outputs ~f:(fun value -> contains value "Lantern"));
  assert (List.exists outputs ~f:(fun value -> contains value "verification"));
  assert (List.exists outputs ~f:(fun value -> contains value "error"));
  assert (not (List.exists outputs ~f:(fun value -> contains value "manifest_grants")))
;;

let%expect_test
    "public authored and generated specialists retain useful review history and bounded \
     tools"
  =
  with_team (fun create invoke state continuations ->
    let authored = create "authored" in
    let call handle name args = invoke handle name (`Object args) |> complete in
    let first =
      call
        authored
        "quick_review"
        [ "input", `String "Review the recorded documentation findings."
        ; "mode", `String "persistent"
        ]
    in
    let id = field first "session_id" in
    let child = P.Id.Session.of_json id |> protocol_ok in
    check_reads (state child);
    let next =
      call
        authored
        "quick_review"
        [ "input", `String "Refine the proposed verification wording."
        ; "mode", `String "persistent"
        ; "session_id", id
        ]
    in
    assert (Jsonaf.exactly_equal id (field next "session_id"));
    assert (contains next "Refined verification wording");
    (match
       invoke
         authored
         "review_thread"
         (`Object [ "input", `String "Review."; "session_id", id ])
     with
     | Fail error -> [%test_eq: string] "agent.authored.permission_denied" error.code
     | outcome ->
       raise_s [%sexp "wrong specialist accepted", (outcome : P.Invocation.outcome)]);
    let one_off = call authored "quick_review" [ "input", `String "Review once." ] in
    assert (contains one_off "Recorded verification failure");
    let fixed =
      call authored "review_thread" [ "input", `String "Review in a retained thread." ]
    in
    assert (not (Jsonaf.exactly_equal id (field fixed "session_id")));
    print_endline
      "authored: optional and fixed persistence; same-instance follow-up; wrong wrapper \
       denied; one-off output";
    let generated = create "generated" in
    let prepare =
      call
        generated
        "ochat_authoring_context"
        [ "version", `Number "1"
        ; "operation", `String "prepare"
        ; "task", `String "child_agent"
        ; "query", `Null
        ; "topic_id", `Null
        ; "features", `Null
        ; "cursor", `Null
        ; "max_tokens", `Null
        ]
    in
    assert (contains prepare "child_agent");
    let validate = Jsonaf.of_string (source "generated/validate.json") in
    let candidate = Jsonaf.of_string (source "generated/create.json") in
    List.iter
      (field candidate "sources" |> Jsonaf.list_exn)
      ~f:(fun item ->
        [%test_eq: string] (source ("generated/" ^ text item "path")) (text item "text"));
    let validation = invoke generated "ochat_validate" validate |> native_json in
    assert (field validation "valid" |> Jsonaf.bool_exn);
    assert (Jsonaf.exactly_equal (field candidate "sources") (field validate "sources"));
    let replace json name value =
      match json with
      | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
      | _ -> failwith "expected candidate object"
    in
    let widened_sources =
      field validate "sources"
      |> Jsonaf.list_exn
      |> List.map ~f:(fun item ->
        if String.equal (text item "path") "tools.chatmd"
        then
          replace
            item
            "text"
            (`String {|<tool name="read_file"><read id="project" path="/"/></tool>|})
        else item)
    in
    let rejected =
      invoke
        generated
        "ochat_validate"
        (replace validate "sources" (`Array widened_sources))
      |> native_json
    in
    assert (not (field rejected "valid" |> Jsonaf.bool_exn));
    let created = invoke generated "agent_create" candidate |> complete in
    let generated_id = field created "session_id" in
    let retry = invoke generated "agent_create" candidate |> complete in
    assert (Jsonaf.exactly_equal generated_id (field retry "session_id"));
    List.iteri
      [ "Review the recorded documentation findings."
      ; "Refine the proposed verification wording."
      ]
      ~f:(fun index message ->
        let receipt =
          call
            generated
            "agent_send"
            [ "session_id", generated_id
            ; "message", `String message
            ; "idempotency_key", `String (sprintf "review-%d" index)
            ]
        in
        let query =
          [ "session_id", generated_id; "receipt_id", field receipt "receipt_id" ]
        in
        let waited = call generated "agent_wait" query in
        assert (contains waited "completed");
        let output = call generated "agent_read" query in
        assert (
          contains
            output
            (if index = 0
             then "Recorded verification failure"
             else "Refined verification wording")));
    check_reads (state (P.Id.Session.of_json generated_id |> protocol_ok));
    let query = [ "session_id", generated_id ] in
    call generated "agent_status" query |> ignore;
    call
      generated
      "agent_stop"
      (query
       @ [ "mode", `String "graceful"; "idempotency_key", `String "review-finished" ])
    |> ignore;
    assert (contains (call generated "agent_read" query) "Refined verification wording");
    [%test_eq: int] 2 !continuations;
    H.close authored;
    H.close generated;
    print_endline
      "generated: queried authoring help; exact captured source validated; broader tool \
       rejected; creation retry reused child; distinct receipts and retained follow-up";
    print_endline
      "both: real sample reads; private configuration denied; stopped child remains \
       readable; no model API calls");
  [%expect
    {|
    authored: optional and fixed persistence; same-instance follow-up; wrong wrapper denied; one-off output
    generated: queried authoring help; exact captured source validated; broader tool rejected; creation retry reused child; distinct receipts and retained follow-up
    both: real sample reads; private configuration denied; stopped child remains readable; no model API calls
    |}]
;;
