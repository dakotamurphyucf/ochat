open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module Notes = Recovery_note_share_tests
module T = A.Script_tool_calls
module C = Chat_response.Tool_capability
module D = Chat_response.In_memory_stream.Tool_dispatch
module M = Inference_runtime.Protected_delivery.Materialized
module I = P.Invocation
module Worker = A.Operation_worker

let effect_names =
  [ "recovery_note_read"
  ; "recovery_note_list"
  ; "recovery_note_search"
  ; "recovery_note_write"
  ; "recovery_note_append"
  ; "recovery_note_delete"
  ]
;;

let selected_tools env names =
  let source =
    List.map names ~f:(sprintf {|<tool name="%s"/>|}) |> String.concat ~sep:"\n"
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs ~dir:(Eio.Stdenv.cwd env) source
  in
  A.Extensibility_native_tools.registrations
    ~env
    ~elements
    ~one_off_policy:None
    ~authoring_validation_host:None
  |> protocol_ok
;;

let selected registrations =
  let names_and_contracts =
    List.map
      registrations
      ~f:(fun (r : Chat_response.Agent_runtime.native_registration) ->
        r.implementation.info.function_.name, r.result_contract)
  in
  let implementations =
    List.map
      registrations
      ~f:(fun (r : Chat_response.Agent_runtime.native_registration) ->
        r.implementation_revision, r.implementation)
  in
  C.create
    ~result_contracts:names_and_contracts
    ~owner:"selected-note-test"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "fixture")
    implementations
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let outcome value = I.outcome_of_json value |> protocol_ok

let output_outcome = function
  | Openai.Responses.Tool_output.Output.Text text -> outcome (Jsonaf.of_string text)
  | _ -> failwith "expected structured invocation outcome"
;;

let complete = function
  | I.Complete value -> value
  | outcome -> raise_s [%sexp "expected complete", (outcome : I.outcome)]
;;

let field value name =
  match Document_schema.Json.field value ~name with
  | Value value -> value
  | Null -> `Null
  | Absent -> failwith ("missing field " ^ name)
;;

let expect_json actual expected = assert (Document_schema.Json.equal actual expected)

let expect_failure result code =
  match result with
  | I.Fail error -> [%test_eq: string] error.code code
  | outcome -> raise_s [%sexp "expected failure", (outcome : I.outcome)]
;;

let body = "local-private-β-fixture"

let local_service actor =
  let module Delivery = Inference_runtime.Protected_delivery in
  let prepare ~access query =
    let%map.Result selected = Actor.prepare_recovery_note_query actor ~access query in
    let materialize () =
      match selected.materialize () with
      | Error _ -> Error Delivery.Unavailable
      | Ok value ->
        Ok
          (History_entry.Payload.Output.Text
             (I.outcome_to_json (Complete value) |> Jsonaf.to_string))
    in
    A.Recovery_note_native_service.
      { custody = Local; materialize; refresh = (fun ~scope:_ -> selected.check_current) }
  in
  let mutate ~access operation =
    Actor.mutate_recovery_note_native actor ~access operation
  in
  let prepare_shared ~access:_ _ _ = Error (A.Recovery_note_share.unavailable ()) in
  A.Recovery_note_native_service.{ prepare; prepare_shared; mutate }
;;

let script_registration env tools =
  A.Run_chatml_tool.registration
    ~env
    ~policy:Chat_response.One_off_request.default_policy
    ~services:(fun () ->
      let open Result.Let_syntax in
      let%map script_tools = T.current_native_services () in
      assert (phys_equal script_tools (Option.value_exn !tools));
      A.Run_chatml_tool.
        { script_tools
        ; observer = None
        ; now = P.Timestamp.now
        ; moderate_tool =
            (fun _ call ->
              assert (Document_schema.Json.equal call.args (`Object []));
              [%test_eq: string] call.payload_text "{}";
              Ok None)
        ; prepare_outcome = (fun _ -> Ok ())
        })
;;

let script_request =
  let source =
    {|let main input =
  let body = String.concat("script-private-", "β-body") in
  let request = `Object([
    {key = "kind"; value = `String("create")},
    {key = "expected_catalog_revision"; value = `String("1")},
    {key = "path"; value = `String("research/script")},
    {key = "expected"; value = `Null},
    {key = "content"; value = `String(body)}
  ]) in
  let* result = Tool.call("recovery_note_write", request) in
  match result with
  | `Ok(`Null) ->
    let* read = Tool.call("recovery_note_read", `Object([])) in
    (match read with
    | `Error("invocation.protected_owner_required") -> Task.pure(`String("script committed"))
    | _ -> Task.fail("a script observed protected data"))
  | `Error(code) -> Task.fail(code)
  | _ -> Task.fail("expected a body-free acknowledgement")|}
  in
  `Object
    [ "source", `String source
    ; "input", `Null
    ; "tools", `Array [ `String "recovery_note_write"; `String "recovery_note_read" ]
    ]
;;

let invoke ~caps dispatch history call_count name args =
  let entry ?call_relation item =
    let id =
      History_entry.Id_source.allocate caps.Worker.Capabilities.id_source
      |> Result.ok_or_failwith
    in
    let entry = Openai.Responses_history.create_with_id_exn ?call_relation ~id item in
    let semantic = History_entry.Payload.semantic (History_entry.payload entry) in
    History_entry.with_payload entry (History_entry.Payload.authored semantic)
  in
  let protection = Option.value_exn dispatch.D.protection in
  let call_id = sprintf "note-%d" !call_count in
  Int.incr call_count;
  let payload = Jsonaf.to_string args in
  let canonical_input = if protection.claims ~name then "{}" else payload in
  let call =
    Chat_response.Tool_call.call_item
      ~kind:Function
      ~name
      ~payload:canonical_input
      ~call_id
      ~id:None
    |> entry
  in
  let request =
    D.
      { kind = Function
      ; original_name = name
      ; original_payload = payload
      ; name
      ; payload
      ; rejection = None
      ; call
      ; history = !history @ [ call ]
      ; source = None
      ; parent_call_id = None
      }
  in
  assert (dispatch.commit_call request);
  let result = dispatch.run request ~authorize:(fun () -> ()) |> Option.value_exn in
  let output =
    Chat_response.Tool_call.output_item ~kind:Function ~call_id ~output:result.output
    |> entry ~call_relation:(Bound (History_entry.id call))
  in
  (Option.value_exn result.commit_output) output;
  history := !history @ [ call; output ];
  let plans = protection.take () in
  let transient =
    match plans with
    | [] -> None
    | _ ->
      let prepared = M.prepare plans ~history:!history |> Result.ok |> Option.value_exn in
      (match M.find prepared (History_entry.id output) with
       | Some (Text value) -> Some (complete (outcome (Jsonaf.of_string value)))
       | Some (Content _) | None -> failwith "missing bounded transient note response")
  in
  output_outcome result.output, transient
;;

let str value = `String value
let num value = `Number (Int.to_string value)
let page = [ "limit", num 1; "cursor", `Null ]
let selection rev = `Object [ "id", str "1"; "revision", str rev ]

let read revision ~start ~stop =
  `Object
    [ ( "selection"
      , `Object [ "kind", str "local"; "id", str "1"; "revision", str revision ] )
    ; "range", `Object [ "start", num start; "stop", num stop ]
    ]
;;

let exercise calls =
  let create =
    `Object
      [ "kind", str "create"
      ; "expected", `Null
      ; "path", str "research/local"
      ; "expected_catalog_revision", str "0"
      ; "content", str body
      ]
  in
  let check_ack (durable, transient) =
    assert (Document_schema.Json.equal (complete durable) `Null);
    assert (Option.is_none transient)
  in
  check_ack (calls "recovery_note_write" create);
  let script_reply, _ = calls "run_chatml" script_request in
  assert (Document_schema.Json.equal (complete script_reply) (str "script committed"));
  let append = `Object [ "expected", selection "1"; "content", str "!append-private!" ] in
  check_ack (calls "recovery_note_append" append);
  let stale, _ = calls "recovery_note_append" append in
  expect_failure stale "recovery.conflict";
  let local, reply =
    calls "recovery_note_read" (read "2" ~start:0 ~stop:(String.length body))
  in
  assert (Document_schema.Json.equal (complete local) `Null);
  let reply = Option.value_exn reply in
  expect_json (field reply "text") (str body);
  let bad_range, _ = calls "recovery_note_read" (read "2" ~start:15 ~stop:16) in
  expect_failure bad_range "recovery.invalid_request";
  let _, listing = calls "recovery_note_list" (`Object page) in
  let listing = Option.value_exn listing in
  expect_json (field listing "catalog_revision") (str "3");
  let cursor = field listing "next" in
  assert (not (Document_schema.Json.equal cursor `Null));
  let _, second =
    calls "recovery_note_list" (`Object [ "limit", num 1; "cursor", cursor ])
  in
  let second = Option.value_exn second in
  assert (Document_schema.Json.equal (field second "next") `Null);
  let mismatched_cursor, _ =
    calls
      "recovery_note_search"
      (`Object [ "limit", num 1; "cursor", cursor; "term", str "β" ])
  in
  expect_failure mismatched_cursor "recovery.cursor_expired";
  let _, search = calls "recovery_note_search" (`Object (("term", str "β") :: page)) in
  let hit =
    match field (Option.value_exn search) "items" with
    | `Array [ hit ] -> hit
    | _ -> failwith "missing local literal result"
  in
  expect_json (field hit "start") (num 14);
  expect_json (field hit "stop") (num 16);
  check_ack (calls "recovery_note_delete" (`Object [ "expected", selection "2" ]));
  let missing, _ = calls "recovery_note_read" (read "2" ~start:0 ~stop:0) in
  expect_failure missing "recovery.note_missing";
  let expired, _ =
    calls "recovery_note_list" (`Object [ "limit", num 1; "cursor", cursor ])
  in
  expect_failure expired "recovery.cursor_expired"
;;

let%expect_test
    "notes-only ChatMD and a selected ChatML child use the protected local actor"
  =
  with_actor_workspace (fun env workspace ->
    let only_read = selected_tools env [ "recovery_note_read" ] in
    [%test_eq: int] (List.length only_read) 1;
    [%test_eq: int] (List.length (selected_tools env [])) 0;
    Eio.Switch.run (fun sw ->
      Notes.new_session
        env
        sw
        workspace
        Notes.owner
        ~name:"local-note-native"
        (fun actor _ _ ->
           let worker_failure = ref None in
           let worker =
             A.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
               let services = ref None in
               let declared =
                 selected
                   (script_registration env services :: selected_tools env effect_names)
               in
               let tools =
                 T.create
                   ~registry:(fun () -> declared)
                   ~moderator_names:String.Set.empty
                   ~now:P.Timestamp.now
                   ~is_halted:(fun () -> false)
                   ~requires_active_moderator:(fun _ -> false)
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(function
                     | Openai.Responses.Tool_output.Output.Text text -> Ok (str text)
                     | output ->
                       Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
                   ~defer_observation:(fun _ -> Ok ())
                 |> T.with_local_execution_current
                 |> fun tools -> T.with_recovery_note_service tools (local_service actor)
               in
               services := Some tools;
               let dispatch =
                 T.native_dispatch tools ~declared ~input ~capabilities:caps
               in
               let history = ref input.history in
               let count = ref 0 in
               (match
                  exercise (fun name args ->
                    try invoke ~caps dispatch history count name args with
                    | exn -> failwith (name ^ ": " ^ Exn.to_string exn))
                with
                | () -> ()
                | exception exn ->
                  worker_failure := Some exn;
                  raise exn);
               Worker.Completed
                 { final_history = !history
                 ; runtime_requests = []
                 ; moderator_snapshot = None
                 })
           in
           let attachment_id = Recovery_note_actor_tests.attach actor Read_write in
           Actor.set_operation_worker actor (Some worker) |> protocol_ok;
           Actor.start actor ~attachment_id |> protocol_ok |> ignore;
           let reservation = Actor.reserve_history_block actor ~count:1 |> protocol_ok in
           let user_entry =
             Pending_input_tests.occurrence
               (Int64.to_int_exn reservation.first_sequence)
               "recover"
           in
           Actor.submit_message_authenticated
             actor
             ~actor:
               (Recovery_note_actor_tests.proof
                  [ Send_messages; View_session_transcript ])
             ~attachment_id
             user_entry
           |> protocol_ok
           |> ignore;
           let state = await_idle actor in
           Option.iter !worker_failure ~f:raise;
           assert (Option.is_none state.recovery_windows);
           let private_values = [ body; "script-private-β-body"; "!append-private!" ] in
           let assert_public text =
             List.iter private_values ~f:(fun value ->
               assert (not (String.is_substring text ~substring:value)))
           in
           List.iter state.invocations ~f:(fun invocation ->
             if List.mem effect_names invocation.context.tool_name ~equal:String.equal
             then assert (Document_schema.Json.equal invocation.context.input `Null);
             assert_public (I.to_json invocation |> Jsonaf.to_string));
           List.iter state.conversation.canonical_history ~f:(fun entry ->
             assert_public (P.History.sexp_of_entry entry |> Sexp.to_string));
           let child =
             List.find_exn state.invocations ~f:(fun invocation ->
               P.Invocation.equal_origin invocation.context.origin Script
               && String.equal invocation.context.tool_name "recovery_note_write")
           in
           (match child.status with
            | Resolved (Complete `Null) -> ()
            | status ->
              raise_s
                [%sexp "expected authored durable acknowledgement", (status : I.status)]);
           print_endline
             "selected tools used a note catalog with no history window; invocation data \
              was body-free")));
  [%expect
    {| selected tools used a note catalog with no history window; invocation data was body-free |}]
;;

let%expect_test "local note ranges continue on scalar boundaries and plans never retarget"
  =
  let module C = Recovery_notes.Catalog in
  let module Q = A.Recovery_note_query in
  let catalog = C.empty ~owner:Notes.owner |> protocol_ok in
  let identity key =
    C.Mutation_identity.Client { principal = principal_id; key = Notes.key key }
  in
  let apply catalog name mutation =
    match C.apply catalog ~identity:(identity name) mutation with
    | Ok (Applied (catalog, receipt)) -> catalog, receipt
    | _ -> failwith "fixture mutation was not applied"
  in
  let body = String.make 4095 'x' ^ "β!" in
  let catalog, created =
    apply
      catalog
      "bounded-native-create"
      (Create
         { expected_catalog_revision = C.revision catalog
         ; path = C.Note_path.of_string "research/range" |> protocol_ok
         ; content = C.Content.of_string body |> protocol_ok
         })
  in
  let range ~start ~stop = Q.Range.create ~start ~stop |> protocol_ok in
  let read selection range = Q.prepare (Read (selection, range)) catalog |> protocol_ok in
  let selection = created.selection in
  let plan = read selection (range ~start:0 ~stop:4098) in
  let first = Q.render plan catalog |> protocol_ok in
  expect_json (field first "stop") (num 4095);
  expect_json (field first "next_offset") (num 4095);
  let continued =
    Q.render (read selection (range ~start:4095 ~stop:4098)) catalog |> protocol_ok
  in
  expect_json (field continued "text") (str "β!");
  expect_json (field continued "next_offset") `Null;
  let changed, _ =
    apply
      catalog
      "bounded-native-replace"
      (Replace
         { expected = selection; content = C.Content.of_string "new" |> protocol_ok })
  in
  assert (not (Q.current plan changed));
  assert (Result.is_error (Q.render plan changed));
  assert (Result.is_error (C.Content.of_string (String.make 65_537 'x')));
  print_endline "UTF-8 continuation and pinned revision held";
  [%expect {| UTF-8 continuation and pinned revision held |}]
;;
