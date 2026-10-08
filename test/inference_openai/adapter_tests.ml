open! Core
module D = Openai.Responses_driver
module A = Openai.Inference_adapter
module R = Inference.Request
module E = Inference.Event
module O = Inference.Observation
module Runtime = Inference_runtime
module P = History_entry.Payload

let ok result =
  Result.map_error result ~f:(fun _ -> "fixture admission") |> Result.ok_or_failwith
;;

let limits = Document_schema.Limits.default

let scope =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string "host" |> ok)
    ~attempt:(Transcript.Attempt_id.of_string "1" |> ok)
    ~relation:Root
  |> ok
;;

let accounting_id = O.Observation_id.of_string "usage" |> ok

let caps =
  D.Capability.create
    ~baseline:
      [ Text_input, Supported
      ; Image_input, Supported
      ; Function_tools, Supported
      ; Custom_tools, Supported
      ; Opaque_replay, Supported
      ; Setting "temperature", Supported
      ]
    ~models:[]
  |> ok
;;

let profile ?(defaults = []) endpoint =
  D.Profile.create
    ~id:"selected"
    ~account:(Some "account")
    ~endpoint
    ~capabilities:caps
    ~defaults
  |> ok
;;

let target profile =
  A.capture_target
    profile
    ~profile_revision:None
    ~model:"arbitrary-model"
    ~settings:[]
    ~limits
  |> ok
;;

let request ?(history = []) target =
  R.create ~target ~history ~tools:[] ~assets:[] ~limits |> ok
;;

let driver env = D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) () |> ok

let context ?(runtime_limits = Runtime.Limits.default) env profile ~target ~auth =
  A.create
    (driver env)
    ~profile
    ~profile_revision:None
    ~auth:(A.Auth_source.Static auth)
    ~limits:runtime_limits
  |> ok
  |> Runtime.Context.create ~target
  |> ok
;;

let prepare context request =
  Runtime.Context.prepare context ~preparation_id:"opaque" request |> ok
;;

let run ~sw ?(on_event = ignore) prepared =
  let attempt = Runtime.Prepared.start prepared ~scope ~accounting_id |> ok in
  Runtime.Attempt.run attempt ~sw ~on_event ~on_observation:ignore |> ok
;;

let auth ~sw:_ _ = D.Auth.bearer "test-only"

let read_request flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:2_000_000 in
  ignore (Eio.Buf_read.line reader : string);
  let rec headers length =
    let line = Eio.Buf_read.line reader in
    if String.is_empty line
    then length
    else (
      let key, value = String.lsplit2 line ~on:':' |> Option.value_exn in
      headers
        (if String.Caseless.equal key "content-length"
         then Int.of_string (String.strip value)
         else length))
  in
  let length = headers 0 in
  Eio.Buf_read.take length reader |> Jsonaf.of_string
;;

let with_server ?(status = 200) env handler f =
  Eio.Switch.run (fun sw ->
    let socket =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:4
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr socket with
      | `Tcp (_, port) -> port
      | _ -> assert false
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        let flow, _ = Eio.Net.accept ~sw socket in
        let body = read_request flow |> handler in
        Eio.Flow.copy_string
          (sprintf
             "HTTP/1.1 %d Test\r\n\
              Content-Type: text/event-stream\r\n\
              Content-Length: %d\r\n\
              Connection: close\r\n\
              \r\n\
              %s"
             status
             (String.length body)
             body)
          flow;
        Eio.Flow.close flow
      done);
    f sw (sprintf "http://127.0.0.1:%d/v1/responses" port))
;;

let frame json = "data: " ^ Jsonaf.to_string json ^ "\n\n"

let call ?(caller = []) id =
  `Object
    ([ "type", `String "function_call"
     ; "id", `String id
     ; "call_id", `String ("call-" ^ id)
     ; "name", `String "inspect"
     ; "arguments", `String " { exact } "
     ; "status", `String "completed"
     ; "future", `Object [ "number", `Number "1e+00"; "null", `Null ]
     ]
     @ caller)
;;

let message =
  Jsonaf.of_string
    {|{"type":"message","id":"message","role":"assistant","status":"completed","content":[{"type":"output_text","text":"hello","annotations":[]}]}|}
;;

let terminal ?(status = "completed") ?(usage = []) output =
  frame
    (`Object
        [ "type", `String ("response." ^ status)
        ; "sequence_number", `Number "5"
        ; ( "response"
          , `Object
              ([ "object", `String "response"
               ; "id", `String "response"
               ; "status", `String status
               ; "output", `Array output
               ]
               @ usage) )
        ])
;;

let payloads receipt =
  List.map (Runtime.Receipt.output receipt) ~f:(fun event ->
    match E.view event with
    | Candidate_ready { payload; _ } -> payload
    | Live _ | Terminal _ -> assert false)
;;

let raw payload =
  match P.representation payload with
  | Captured { raw; _ } -> raw
  | Authored | Reconstructed _ -> assert false
;;

let%expect_test
    "real adapter preserves opaque calls but only eligible caller reaches tool evidence"
  =
  Eio_main.run (fun env ->
    let safe = call "safe" in
    let future =
      call "future" ~caller:[ "caller", `Object [ "type", `String "future.caller" ] ]
    in
    with_server
      env
      (fun _ -> terminal [ safe; future ])
      (fun sw endpoint ->
         let profile = profile endpoint in
         let target = target profile in
         let context = context env profile ~target ~auth in
         let receipt = prepare context (request target) |> run ~sw in
         List.iter (Runtime.Receipt.output receipt) ~f:(fun event ->
           match E.view event with
           | Candidate_ready { local_execution; _ } ->
             print_s [%sexp (local_execution : E.local_execution)]
           | Live _ | Terminal _ -> assert false);
         List.iter2_exn [ safe; future ] (payloads receipt) ~f:(fun expected payload ->
           assert (Document_schema.Json.equal expected (raw payload)));
         print_endline "exact raw unknown fields retained"));
  [%expect
    {|
    Tool_candidate
    Not_eligible
    exact raw unknown fields retained
    |}]
;;

let%expect_test "streamed completed call reconciles once with response array order" =
  Eio_main.run (fun env ->
    let call = call "call" in
    let done_ =
      frame
        (`Object
            [ "type", `String "response.output_item.done"
            ; "sequence_number", `Number "1"
            ; "output_index", `Number "1"
            ; "item", call
            ])
    in
    with_server
      env
      (fun _ -> done_ ^ terminal [ message; call ])
      (fun sw endpoint ->
         let profile = profile endpoint in
         let target = target profile in
         let context = context env profile ~target ~auth in
         let observed = ref [] in
         let on_event event =
           match E.view event with
           | Candidate_ready { item; _ } ->
             observed := Transcript.Item_id.to_string item.id :: !observed
           | Terminal _ -> observed := "terminal" :: !observed
           | Live _ -> ()
         in
         let receipt = prepare context (request target) |> run ~sw ~on_event in
         print_s [%sexp (List.rev !observed : string list)];
         List.iter2_exn [ message; call ] (payloads receipt) ~f:(fun expected payload ->
           assert (Document_schema.Json.equal expected (raw payload)))));
  [%expect {| (output:1 output:0 terminal) |}]
;;

let%expect_test
    "captured selection omits current profile defaults and raw replay stays exact"
  =
  Eio_main.run (fun env ->
    let captured_call =
      call "direct" ~caller:[ "caller", `Object [ "type", `String "direct" ] ]
    in
    let requests = ref [] in
    with_server
      env
      (fun request ->
         requests := request :: !requests;
         terminal [ captured_call ])
      (fun sw endpoint ->
         let original = profile endpoint in
         let selected = target original in
         let default =
           D.Setting.create
             ~name:"temperature"
             ~value:(Value (`Number "0.8"))
             ~provenance:Profile_default
           |> ok
         in
         let updated = profile ~defaults:[ default ] endpoint in
         let context = context env updated ~target:selected ~auth in
         let first = prepare context (request selected) |> run ~sw in
         let payload = List.hd_exn (payloads first) in
         let entry =
           History_entry.create_with_id
             ~id:(History_entry.Id.create ~namespace:"host" ~sequence:0 |> ok)
             payload
         in
         ignore (prepare context (request ~history:[ entry ] selected) |> run ~sw);
         let second = List.hd_exn !requests in
         assert (
           Document_schema.Json.equal
             (`Array [ captured_call ])
             (match Document_schema.Json.field second ~name:"input" with
              | Value json -> json
              | _ -> assert false));
         List.iter !requests ~f:(fun request ->
           assert (
             match Document_schema.Json.field request ~name:"temperature" with
             | Absent -> true
             | Null | Value _ -> false));
         let wrong_semantic =
           P.Semantic.create
             (Message
                { form = Output
                ; role = Assistant
                ; content =
                    [ Text { text = "tampered"; annotations = []; logprobs = Absent } ]
                ; phase = Absent
                })
             ~metadata:(P.Semantic.metadata (P.semantic payload))
           |> ok
         in
         let origin =
           match P.representation payload with
           | Captured { origin; _ } -> origin
           | _ -> assert false
         in
         let forged = P.captured wrong_semantic ~origin ~raw:captured_call |> ok in
         let bad = History_entry.with_payload entry forged in
         assert (
           Result.is_error
             (Runtime.Context.prepare
                context
                ~preparation_id:"bad"
                (request ~history:[ bad ] selected)));
         print_endline
           "frozen omission; exact replay; tampered semantic rejected before dispatch"));
  [%expect
    {| frozen omission; exact replay; tampered semantic rejected before dispatch |}]
;;

let%expect_test "provider usage preserves actual zero, explicit null and missing details" =
  Eio_main.run (fun env ->
    let usage =
      [ ( "usage"
        , Jsonaf.of_string
            {|{"input_tokens":0,"output_tokens":3,"total_tokens":3,"input_tokens_details":{"cached_tokens":null},"output_tokens_details":{"reasoning_tokens":2}}|}
        )
      ]
    in
    with_server
      env
      (fun _ -> terminal ~usage [])
      (fun sw endpoint ->
         let profile = profile endpoint in
         let target = target profile in
         let context = context env profile ~target ~auth in
         let receipt = prepare context (request target) |> run ~sw in
         match O.payload (Runtime.Receipt.usage receipt) with
         | Usage usage ->
           List.iter
             O.Usage.Component.
               [ Input; Cached_input; Cache_write_input; Reasoning_output ]
             ~f:(fun component ->
               match O.Count.view (O.Usage.count usage component) with
               | Actual n -> printf "actual:%Ld\n" n
               | Unknown reason -> print_s [%sexp (reason : O.Count.unknown_reason)]
               | Estimated _ -> assert false)
         | Context_estimate _ | Configuration _ | Transport_selection _ | Diagnostic _ ->
           assert false));
  [%expect
    {|
    actual:0
    Explicit_null
    Not_reported
    actual:2
    |}]
;;

let%expect_test "auth failure has unknown unsubmitted usage and no candidate" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let profile = profile "http://127.0.0.1:1/v1/responses" in
      let target = target profile in
      let calls = ref 0 in
      let auth ~sw:_ _ =
        incr calls;
        Error D.Auth.Missing
      in
      let context = context env profile ~target ~auth in
      let prepared = prepare context (request target) in
      assert (!calls = 0);
      let receipt = run ~sw prepared in
      assert (!calls = 1 && List.is_empty (Runtime.Receipt.output receipt));
      print_s
        [%sexp
          (E.Terminal.outcome (Runtime.Receipt.terminal receipt) : E.Terminal.outcome)];
      match O.payload (Runtime.Receipt.usage receipt) with
      | Usage usage ->
        (match O.Count.view (O.Usage.count usage Input) with
         | Unknown Not_submitted -> print_endline "unknown, not submitted"
         | _ -> assert false)
      | _ -> assert false));
  [%expect
    {|
    (Failed (Authentication Missing))
    unknown, not submitted
    |}]
;;

let%expect_test "actual configured output limits stop projection before excess candidates"
  =
  Eio_main.run (fun env ->
    with_server
      env
      (fun _ -> terminal [ call "first"; call "second" ])
      (fun sw endpoint ->
         let runtime_limits =
           Runtime.Limits.create
             ~event_limits:Transcript.Admission.default
             ~max_candidates:1
             ~max_evidence_bytes:65536
           |> ok
         in
         let profile = profile endpoint in
         let target = target profile in
         let context = context ~runtime_limits env profile ~target ~auth in
         let candidates = ref 0 in
         let terminals = ref 0 in
         let rejected =
           try
             ignore
               (prepare context (request target)
                |> run ~sw ~on_event:(fun event ->
                  match E.view event with
                  | Candidate_ready _ -> incr candidates
                  | Terminal _ -> incr terminals
                  | Live _ -> ())
                : Runtime.Receipt.t);
             false
           with
           | Runtime.Contract_violation Evidence_limit -> true
         in
         printf "rejected=%b candidates=%d terminals=%d\n" rejected !candidates !terminals));
  [%expect {| rejected=true candidates=1 terminals=0 |}]
;;

let%expect_test
    "bounded auxiliary driver stops raw response bytes before output publication"
  =
  Eio_main.run (fun env ->
    with_server
      env
      (fun _ -> terminal [ message ])
      (fun sw endpoint ->
         let profile = profile endpoint in
         let target = target profile in
         let driver = D.with_response_limit (driver env) ~max_body_bytes:128 |> ok in
         let driver = D.with_response_limit driver ~max_body_bytes:100000 |> ok in
         let context =
           A.create
             driver
             ~profile
             ~profile_revision:None
             ~auth:(A.Auth_source.Static auth)
             ~limits:Runtime.Limits.default
           |> ok
           |> Runtime.Context.create ~target
           |> ok
         in
         let receipt = prepare context (request target) |> run ~sw in
         assert (List.is_empty (Runtime.Receipt.output receipt));
         print_s
           [%sexp
             (E.Terminal.outcome (Runtime.Receipt.terminal receipt) : E.Terminal.outcome)]));
  [%expect {| (Failed (Transport Body_limit)) |}]
;;

let%expect_test "authored document semantic kind cannot relabel a different input shape" =
  Eio_main.run (fun env ->
    let profile = profile "http://127.0.0.1:1/v1/responses" in
    let target = target profile in
    let calls = ref 0 in
    let context =
      context env profile ~target ~auth:(fun ~sw:_ _ ->
        incr calls;
        Error D.Auth.Missing)
    in
    let semantic =
      P.Semantic.create
        (Message
           { form = Input
           ; role = User
           ; phase = Absent
           ; content =
               [ Unknown
                   { kind = "input_file"
                   ; raw =
                       `Object
                         [ "type", `String "input_text"; "text", `String "mismatched" ]
                   }
               ]
           })
        ~metadata:P.Metadata.empty
      |> ok
    in
    let entry =
      History_entry.create_with_id
        ~id:(History_entry.Id.create ~namespace:"input" ~sequence:0 |> ok)
        (P.authored semantic)
    in
    let rejected =
      Result.is_error
        (Runtime.Context.prepare
           context
           ~preparation_id:"mismatch"
           (request ~history:[ entry ] target))
    in
    printf "rejected=%b auth_calls=%d\n" rejected !calls);
  [%expect {| rejected=true auth_calls=0 |}]
;;

let%expect_test "reconstructed images require immutable inline data before authentication"
  =
  Eio_main.run (fun env ->
    let profile = profile "http://127.0.0.1:1/v1/responses" in
    let target = target profile in
    let auth_calls = ref 0 in
    let context =
      context env profile ~target ~auth:(fun ~sw:_ _ ->
        incr auth_calls;
        Error D.Auth.Missing)
    in
    List.iter
      [ "data:image/png;base64,eA=="; "https://remote.example/image.png" ]
      ~f:(fun uri ->
        let item =
          Openai.Responses.Item.t_of_jsonaf
            (`Object
                [ "type", `String "message"
                ; "role", `String "user"
                ; ( "content"
                  , `Array
                      [ `Object
                          [ "type", `String "input_image"
                          ; "image_url", `String uri
                          ; "detail", `String "auto"
                          ]
                      ] )
                ])
        in
        let payload = Openai.Responses_history.of_item item |> ok in
        let entry =
          History_entry.create_with_id
            ~id:(History_entry.Id.create ~namespace:"input" ~sequence:0 |> ok)
            payload
        in
        let result =
          Runtime.Context.prepare
            context
            ~preparation_id:"image"
            (request ~history:[ entry ] target)
          |> Result.map ~f:(fun _ -> ())
        in
        print_s [%sexp (result : (unit, Runtime.Preparation_error.t) Result.t)]);
    printf "auth_calls=%d\n" !auth_calls);
  [%expect
    {|
    (Ok ())
    (Error Unsupported_input)
    auth_calls=0
    |}]
;;

let%expect_test "retained model changes preflight without auth and replay original bytes" =
  Eio_main.run (fun env ->
    let received = ref [] in
    let auth_calls = ref 0 in
    let auth ~sw:_ _ =
      incr auth_calls;
      D.Auth.bearer "test-only"
    in
    with_server
      env
      (fun request ->
         received := request :: !received;
         terminal [ message ])
      (fun sw endpoint ->
         let base = profile endpoint in
         let selected = target base in
         let source = context env base ~target:selected ~auth in
         let first = prepare source (request selected) |> run ~sw in
         let payload = List.hd_exn (payloads first) in
         let entry =
           History_entry.create_with_id
             ~id:(History_entry.Id.create ~namespace:"switch" ~sequence:0 |> ok)
             payload
         in
         let replay =
           Openai.Responses_replay.create
             ~transitions:[ "arbitrary-model", "next-model", [ Assistant_text ] ]
           |> ok
         in
         let profile = D.Profile.with_replay_policy base replay in
         let next =
           A.capture_target
             profile
             ~profile_revision:None
             ~model:"next-model"
             ~settings:[]
             ~limits
           |> ok
         in
         let next_context = context env profile ~target:next ~auth in
         Runtime.Context.preflight_history next_context [ entry ] |> ok;
         assert (!auth_calls = 1);
         ignore (prepare next_context (request ~history:[ entry ] next) |> run ~sw);
         let input = Document_schema.Json.field (List.hd_exn !received) ~name:"input" in
         (match input with
          | Value actual ->
            assert (Document_schema.Json.equal actual (`Array [ raw payload ]))
          | _ -> assert false);
         let unknown =
           match raw payload with
           | `Object fields -> `Object (fields @ [ "future", `Null ])
           | _ -> assert false
         in
         let origin =
           match P.representation payload with
           | Captured { origin; _ } -> origin
           | _ -> assert false
         in
         let unknown_payload =
           P.captured (P.semantic payload) ~origin ~raw:unknown |> ok
         in
         let unknown_entry = History_entry.with_payload entry unknown_payload in
         (match Runtime.Context.preflight_history next_context [ unknown_entry ] with
          | Error Incompatible_replay -> ()
          | _ -> assert false);
         (match
            Runtime.Context.prepare
              next_context
              ~preparation_id:"refused"
              (request ~history:[ unknown_entry ] next)
          with
          | Error Incompatible_replay -> ()
          | _ -> assert false);
         Runtime.Context.preflight_history source [ unknown_entry ] |> ok;
         assert (!auth_calls = 2);
         assert (List.length !received = 2);
         assert (Document_schema.Json.equal (raw unknown_payload) unknown)));
  print_endline
    "pure admission agrees with dispatch; preserved raw bytes and actual origin; refusal \
     performs no auth/network";
  [%expect
    {| pure admission agrees with dispatch; preserved raw bytes and actual origin; refusal performs no auth/network |}]
;;

let%expect_test
    "preflight checks authored and reconstructed features while deferring assets"
  =
  Eio_main.run (fun env ->
    let capabilities =
      D.Capability.create
        ~baseline:
          [ Text_input, Supported; Image_input, Supported; Function_tools, Unsupported ]
        ~models:[]
      |> ok
    in
    let profile =
      D.Profile.create
        ~id:"selected"
        ~account:(Some "account")
        ~endpoint:"https://api.openai.com/v1/responses"
        ~capabilities
        ~defaults:[]
      |> ok
    in
    let target = target profile in
    let auth_calls = ref 0 in
    let context =
      context env profile ~target ~auth:(fun ~sw:_ _ ->
        incr auth_calls;
        D.Auth.bearer "test-only")
    in
    let id n = History_entry.Id.create ~namespace:"preflight" ~sequence:n |> ok in
    let call =
      P.Semantic.create
        (Call
           { kind = Function
           ; name = "tool"
           ; namespace = Absent
           ; input_bytes = "{}"
           ; async = Absent
           })
        ~metadata:{ P.Metadata.empty with call_id = Value "call" }
      |> ok
    in
    let authored = History_entry.create_with_id ~id:(id 0) (P.authored call) in
    let reconstructed =
      Openai.Responses_history.create_with_id_exn
        ~id:(id 1)
        (Openai.Responses.Item.Function_call
           { name = "tool"
           ; arguments = "{}"
           ; call_id = "call"
           ; _type = "function_call"
           ; id = None
           ; status = None
           })
    in
    List.iter [ authored; reconstructed ] ~f:(fun entry ->
      match Runtime.Context.preflight_history context [ entry ] with
      | Error Incompatible_replay -> ()
      | _ -> assert false);
    let image =
      P.Semantic.create
        (Message
           { form = Input
           ; role = User
           ; content = [ Image { uri = "asset:local-image"; detail = Absent } ]
           ; phase = Absent
           })
        ~metadata:P.Metadata.empty
      |> ok
      |> P.authored
      |> History_entry.create_with_id ~id:(id 2)
    in
    Runtime.Context.preflight_history context [ image ] |> ok;
    (match
       Runtime.Context.prepare
         context
         ~preparation_id:"resolve-later"
         (request ~history:[ image ] target)
     with
     | Error Asset_unavailable -> ()
     | _ -> assert false);
    assert (!auth_calls = 0));
  print_endline
    "unsupported authored/reconstructed tools refused; image capability admitted but \
     actual asset still required";
  [%expect
    {| unsupported authored/reconstructed tools refused; image capability admitted but actual asset still required |}]
;;

let%expect_test
    "actual adapter protocol diagnostic is scoped persisted and precedes failure"
  =
  Eio_main.run (fun env ->
    let module L = Agent_session.Inference_ledger in
    let created =
      frame
        (`Object
            [ "type", `String "response.created"
            ; "sequence_number", `Number "0"
            ; ( "response"
              , `Object
                  [ "id", `String "response"
                  ; "object", `String "response"
                  ; "status", `String "in_progress"
                  ; "output", `Array []
                  ] )
            ])
    in
    let malformed =
      terminal ~status:"failed" [ message ]
      |> fun value ->
      String.substr_replace_all
        value
        ~pattern:"\"status\":\"failed\""
        ~with_:"\"status\":\"completed\""
    in
    let reasoning value =
      `Object
        [ "type", `String "reasoning"
        ; "id", `String "PRIVATE_REASONING_ID"
        ; "summary", `Array []
        ; "encrypted_content", `String value
        ]
    in
    let done_ =
      frame
        (`Object
            [ "type", `String "response.output_item.done"
            ; "sequence_number", `Number "1"
            ; "output_index", `Number "0"
            ; "item", reasoning "PRIVATE_EARLY_CIPHERTEXT"
            ])
    in
    let snapshot_mismatch =
      created
      ^ done_
      ^ terminal
          [ (match reasoning "PRIVATE_FINAL_CIPHERTEXT" with
             | `Object fields ->
               `Object
                 (List.map fields ~f:(fun (name, old) ->
                    ( name
                    , if String.equal name "summary"
                      then
                        `Array
                          [ `Object
                              [ "type", `String "summary_text"
                              ; "text", `String "PRIVATE_CHANGED_SUMMARY"
                              ]
                          ]
                      else old )))
             | _ -> assert false)
          ]
    in
    List.iter
      [ created ^ malformed; snapshot_mismatch ]
      ~f:(fun body ->
        with_server
          env
          (fun _ -> body)
          (fun sw endpoint ->
             let profile = profile endpoint in
             let target = target profile in
             let prepared =
               prepare (context env profile ~target ~auth) (request target)
             in
             let ledger =
               L.create
                 ~session_id:
                   (Agent_protocol.Id.Session.of_string "ses_adapter_protocol" |> ok)
                 ~generation:0
                 ~before_tracking_unknown:false
                 ~limits:L.Limits.default
               |> ok
             in
             let ledger, handle, _ =
               L.admit
                 ledger
                 ~source:(Transcript.Source_id.of_string "actual-adapter" |> ok)
                 ~relation:Root
                 ~operation_id:None
                 ~invocation_id:None
                 ~configuration:(Runtime.Prepared.configuration prepared)
               |> ok
             in
             let ledger = ref ledger in
             let order = ref [] in
             let attempt =
               Runtime.Prepared.start
                 prepared
                 ~scope:(L.Handle.scope handle)
                 ~accounting_id:(L.Handle.accounting_id handle)
               |> ok
             in
             let receipt =
               Runtime.Attempt.run
                 attempt
                 ~sw
                 ~on_event:(fun event ->
                   match E.view event with
                   | Terminal _ -> order := "terminal" :: !order
                   | _ -> ())
                 ~on_observation:(fun observation ->
                   (match O.payload observation with
                    | Diagnostic _ -> order := "diagnostic" :: !order
                    | _ -> ());
                   ledger := fst (L.observe !ledger handle observation |> ok))
               |> ok
             in
             ledger
             := L.set_state !ledger handle (Terminal (Runtime.Receipt.terminal receipt))
                |> ok;
             let document = L.to_document !ledger |> ok in
             let restored = L.of_document document ~limits:L.Limits.default |> ok in
             let diagnostics =
               O.Attempt_record.observations
                 (L.Row.record (List.hd_exn (L.rows restored)))
               |> List.filter_map ~f:(fun observation ->
                 match O.payload observation with
                 | Diagnostic diagnostic -> Some diagnostic
                 | _ -> None)
             in
             assert (List.length diagnostics = 1);
             let diagnostic = List.hd_exn diagnostics in
             print_s
               [%sexp
                 (O.Diagnostic.reason diagnostic : O.Diagnostic.reason)
               , (O.Diagnostic.delivery diagnostic : E.Terminal.delivery option)];
             print_s [%sexp (List.rev !order : string list)];
             print_s
               [%sexp
                 (E.Terminal.outcome (Runtime.Receipt.terminal receipt)
                  : E.Terminal.outcome)];
             let wire =
               O.Diagnostic.Protocol_violation.to_json
                 (match O.Diagnostic.reason diagnostic with
                  | Protocol_violation value -> value
                  | _ -> assert false)
               |> Jsonaf.to_string
             in
             assert (not (String.is_substring wire ~substring:"PRIVATE")))));
  [%expect
    {|
((Protocol_violation ((stage Feed) (kind (Tracker Terminal_mismatch))))
 (Response_started))
(diagnostic terminal)
(Failed (Transport Protocol))
((Protocol_violation
  ((stage Feed)
   (kind
    (Item_conflict
     ((event Terminal) (cause Final_snapshot_changed)
      (fields (Summary Encrypted_content)))))))
 (Response_started))
(diagnostic terminal)
(Failed (Transport Protocol))
|}]
;;

let%expect_test
    "terminal reasoning representation drift preserves actual primary candidate \
     canonical restore and replay"
  =
  Eio_main.run (fun env ->
    let reasoning encrypted =
      `Object
        [ "type", `String "reasoning"
        ; "id", `String "reasoning"
        ; "summary", `Array []
        ; "encrypted_content", `String encrypted
        ]
    in
    let primary = reasoning "PRIMARY_DONE_OPAQUE" in
    let terminal_snapshot = reasoning "TERMINAL_SNAPSHOT_OPAQUE" in
    let done_ =
      frame
        (`Object
            [ "type", `String "response.output_item.done"
            ; "sequence_number", `Number "1"
            ; "output_index", `Number "0"
            ; "item", primary
            ])
    in
    let requests = ref [] in
    with_server
      env
      (fun body ->
         requests := body :: !requests;
         if List.length !requests = 1
         then done_ ^ terminal [ terminal_snapshot ]
         else terminal [])
      (fun sw endpoint ->
         let profile = profile endpoint in
         let target = target profile in
         let context = context env profile ~target ~auth in
         let prepared = prepare context (request target) in
         let early = ref [] in
         let attempt = Runtime.Prepared.start prepared ~scope ~accounting_id |> ok in
         let receipt =
           Runtime.Attempt.run attempt ~sw ~on_observation:ignore ~on_event:(fun event ->
             match E.view event with
             | Candidate_ready { payload; _ } -> early := payload :: !early
             | Live _ | Terminal _ -> ())
           |> ok
         in
         assert (
           E.Terminal.equal_outcome
             (E.Terminal.outcome (Runtime.Receipt.terminal receipt))
             Completed);
         assert (List.length !early = 1);
         let payload = List.hd_exn (payloads receipt) in
         assert (Document_schema.Json.equal primary (raw payload));
         assert (Document_schema.Json.equal (raw (List.hd_exn !early)) (raw payload));
         assert (
           Runtime.Receipt.equal_output_coverage
             (Runtime.Receipt.output_coverage receipt)
             Response_output);
         let entry =
           History_entry.create_with_id
             ~id:(History_entry.Id.create ~namespace:"restore" ~sequence:0 |> ok)
             payload
         in
         let canonical =
           Agent_session.History_codec.to_canonical entry
           |> Agent_protocol.History.entry_to_json
           |> Jsonaf.to_string
           |> Jsonaf.of_string
           |> Agent_protocol.History.entry_of_json
           |> ok
         in
         let restored = Agent_session.History_codec.of_canonical canonical |> ok in
         assert (Document_schema.Json.equal primary (raw (History_entry.payload restored)));
         ignore (prepare context (request ~history:[ restored ] target) |> run ~sw);
         let subsequent = List.hd_exn !requests in
         let decoded = Openai.Responses_request.of_jsonaf subsequent |> Or_error.ok_exn in
         let roundtrip = Openai.Responses_request.to_jsonaf decoded in
         let input =
           match Document_schema.Json.field roundtrip ~name:"input" with
           | Value value -> value
           | _ -> assert false
         in
         assert (Document_schema.Json.equal input (`Array [ primary ]));
         print_endline
           "exact item.done candidate and canonical bytes restored; next actual request \
            replays primary ciphertext"));
  [%expect
    {| exact item.done candidate and canonical bytes restored; next actual request replays primary ciphertext |}]
;;

let%test_unit "actual HTTP rejection diagnostic survives ledger roundtrip without body" =
  Eio_main.run (fun env ->
    with_server
      ~status:400
      env
      (fun _ -> {|{"detail":"Instructions are required","private":"SECRET_CANARY"}|})
      (fun sw endpoint ->
         let module L = Agent_session.Inference_ledger in
         let profile = profile endpoint in
         let target = target profile in
         let prepared = prepare (context env profile ~target ~auth) (request target) in
         let ledger =
           L.create
             ~session_id:(Agent_protocol.Id.Session.of_string "ses_http_diagnostic" |> ok)
             ~generation:0
             ~before_tracking_unknown:false
             ~limits:L.Limits.default
           |> ok
         in
         let ledger, handle, _ =
           L.admit
             ledger
             ~source:(Transcript.Source_id.of_string "actual-adapter" |> ok)
             ~relation:Root
             ~operation_id:None
             ~invocation_id:None
             ~configuration:(Runtime.Prepared.configuration prepared)
           |> ok
         in
         let ledger = ref ledger
         and order = ref [] in
         let attempt =
           Runtime.Prepared.start
             prepared
             ~scope:(L.Handle.scope handle)
             ~accounting_id:(L.Handle.accounting_id handle)
           |> ok
         in
         let receipt =
           Runtime.Attempt.run
             attempt
             ~sw
             ~on_event:(fun event ->
               match E.view event with
               | Terminal _ -> order := "terminal" :: !order
               | _ -> ())
             ~on_observation:(fun observation ->
               (match O.payload observation with
                | Diagnostic _ -> order := "diagnostic" :: !order
                | _ -> ());
               ledger := fst (L.observe !ledger handle observation |> ok))
           |> ok
         in
         ledger
         := L.set_state !ledger handle (Terminal (Runtime.Receipt.terminal receipt)) |> ok;
         let restored =
           L.of_document (L.to_document !ledger |> ok) ~limits:L.Limits.default |> ok
         in
         let observations =
           O.Attempt_record.observations (L.Row.record (List.hd_exn (L.rows restored)))
         in
         let diagnostics =
           List.filter_map observations ~f:(fun o ->
             match O.payload o with
             | Diagnostic d -> Some d
             | _ -> None)
         in
         assert (List.equal String.equal (List.rev !order) [ "diagnostic"; "terminal" ]);
         match diagnostics with
         | [ diagnostic ] ->
           (match O.Diagnostic.reason diagnostic with
            | Http_rejection rejection ->
              assert (
                O.Diagnostic.Http_rejection.equal_reason
                  (O.Diagnostic.Http_rejection.reason rejection)
                  Missing_required_parameter);
              assert (O.Diagnostic.equal_phase (O.Diagnostic.phase diagnostic) Dispatch)
            | _ -> assert false);
           assert (
             not
               (String.is_substring
                  (Jsonaf.to_string
                     (O.Attempt_record.to_json
                        (L.Row.record (List.hd_exn (L.rows restored)))))
                  ~substring:"SECRET_CANARY"));
           assert (
             Option.equal
               E.Terminal.equal_delivery
               (O.Diagnostic.delivery diagnostic)
               (Some Possibly_submitted))
         | _ -> assert false))
;;
