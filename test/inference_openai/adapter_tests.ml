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

let with_server env handler f =
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
             "HTTP/1.1 200 OK\r\n\
              Content-Type: text/event-stream\r\n\
              Content-Length: %d\r\n\
              Connection: close\r\n\
              \r\n\
              %s"
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
         | Context_estimate _ | Configuration _ | Diagnostic _ -> assert false));
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
