open! Core
module P = Provider_runtime_host.Profile_policy
module D = Openai.Responses_driver

let%expect_test "transport policy accepts only explicit bounded choices" =
  List.iter
    [ "sse"; "prefer-websocket"; "require-websocket"; "websocket"; "SSE"; "" ]
    ~f:(fun text ->
      match P.Transport_policy.of_string text with
      | Ok policy -> printf "%s\n" (P.Transport_policy.to_string policy)
      | Error _ -> print_endline "invalid");
  [%expect
    {|
    sse
    prefer-websocket
    require-websocket
    invalid
    invalid
    invalid
    |}]
;;

let%expect_test
    "unqualified models and endpoint overrides cannot acquire WebSocket support"
  =
  List.iter [ P.Public_api; Direct_codex ] ~f:(fun route ->
    List.iter
      [ P.endpoint route; "https://unqualified.example/v1/responses" ]
      ~f:(fun endpoint ->
        let capabilities = P.capabilities route ~endpoint |> Or_error.ok_exn in
        List.iter
          [ "ochat-test-unlisted-alpha"; "ochat-test-unlisted-beta" ]
          ~f:(fun model ->
            assert (
              D.Capability.equal_support
                (D.Capability.resolve capabilities ~model ~feature:Websocket)
                Unknown)));
    printf "%s: all unqualified\n" (Sexp.to_string (P.sexp_of_route route)));
  let capabilities =
    P.capabilities Direct_codex ~endpoint:(P.endpoint Direct_codex) |> Or_error.ok_exn
  in
  List.iter [ "temperature"; "top_p"; "max_output_tokens" ] ~f:(fun name ->
    print_s
      [%sexp
        (D.Capability.resolve
           capabilities
           ~model:"ochat-test-unlisted-alpha"
           ~feature:(Setting name)
         : D.Capability.support)]);
  [%expect
    {|
    Public_api: all unqualified
    Direct_codex: all unqualified
    Unsupported
    Unsupported
    Unsupported
    |}]
;;

let%expect_test "WebSocket qualification is exact model route and endpoint" =
  let support route endpoint model =
    P.capabilities route ~endpoint
    |> Or_error.ok_exn
    |> fun capabilities -> D.Capability.resolve capabilities ~model ~feature:Websocket
  in
  List.iter
    [ P.Public_api, P.endpoint Public_api, "gpt-6-luna"
    ; Public_api, P.endpoint Public_api, "gpt-6-luna-preview"
    ; Public_api, P.endpoint Public_api, "gpt-6-luna-extra"
    ; Public_api, P.endpoint Public_api, "GPT-6-LUNA"
    ; Public_api, P.endpoint Public_api, "gpt-6"
    ; Public_api, "https://api.openai.com/v1/responses/", "gpt-6-luna"
    ; Public_api, "https://unqualified.example/v1/responses", "gpt-6-luna"
    ; Direct_codex, P.endpoint Direct_codex, "gpt-6-luna"
    ; Direct_codex, P.endpoint Direct_codex, "gpt-6-luna-preview"
    ; Direct_codex, P.endpoint Direct_codex, "GPT-6-LUNA"
    ; Direct_codex, P.endpoint Direct_codex ^ "/", "gpt-6-luna"
    ; Direct_codex, P.endpoint Public_api, "gpt-6-luna"
    ]
    ~f:(fun (route, endpoint, model) ->
      print_s [%sexp (support route endpoint model : D.Capability.support)]);
  [%expect
    {|
    Supported
    Unknown
    Unknown
    Unknown
    Unknown
    Unknown
    Unknown
    Supported
    Unknown
    Unknown
    Unknown
    Unknown
    |}]
;;

let%expect_test
    "shipping required WebSocket refuses before credential acquisition or network"
  =
  Eio_mock.Backend.run_full (fun env ->
    let driver =
      D.create
        ~net:(Eio_mock.Net.make "must-remain-unused")
        ~clock:(Eio.Stdenv.clock env)
        ()
      |> Or_error.ok_exn
    in
    List.iter
      [ P.Public_api, P.endpoint Public_api, "ochat-test-unlisted-alpha"
      ; Direct_codex, P.endpoint Direct_codex, "ochat-test-unlisted-alpha"
      ; Public_api, "https://api.openai.com/v1/responses/", "gpt-6-luna"
      ; Direct_codex, P.endpoint Direct_codex, "gpt-6-luna-preview"
      ]
      ~f:(fun (route, endpoint, model) ->
        let profile =
          D.Profile.create
            ~id:"qualified-route-unqualified-model"
            ~account:None
            ~endpoint
            ~capabilities:(P.capabilities route ~endpoint |> Or_error.ok_exn)
            ~defaults:[]
          |> Or_error.ok_exn
        in
        let prepared =
          D.Prepared.create profile ~model ~history:[] ~tools:[] ~settings:[]
          |> Or_error.ok_exn
        in
        let acquired = ref 0 in
        let events = ref 0 in
        let selected = ref 0 in
        let result =
          D.run_with_transport
            driver
            ~session:None
            ~policy:Require_websocket
            ~auth:(fun ~sw:_ _ ->
              incr acquired;
              D.Auth.bearer "synthetic-only")
            ~prepared
            ~on_selected:(fun _ _ -> incr selected)
            ~on_event:(fun _ -> incr events)
        in
        (match result with
         | Ok
             (D.Terminal.Failed
                { delivery = Definitely_not_submitted; reason = Unsupported_transport })
           -> ()
         | Error _ | Ok _ -> failwith "unqualified WebSocket admission");
        printf
          "credentials:%d selected:%d terminal-events:%d\n"
          !acquired
          !selected
          !events));
  [%expect
    {|
    credentials:0 selected:0 terminal-events:1
    credentials:0 selected:0 terminal-events:1
    credentials:0 selected:0 terminal-events:1
    credentials:0 selected:0 terminal-events:1
    |}]
;;

let%expect_test
    "direct application policy refuses selected controls instead of stripping them"
  =
  let setting =
    D.Setting.create
      ~name:"max_output_tokens"
      ~value:(Value (`Number "17"))
      ~provenance:Captured_prompt
    |> Or_error.ok_exn
  in
  List.iter [ P.Public_api; Direct_codex ] ~f:(fun route ->
    let profile =
      D.Profile.create
        ~id:"selected-control"
        ~account:None
        ~endpoint:(P.endpoint route)
        ~capabilities:
          (P.capabilities route ~endpoint:(P.endpoint route) |> Or_error.ok_exn)
        ~defaults:[]
      |> Or_error.ok_exn
    in
    match
      D.Prepared.create
        profile
        ~model:"synthetic"
        ~history:[]
        ~tools:[]
        ~settings:[ setting ]
    with
    | Error error ->
      assert (P.equal_route route Direct_codex);
      assert (
        String.equal
          (Error.to_string_hum error)
          "unsupported capability: (Setting max_output_tokens)");
      print_endline "direct: selected control refused"
    | Ok prepared ->
      assert (P.equal_route route Public_api);
      (match D.Prepared.settings prepared with
       | [ preserved ] ->
         assert (
           Jsonaf.exactly_equal
             (match D.Setting.value preserved with
              | Value value -> value
              | _ -> assert false)
             (`Number "17"))
       | _ -> failwith "selected setting stripped");
      print_endline "API: selected control preserved");
  [%expect
    {|
    API: selected control preserved
    direct: selected control refused
    |}]
;;

let%expect_test
    "request encoding follows selected route without changing profile identity"
  =
  List.iter [ P.Public_api; Direct_codex ] ~f:(fun route ->
    let capabilities =
      P.capabilities route ~endpoint:(P.endpoint route) |> Or_error.ok_exn
    in
    let default =
      D.Setting.create
        ~name:"instructions"
        ~value:(Value (`String "fixed policy default"))
        ~provenance:Profile_default
      |> Or_error.ok_exn
    in
    let profile =
      D.Profile.create
        ~id:"encoding-policy"
        ~account:(Some "nonsecret-account")
        ~endpoint:(P.endpoint route)
        ~capabilities
        ~defaults:[ default ]
      |> Or_error.ok_exn
    in
    let selected = P.apply_endpoint_policy profile ~route in
    assert (String.equal (D.Profile.id profile) (D.Profile.id selected));
    assert (
      Option.equal String.equal (D.Profile.account profile) (D.Profile.account selected));
    assert (String.equal (D.Profile.endpoint profile) (D.Profile.endpoint selected));
    let before = D.Profile.effective_settings profile [] |> Or_error.ok_exn in
    let after = D.Profile.effective_settings selected [] |> Or_error.ok_exn in
    List.iter [ before; after ] ~f:(function
      | [ setting ] ->
        assert (String.equal (D.Setting.name setting) "instructions");
        assert (D.Setting.equal_provenance (D.Setting.provenance setting) Profile_default);
        (match D.Setting.value setting with
         | Value (`String value) -> assert (String.equal value "fixed policy default")
         | _ -> assert false)
      | _ -> assert false);
    assert (
      D.Capability.equal_support
        (D.Profile.capability profile ~model:"gpt-6-luna" ~feature:Text_input)
        (D.Profile.capability selected ~model:"gpt-6-luna" ~feature:Text_input));
    let emission =
      match D.Profile.truncation_emission selected with
      | Openai.Responses_request.Truncation_emission.Explicit_disabled ->
        "explicit-disabled"
      | Omit -> "omitted"
    in
    let response_policy =
      match D.Profile.response_content_type_policy selected with
      | D.Profile.Response_content_type_policy.Require_event_stream ->
        "require-event-stream"
      | Allow_absent_event_stream -> "allow-absent-event-stream"
    in
    printf
      "%s: %s, %s\n"
      (Sexp.to_string (P.sexp_of_route route))
      emission
      response_policy);
  [%expect
    {|
    Public_api: explicit-disabled, require-event-stream
    Direct_codex: omitted, allow-absent-event-stream
    |}]
;;

let%expect_test
    "compaction inherits shipping profile settings and preserves explicit controls"
  =
  Eio_main.run (fun env ->
    List.iter
      [ P.Direct_codex, None; Public_api, Some 2048; Direct_codex, Some 2048 ]
      ~f:(fun (route, max_tokens) ->
        let auth_calls = ref 0 in
        let profile =
          D.Profile.create
            ~id:"compaction-policy"
            ~account:None
            ~endpoint:(P.endpoint route)
            ~capabilities:
              (P.capabilities route ~endpoint:(P.endpoint route) |> Or_error.ok_exn)
            ~defaults:[]
          |> Or_error.ok_exn
          |> P.apply_endpoint_policy ~route
        in
        let driver =
          D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
          |> Or_error.ok_exn
        in
        let host =
          Inference_host.create
            driver
            ~profile
            ~profile_revision:None
            ~auth:(fun ~sw:_ _ ->
              incr auth_calls;
              Error D.Auth.Missing)
            ~default_model:"gpt-6-luna"
            ~namespace:"compaction-policy"
            ~limits:Inference_runtime.Limits.default
          |> Result.map_error ~f:(fun _ -> "host")
          |> Result.ok_or_failwith
        in
        let target =
          Inference_host.capture_config
            host
            { Chat_response.Config.default with max_tokens }
          |> Result.map_error ~f:(fun _ -> "capture")
          |> Result.ok_or_failwith
        in
        let context =
          Inference_host.resolve host target
          |> Result.map_error ~f:(fun _ -> "resolve")
          |> Result.ok_or_failwith
        in
        let inference =
          Inference_client.Execution.create
            ~context
            ~identity:(Inference_host.identity host)
            ~relation:Root
            ~before_dispatch:ignore
            ~on_attempt:ignore
            ~on_completion:ignore
            ~on_observation:ignore
        in
        let result =
          Context_compaction.Summarizer.summarise ~inference ~relevant_items:[] ~env:None
        in
        let outcome =
          match result with
          | Error (Context_compaction.Summarizer.Failed error) ->
            Sexp.to_string (Inference_client.Execution.Completion_error.sexp_of_t error)
          | Error exn -> Exn.to_string_mach exn
          | Ok _ -> "unexpected summary"
        in
        printf
          "%s control:%s auth:%d %s\n"
          (Sexp.to_string (P.sexp_of_route route))
          (Option.value_map max_tokens ~default:"absent" ~f:Int.to_string)
          !auth_calls
          outcome));
  [%expect
    {|
    Direct_codex control:absent auth:1 (Outcome(Failed(Authentication Missing)))
    Public_api control:2048 auth:1 (Outcome(Failed(Authentication Missing)))
    Direct_codex control:2048 auth:0 (Dispatch(Preparation Unsupported_input))
  |}]
;;
