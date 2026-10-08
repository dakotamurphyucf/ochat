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
      ; Direct_codex, P.endpoint Direct_codex, "gpt-6-luna"
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
