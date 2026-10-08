open! Core
module D = Openai.Responses_driver

let%expect_test
    "direct route/account refusal happens before HTTP or WS connect and fallback"
  =
  Eio_mock.Backend.run_full (fun env ->
    let net = Eio_mock.Net.make "must-remain-unused" in
    let driver = D.create ~net ~clock:(Eio.Stdenv.clock env) () |> Or_error.ok_exn in
    List.iter
      [ "HTTP", Inference.Observation.Transport_policy.Http_sse
      ; "WS", Require_websocket
      ; "prefer WS", Prefer_websocket
      ]
      ~f:(fun (name, policy) ->
        let resolved = ref 0 in
        let refused = ref 0 in
        let invalid_profiles = ref 0 in
        let events = ref 0 in
        List.iter
          [ "https://api.openai.com/v1/responses", Some "account-a", false
          ; ( "https://chatgpt.com/backend-api/codex/responses?alternate=1"
            , Some "account-a"
            , true )
          ; "http://chatgpt.com/backend-api/codex/responses", Some "account-a", true
          ; "https://chatgpt.com/backend-api/codex/responses", None, false
          ; ( "https://chatgpt.com/backend-api/codex/responses"
            , Some "different-account"
            , false )
          ]
          ~f:(fun (endpoint, account, invalid_profile) ->
            let caps = Driver_tests.capabilities ~extra:[ Websocket, Supported ] () in
            match
              D.Profile.create
                ~id:"synthetic"
                ~account
                ~endpoint
                ~capabilities:caps
                ~defaults:[]
            with
            | Error _ ->
              if not invalid_profile then failwith "unexpected profile rejection";
              incr invalid_profiles
            | Ok profile ->
              if invalid_profile then failwith "invalid endpoint admitted";
              let prepared = Driver_tests.prepare profile in
              let result =
                D.run_with_transport
                  driver
                  ~session:None
                  ~policy
                  ~auth:(fun ~sw:_ _ ->
                    incr resolved;
                    D.Auth.direct_codex "synthetic-secret" ~account:"account-a")
                  ~prepared
                  ~on_selected:(fun _ _ -> ())
                  ~on_event:(fun _ -> incr events)
              in
              (match result with
               | Error D.Auth.Invalid_credential -> incr refused
               | Error _ | Ok _ -> failwith "unexpected direct credential admission"));
        printf
          "%s invalid-profiles:%d refused:%d resolved:%d events:%d\n"
          name
          !invalid_profiles
          !refused
          !resolved
          !events));
  [%expect
    {|
    HTTP invalid-profiles:2 refused:3 resolved:3 events:0
    WS invalid-profiles:2 refused:3 resolved:3 events:0
    prefer WS invalid-profiles:2 refused:3 resolved:3 events:0
    |}]
;;

let%expect_test "fixed direct header builder keeps bearer-only requests unchanged" =
  let profile = Driver_tests.profile "https://chatgpt.com/backend-api/codex/responses" in
  List.iter
    [ D.Auth.bearer "synthetic-secret"
    ; D.Auth.direct_codex "synthetic-secret" ~account:"account-a"
    ]
    ~f:(fun lease ->
      let summary =
        D.For_testing.header_summary (Driver_tests.unwrap lease) profile
        |> Driver_tests.unwrap
      in
      print_s [%sexp (summary : D.For_testing.header_summary)]);
  [%expect
    {|
    ((account false) (originator false) (user_agent false))
    ((account true) (originator true) (user_agent true))
    |}]
;;
