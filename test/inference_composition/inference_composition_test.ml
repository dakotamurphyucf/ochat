open! Core

let%expect_test "CLI captures the configured provider base without remote fallback" =
  List.iter
    [ None
    ; Some "api.openai.com"
    ; Some "https://gateway.example/proxy"
    ; Some "https://gateway.example/proxy/"
    ; Some "http://127.0.0.1:8765"
    ; Some ""
    ]
    ~f:(fun api_url -> print_endline (Inference_composition.responses_endpoint ~api_url));
  [%expect
    {|
    https://api.openai.com/v1/responses
    https://api.openai.com/v1/responses
    https://gateway.example/proxy/v1/responses
    https://gateway.example/proxy/v1/responses
    http://127.0.0.1:8765/v1/responses
    https:///v1/responses
    |}]
;;
