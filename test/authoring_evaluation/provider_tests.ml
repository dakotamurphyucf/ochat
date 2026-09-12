open Core
open Authoring_evaluation
open Runner
module P = Responses_provider

let action operation payload =
  `Object [ "operation", `String operation; "payload", `String payload ]
;;

let response
      ?(status = "completed")
      ?(usage = `Object [ "input_tokens", `Number "13" ])
      action
  =
  `Object
    [ "status", `String status
    ; "usage", usage
    ; ( "output"
      , `Array
          [ `Object [ "type", `String "reasoning" ]
          ; `Object
              [ "type", `String "message"
              ; "role", `String "assistant"
              ; "status", `String "completed"
              ; ( "content"
                , `Array
                    [ `Object
                        [ "type", `String "output_text"
                        ; "text", `String (Jsonaf.to_string action)
                        ]
                    ] )
              ]
          ] )
    ]
;;

let config =
  { Driver_tests.config with
    seeds = [ None ]
  ; model = "explicit-test-model"
  ; model_parameters =
      `Object
        [ "max_output_tokens", `Number "4096"
        ; "temperature", `Number "0"
        ; "reasoning", `Object [ "effort", `String "high" ]
        ]
  ; max_transcript_bytes = 1000000
  }
;;

let%expect_test
    "Responses adapter preserves settings, measured protocol, retrieval and usage \
     through the driver"
  =
  Eio_main.run (fun env ->
    let calls = ref 0 in
    let post request =
      incr calls;
      assert (
        Jsonaf.exactly_equal (Jsonaf.member_exn "model" request) (`String config.model));
      assert (Jsonaf.exactly_equal (Jsonaf.member_exn "store" request) `False);
      assert (Jsonaf.exactly_equal (Jsonaf.member_exn "tools" request) (`Array []));
      let expected =
        match config.model_parameters with
        | `Object fields -> fields
        | _ -> assert false
      in
      List.iter expected ~f:(fun (key, value) ->
        assert (Jsonaf.exactly_equal (Jsonaf.member_exn key request) value));
      assert (Option.is_none (Jsonaf.member "seed" request));
      let format = Jsonaf.member_exn "text" request |> Jsonaf.member_exn "format" in
      assert (Jsonaf.exactly_equal (Jsonaf.member_exn "strict" format) `True);
      assert (Jsonaf.exactly_equal (Jsonaf.member_exn "schema" format) P.action_schema);
      let input =
        match Jsonaf.member_exn "input" request with
        | `Array input -> input
        | _ -> failwith "provider input was not an array"
      in
      assert (
        List.exists input ~f:(fun item ->
          Jsonaf.member_exn "content" item
          |> Jsonaf.string_exn
          |> String.is_prefix ~prefix:P.protocol));
      let payload =
        match !calls mod 2 with
        | 1 ->
          action
            "retrieve"
            (Jsonaf.to_string
               (Reference_backend.request ~task:"one_off_script" "prepare"))
        | _ -> action "submit" "{}"
      in
      P.read_response
        ~status:200
        (Eio.Flow.string_source (Jsonaf.to_string (response payload)))
    in
    let with_backend task f =
      Driver_tests.with_backend task (fun prepared ->
        f { prepared with backend = P.with_protocol prepared.backend })
    in
    let artifact =
      Driver.run ~env ~config ~with_backend ~make_provider:(P.make_provider ~post) ()
    in
    let report = Report.create artifact |> Result.ok_or_failwith in
    assert (!calls = 48);
    List.iter report.policies ~f:(fun row ->
      assert (Float.equal row.runtime_success_rate 1.);
      assert (Option.equal Int.equal row.provider_input_tokens (Some 208)));
    assert (String.equal report.real_model_evaluation "not_run");
    List.iter artifact.rows ~f:(fun row ->
      match row.result, row.exchanges with
      | ( Some result
        , [ { action = Some (Retrieve _); _ }; { action = Some (Submit _); _ } ] ) ->
        assert (result.retrieval_calls = 1);
        assert (result.tokens.tool_descriptions >= Runner.estimate P.protocol)
      | _ -> failwith "adapter lost actions or measured protocol");
    print_endline
      "24 paired offline cases; 48 strict requests; settings and protocol retained; \
       usage recorded; no live calls");
  [%expect
    {| 24 paired offline cases; 48 strict requests; settings and protocol retained; usage recorded; no live calls |}]
;;

let%expect_test
    "Responses parser rejects ambiguity, incomplete output and forged action fields \
     without exposing body text"
  =
  let bad =
    [ ( "additional score"
      , Jsonaf.to_string
          (response
             (`Object
                 [ "operation", `String "submit"
                 ; "payload", `String "{}"
                 ; "score", `Number "1"
                 ])) )
    ; ( "duplicate operation"
      , Jsonaf.to_string
          (response
             (`Object
                 [ "operation", `String "submit"
                 ; "operation", `String "decline"
                 ; "payload", `String "private-marker"
                 ])) )
    ; "unknown operation", Jsonaf.to_string (response (action "execute" "private-marker"))
    ; "payload syntax", Jsonaf.to_string (response (action "submit" "private-marker"))
    ; "nonobject payload", Jsonaf.to_string (response (action "retrieve" "[]"))
    ; ( "incomplete"
      , Jsonaf.to_string (response ~status:"incomplete" (action "submit" "{}")) )
    ; ( "negative usage"
      , Jsonaf.to_string
          (response
             ~usage:(`Object [ "input_tokens", `Number "-1" ])
             (action "submit" "{}")) )
    ; ( "executable output"
      , {|{"status":"completed","output":[{"type":"function_call","name":"private-marker"}]}|}
      )
    ; ( "refusal"
      , {|{"status":"completed","output":[{"type":"message","role":"assistant","status":"completed","content":[{"type":"refusal","refusal":"private-marker"}]}]}|}
      )
    ; ( "duplicate actions"
      , {|{"status":"completed","output":[{"type":"message","role":"assistant","status":"completed","content":[{"type":"output_text","text":"{}"},{"type":"output_text","text":"private-marker"}]}]}|}
      )
    ; "body syntax", "private-marker"
    ]
  in
  List.iter bad ~f:(fun (name, body) ->
    match P.read_response ~status:200 (Eio.Flow.string_source body) with
    | exception Infrastructure_failure message ->
      assert (not (String.is_substring message ~substring:"private-marker"));
      print_endline (name ^ ": rejected")
    | _ -> failwith ("accepted malformed response: " ^ name));
  let missing_usage =
    P.decode_response
      (Jsonaf.to_string (response ~usage:`Null (action "decline" "cannot finish")))
  in
  assert (Option.is_none missing_usage.provider_input_tokens);
  (match missing_usage.action with
   | Decline reason -> assert (String.equal reason "cannot finish")
   | _ -> failwith "decline was not preserved");
  [%expect
    {|
    additional score: rejected
    duplicate operation: rejected
    unknown operation: rejected
    payload syntax: rejected
    nonobject payload: rejected
    incomplete: rejected
    negative usage: rejected
    executable output: rejected
    refusal: rejected
    duplicate actions: rejected
    body syntax: rejected
    |}]
;;

let%expect_test
    "Responses configuration and transport errors fail before unsafe fallbacks"
  =
  let forbidden =
    [ { config with seeds = [ Some 7 ] }
    ; { config with model_parameters = `Object [] }
    ; { config with
        model_parameters =
          `Object [ "max_output_tokens", `Number "1"; "tools", `Array [] ]
      }
    ; { config with
        model_parameters =
          `Object [ "max_output_tokens", `Number "1"; "temperature", `Number "3" ]
      }
    ]
  in
  List.iter forbidden ~f:(fun config ->
    match P.validate_config config with
    | exception Invalid_argument _ -> ()
    | _ -> failwith "invalid settings accepted");
  let limited body status =
    match P.read_response ~status (Eio.Flow.string_source body) with
    | exception Infrastructure_failure message -> message
    | _ -> failwith "invalid body accepted"
  in
  assert (String.equal (limited "private-marker" 429) "provider HTTP status 429");
  ignore (limited (String.make (P.max_response_bytes + 2) 'x') 200 : string);
  let call post =
    P.make_provider
      ~post
      ~config
      ~task:(List.hd_exn Tasks.all)
      ~policy:Minimal
      ~repetition:0
      ~seed:None
      ~step:1
      ~messages:[]
  in
  (match call (fun _ -> failwith "secret-key-private-marker") with
   | exception Infrastructure_failure message ->
     assert (String.equal message "provider transport failed")
   | _ -> failwith "transport failure not classified");
  (match call (fun _ -> raise (Eio.Cancel.Cancelled Exit)) with
   | exception Eio.Cancel.Cancelled _ -> ()
   | _ -> failwith "cancellation swallowed");
  print_endline
    "unsupported settings rejected; HTTP/body errors bounded; transport errors \
     sanitized; cancellation propagated";
  [%expect
    {| unsupported settings rejected; HTTP/body errors bounded; transport errors sanitized; cancellation propagated |}]
;;
