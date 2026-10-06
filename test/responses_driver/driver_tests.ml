open! Core
module D = Openai.Responses_driver
module R = Openai.Responses.Codec.Request
module W = Openai.Responses.Codec.Wire

let json = Jsonaf.of_string
let ok = Or_error.ok_exn

let unwrap = function
  | Ok value -> value
  | Error _ -> failwith "expected Ok"
;;

let capabilities ?(extra = []) () =
  D.Capability.create
    ~baseline:([ D.Capability.Text_input, D.Capability.Supported ] @ extra)
    ~models:[]
  |> ok
;;

let profile
      ?(id = "profile-a")
      ?(account = Some "account-a")
      ?(caps = capabilities ())
      ?(defaults = [])
      endpoint
  =
  D.Profile.create ~id ~account ~endpoint ~capabilities:caps ~defaults |> ok
;;

let prepare
      ?(model = "arbitrary-future-model")
      ?(history = [])
      ?(tools = [])
      ?(settings = [])
      p
  =
  D.Prepared.create p ~model ~history ~tools ~settings |> ok
;;

let setting name value provenance = D.Setting.create ~name ~value ~provenance |> ok
let auth ~sw:_ _ = D.Auth.bearer "test-key-a"

let show_terminal = function
  | D.Terminal.Provider (W.Tracker.Response { terminal; _ }) ->
    print_s [%sexp (terminal : W.Event.terminal)]
  | Provider (Error _) -> print_endline "Provider_error"
  | Failed { delivery; reason } ->
    print_s [%sexp (delivery : D.Terminal.delivery), (reason : D.Terminal.failure)]
;;

let response status output extras =
  sprintf
    {|{"id":"resp_fixture","object":"response","status":"%s","output":%s%s}|}
    status
    output
    extras
;;

let frame body = "data: " ^ body ^ "\n\n"

let terminal ?(status = "completed") ?(output = "[]") ?(extras = "") () =
  frame
    (sprintf
       {|{"type":"response.%s","sequence_number":2,"response":%s}|}
       status
       (response status output extras))
;;

let created =
  frame
    (sprintf
       {|{"type":"response.created","sequence_number":0,"response":%s}|}
       (response "in_progress" "[]" ""))
;;

let future =
  frame {|{"type":"response.future","sequence_number":1,"payload":"PRIVATE_EVENT_BYTES"}|}
;;

let normal = created ^ future ^ terminal ()

type request =
  { target : string
  ; headers : (string * string) list
  ; body : string
  }

let read_request flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:2_000_000 in
  let target = Eio.Buf_read.line reader in
  let rec headers acc =
    let line = Eio.Buf_read.line reader in
    if String.is_empty line
    then List.rev acc
    else (
      let key, value = String.lsplit2 line ~on:':' |> Option.value_exn in
      headers ((String.lowercase key, String.strip value) :: acc))
  in
  let headers = headers [] in
  let bytes =
    List.Assoc.find_exn headers ~equal:String.equal "content-length" |> Int.of_string
  in
  let body = Eio.Buf_read.take bytes reader in
  { target; headers; body }
;;

let write flow ?(status = 200) ?(headers = []) body =
  let headers =
    [ "Content-Type", "text/event-stream"
    ; "Content-Length", Int.to_string (String.length body)
    ]
    @ headers
  in
  Eio.Flow.copy_string
    (sprintf
       "HTTP/1.1 %d Test\r\n%sConnection: close\r\n\r\n%s"
       status
       (String.concat (List.map headers ~f:(fun (k, v) -> k ^ ": " ^ v ^ "\r\n")))
       body)
    flow
;;

let with_server env handler f =
  Eio.Switch.run (fun sw ->
    let listener =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:16
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr listener with
      | `Tcp (_, port) -> port
      | _ -> assert false
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        let flow, _ = Eio.Net.accept ~sw listener in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          handler flow (read_request flow);
          Eio.Flow.close flow;
          `Stop_daemon)
      done);
    f sw (sprintf "http://127.0.0.1:%d/v1/responses" port))
;;

let driver
      ?max_request_bytes
      ?max_header_bytes
      ?max_body_bytes
      ?max_frame_bytes
      ?timeout_seconds
      env
  =
  D.create
    ~net:(Eio.Stdenv.net env)
    ~clock:(Eio.Stdenv.clock env)
    ?max_request_bytes
    ?max_header_bytes
    ?max_body_bytes
    ?max_frame_bytes
    ?timeout_seconds
    ()
  |> ok
;;

let run d ~sw:_ = D.run d

let run_print d ~sw prepared =
  let terminals = ref 0 in
  let emitted = ref None in
  let result =
    run d ~sw ~auth ~prepared ~on_event:(function
      | D.Event.Terminal outcome ->
        incr terminals;
        emitted := Some outcome;
        show_terminal outcome
      | Update _ | Finalized _ -> ())
    |> unwrap
  in
  printf
    "terminals:%d matching:%b\n"
    !terminals
    (Option.exists !emitted ~f:(fun terminal -> phys_equal terminal result))
;;

let%expect_test
    "arbitrary models require declared text baseline; optional settings fail closed"
  =
  let p = profile "https://api.example.test/v1/responses" in
  let prepared = prepare p in
  printf "model:%s\n" (D.Prepared.model prepared);
  List.iter [ "temperature"; "reasoning"; "unknown-field" ] ~f:(fun name ->
    match
      D.Setting.create
        ~name
        ~value:(R.Field.Value (`Number "0.5"))
        ~provenance:Execution_override
    with
    | Error _ -> printf "%s:unknown-name\n" name
    | Ok setting ->
      printf
        "%s:prepare-error=%b\n"
        name
        (Result.is_error
           (D.Prepared.create
              p
              ~model:"anything"
              ~history:[]
              ~tools:[]
              ~settings:[ setting ])));
  let caps =
    D.Capability.create
      ~baseline:[ Text_input, Supported; Setting "temperature", Unsupported ]
      ~models:[ "named", [ Setting "temperature", Supported ] ]
    |> ok
  in
  print_s
    [%sexp
      (D.Capability.resolve caps ~model:"named" ~feature:(Setting "temperature")
       : D.Capability.support)];
  printf
    "no-baseline:%b\n"
    (Result.is_error
       (D.Prepared.create
          (profile
             ~caps:(D.Capability.create ~baseline:[] ~models:[] |> ok)
             "https://api.example.test/v1/responses")
          ~model:"x"
          ~history:[]
          ~tools:[]
          ~settings:[]));
  [%expect
    {|
model:arbitrary-future-model
temperature:prepare-error=true
reasoning:prepare-error=true
unknown-field:unknown-name
Unsupported
no-baseline:true
|}]
;;

let%expect_test "profile validation, presence precedence and final guidance fingerprints" =
  List.iter
    [ "http://remote.test/v1/responses"
    ; "https://user:secret@remote.test/v1/responses"
    ; "https://remote.test/v1/responses?secret=x"
    ; "https://remote.test/v1/responses#x"
    ]
    ~f:(fun endpoint ->
      printf
        "endpoint-error:%b\n"
        (Result.is_error
           (D.Profile.create
              ~id:"p"
              ~account:None
              ~endpoint
              ~capabilities:(capabilities ())
              ~defaults:[])));
  let caps =
    capabilities
      ~extra:[ Setting "temperature", Supported; Setting "instructions", Supported ]
      ()
  in
  let defaults = [ setting "temperature" (Value (`Number "0.2")) Profile_default ] in
  let p = profile ~caps ~defaults "https://api.example.test/v1/responses" in
  let settings =
    [ setting "temperature" (Value (`Number "0.7")) Captured_prompt
    ; setting "temperature" (Value (`Number "0.9")) Execution_override
    ]
  in
  let a = prepare ~settings p in
  print_endline (Jsonaf.to_string (R.jsonaf_of_t (D.Prepared.request a)));
  let b =
    prepare
      ~settings:
        (settings
         @ [ setting "instructions" (Value (`String "FINAL GUIDANCE")) Execution_override
           ])
      p
  in
  printf
    "guidance-changes-fingerprint:%b\n"
    (not (String.equal (D.Prepared.fingerprint a) (D.Prepared.fingerprint b)));
  let absent = prepare ~settings:[ setting "temperature" Absent Execution_override ] p in
  printf
    "absent-inherits:%b\n"
    (match R.field (D.Prepared.request absent) "temperature" with
     | Value (`Number "0.2") -> true
     | _ -> false);
  printf
    "duplicate-layer-error:%b\n"
    (Result.is_error
       (D.Prepared.create
          p
          ~model:"x"
          ~history:[]
          ~tools:[]
          ~settings:(settings @ settings)));
  printf
    "invalid-value-error:%b\n"
    (Result.is_error
       (D.Prepared.create
          p
          ~model:"x"
          ~history:[]
          ~tools:[]
          ~settings:[ setting "temperature" (Value (`Number "99")) Execution_override ]));
  [%expect
    {|
endpoint-error:true
endpoint-error:true
endpoint-error:true
endpoint-error:true
{"model":"arbitrary-future-model","input":[],"store":false,"truncation":"disabled","stream":true,"tools":[],"temperature":0.9}
guidance-changes-fingerprint:true
absent-inherits:true
duplicate-layer-error:true
invalid-value-error:true
|}]
;;

let%expect_test
    "real HTTP transmits exact full history and selected tools with incremental delivery"
  =
  Eio_main.run (fun env ->
    let first_seen, signal_first = Eio.Promise.create () in
    let observed_request = ref None in
    with_server
      env
      (fun flow req ->
         observed_request := Some req;
         Eio.Flow.copy_string
           "HTTP/1.1 200 OK\r\n\
            Content-Type: text/event-stream\r\n\
            Connection: close\r\n\
            \r\n"
           flow;
         Eio.Flow.copy_string created flow;
         Eio.Promise.await first_seen;
         Eio.Flow.copy_string (future ^ terminal ()) flow)
      (fun sw endpoint ->
         let caps =
           capabilities
             ~extra:
               [ Function_tools, Supported
               ; Opaque_replay, Supported
               ; Setting "instructions", Supported
               ]
             ()
         in
         let history =
           List.map
             [ {|{"role":"system","content":"PRIVATE SYSTEM"}|}
             ; {|{"role":"user","content":"earlier"}|}
             ; {|{"type":"function_call","name":"lookup","call_id":"call_history","arguments":" { \"x\":1e0 }\n","future":null}|}
             ; {|{"type":"function_call_output","call_id":"call_history","output":"result"}|}
             ; {|{"type":"reasoning","id":"reasoning_history","summary":[],"encrypted_content":"PRIVATE OPAQUE","extension":17}|}
             ; {|{"role":"developer","content":"FINAL GUIDANCE"}|}
             ]
             ~f:json
         in
         let tools =
           [ R.Tool.function_
               ~name:"lookup"
               ~parameters:
                 (Value (json {|{"type":"object","properties":{"x":{"type":"integer"}}}|}))
               ~strict:(Value true)
               ()
             |> ok
           ]
         in
         let prepared = prepare ~history ~tools (profile ~caps endpoint) in
         let events = ref [] in
         let terminal_value = ref None in
         let outcome =
           run (driver env) ~sw ~auth ~prepared ~on_event:(fun event ->
             match event with
             | Update update ->
               events := W.Event.raw update.event :: !events;
               if List.length !events = 1
               then (
                 print_endline "first-event-before-server-finishes";
                 Eio.Promise.resolve signal_first ())
             | Finalized _ -> ()
             | Terminal result ->
               terminal_value := Some result;
               show_terminal result)
           |> unwrap
         in
         let req = Option.value_exn !observed_request in
         let body = json req.body in
         printf
           "post-path:%b exact-body:%b history:%b tool:%b bearer:%b\n"
           (String.equal req.target "POST /v1/responses HTTP/1.1")
           (Jsonaf.exactly_equal body (R.jsonaf_of_t (D.Prepared.request prepared)))
           (match body with
            | `Object fields ->
              Jsonaf.exactly_equal
                (List.Assoc.find_exn fields ~equal:String.equal "input")
                (`Array history)
            | _ -> false)
           (match body with
            | `Object fields ->
              Jsonaf.exactly_equal
                (List.Assoc.find_exn fields ~equal:String.equal "tools")
                (`Array (List.map tools ~f:R.Tool.jsonaf_of_t))
            | _ -> false)
           (String.equal
              (List.Assoc.find_exn req.headers ~equal:String.equal "authorization")
              "Bearer test-key-a");
         printf
           "updates:%d one-matching-terminal:%b\n"
           (List.length !events)
           (Option.exists !terminal_value ~f:(fun terminal -> phys_equal terminal outcome))));
  [%expect
    {|
first-event-before-server-finishes
Completed
post-path:true exact-body:true history:true tool:true bearer:true
updates:2 one-matching-terminal:true
|}]
;;

let%expect_test "auth failure emits no events and cannot reach HTTP" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let prepared = prepare (profile "http://127.0.0.1:1/v1/responses") in
      let events = ref 0 in
      let result =
        run
          (driver env)
          ~sw
          ~prepared
          ~auth:(fun ~sw:_ p ->
            printf
              "identity:%s/%s\n"
              (D.Profile.id p)
              (Option.value_exn (D.Profile.account p));
            Error Missing)
          ~on_event:(fun _ -> incr events)
      in
      printf "auth-error:%b events:%d\n" (Result.is_error result) !events));
  [%expect
    {|
identity:profile-a/account-a
auth-error:true events:0
|}]
;;

let%expect_test
    "normal terminal dispositions retain incomplete, refused and provider-failed evidence"
  =
  Eio_main.run (fun env ->
    List.iter
      [ "incomplete", "[]", {|,"incomplete_details":{"reason":"max_output_tokens"}|}
      ; ( "failed"
        , "[]"
        , {|,"error":{"code":"server_error","message":"PRIVATE PROVIDER ERROR"}|} )
      ; ( "completed"
        , {|[{"id":"m","type":"message","role":"assistant","status":"completed","content":[{"type":"refusal","refusal":"cannot"}]}]|}
        , "" )
      ]
      ~f:(fun (status, output, extras) ->
        with_server
          env
          (fun flow _ -> write flow (terminal ~status ~output ~extras ()))
          (fun sw endpoint ->
             let result =
               run
                 (driver env)
                 ~sw
                 ~auth
                 ~prepared:(prepare (profile endpoint))
                 ~on_event:(function
                   | Terminal outcome -> show_terminal outcome
                   | Update _ | Finalized _ -> failwith "unexpected nonterminal")
               |> unwrap
             in
             match result with
             | Provider (W.Tracker.Response { response; _ }) ->
               (match W.Response.outcome response with
                | Completed -> print_endline "completed"
                | Refused -> print_endline "refused"
                | Incomplete _ -> print_endline "incomplete"
                | Failed _ -> print_endline "failed"
                | Nonterminal _ -> failwith "unexpected")
             | _ -> failwith "wrong outcome")));
  [%expect
    {|
Incomplete
incomplete
Failed
failed
Completed
refused
|}]
;;

let%expect_test "HTTP statuses and private malformed bodies remain redacted; no retries" =
  Eio_main.run (fun env ->
    List.iter [ 401; 429; 500; 302 ] ~f:(fun status ->
      let calls = ref 0 in
      with_server
        env
        (fun flow _ ->
           incr calls;
           write flow ~status "PRIVATE HELPER BODY INCLUDING KEY")
        (fun sw endpoint ->
           run_print (driver env) ~sw (prepare (profile endpoint));
           printf "calls:%d\n" !calls)));
  [%expect
    {|
(Possibly_submitted (Http_status 401))
terminals:1 matching:true
calls:1
(Possibly_submitted (Http_status 429))
terminals:1 matching:true
calls:1
(Possibly_submitted (Http_status 500))
terminals:1 matching:true
calls:1
(Possibly_submitted (Http_status 302))
terminals:1 matching:true
calls:1
|}]
;;

let%expect_test
    "partial publication survives malformed SSE or uncertain EOF without retries"
  =
  Eio_main.run (fun env ->
    List.iter
      [ created ^ "data: PRIVATE MALFORMED BODY\n\n"
      ; created ^ future
      ; "data: []\n\n"
      ; "data: [DONE]\n\n"
      ]
      ~f:(fun body ->
        let calls = ref 0 in
        with_server
          env
          (fun flow _ ->
             incr calls;
             write flow body)
          (fun sw endpoint ->
             let updates = ref 0 in
             let result =
               run
                 (driver env)
                 ~sw
                 ~auth
                 ~prepared:(prepare (profile endpoint))
                 ~on_event:(function
                   | Update _ -> incr updates
                   | Finalized _ -> ()
                   | Terminal terminal -> show_terminal terminal)
               |> unwrap
             in
             (match result with
              | Failed _ -> ()
              | Provider _ -> failwith "unexpected success");
             printf "updates:%d calls:%d\n" !updates !calls)));
  [%expect
    {|
(Response_started Protocol)
updates:1 calls:1
(Response_started Protocol)
updates:2 calls:1
(Possibly_submitted Protocol)
updates:0 calls:1
(Possibly_submitted Protocol)
updates:0 calls:1
|}]
;;

let%expect_test
    "consumer failures including Timeout propagate and do not manufacture terminals"
  =
  Eio_main.run (fun env ->
    List.iter [ Failure "consumer fixture"; Eio.Time.Timeout ] ~f:(fun raised ->
      with_server
        env
        (fun flow _ -> write flow normal)
        (fun sw endpoint ->
           let terminals = ref 0 in
           let propagated =
             try
               ignore
                 (run
                    (driver env)
                    ~sw
                    ~auth
                    ~prepared:(prepare (profile endpoint))
                    ~on_event:(function
                      | Terminal _ -> incr terminals
                      | Update _ | Finalized _ -> raise raised)
                  : (D.Terminal.t, D.Auth.error) Result.t);
               false
             with
             | ex -> phys_equal ex raised
           in
           printf "original-exception:%b terminals:%d\n" propagated !terminals)));
  [%expect
    {|
original-exception:true terminals:0
original-exception:true terminals:0
|}]
;;

let%expect_test "concurrent prepared profiles isolate credentials, model and settings" =
  Eio_main.run (fun env ->
    let requests = ref [] in
    with_server
      env
      (fun flow req ->
         requests := req :: !requests;
         write flow (terminal ()))
      (fun sw endpoint ->
         let caps = capabilities ~extra:[ Setting "temperature", Supported ] () in
         let auth ~sw:_ p = D.Auth.bearer ("test-key-" ^ D.Profile.id p) in
         let run id temperature =
           let p = profile ~caps ~id ~account:(Some ("account-" ^ id)) endpoint in
           let prepared =
             prepare
               ~model:("model-" ^ id)
               ~settings:
                 [ setting "temperature" (Value (`Number temperature)) Execution_override
                 ]
               p
           in
           run (driver env) ~sw ~auth ~prepared ~on_event:(fun _ -> ()) |> unwrap
         in
         Eio.Fiber.both
           (fun () -> ignore (run "a" "0.1" : D.Terminal.t))
           (fun () -> ignore (run "b" "0.8" : D.Terminal.t));
         let rows =
           List.map !requests ~f:(fun req ->
             let body = json req.body in
             let field key =
               match body with
               | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal key
               | _ -> assert false
             in
             ( Jsonaf.to_string (field "model")
             , Jsonaf.to_string (field "temperature")
             , List.Assoc.find_exn req.headers ~equal:String.equal "authorization" ))
         in
         List.sort rows ~compare:(fun (a, _, _) (b, _, _) -> String.compare a b)
         |> List.iter ~f:(fun (m, t, a) -> printf "%s %s %s\n" m t a)));
  [%expect
    {|
"model-a" 0.1 Bearer test-key-a
"model-b" 0.8 Bearer test-key-b
|}]
;;

let%expect_test "null caller metadata and immutable inline assets preserve full replay" =
  let caps =
    capabilities
      ~extra:
        [ D.Capability.Function_tools, Supported
        ; Image_input, Supported
        ; Document_input, Supported
        ]
      ()
  in
  let p = profile ~caps "https://api.example.test/v1/responses" in
  let history =
    List.map
      [ {|{"type":"function_call","call_id":"c","name":"lookup","arguments":"{}","caller":null,"future":17}|}
      ; {|{"type":"function_call_output","call_id":"c","output":"ok","namespace":null,"caller":null}|}
      ; {|{"role":"user","content":[{"type":"input_image","image_url":"data:image/png;base64,aGk=","detail":"auto"},{"type":"input_file","filename":"doc.pdf","file_data":"aGk="}]}|}
      ]
      ~f:json
  in
  let prepared = prepare ~history p in
  printf
    "exact-replay:%b\n"
    (List.equal Jsonaf.exactly_equal history (R.input (D.Prepared.request prepared)));
  List.iter
    [ {|{"role":"user","content":[{"type":"input_image","image_url":"https://remote.test/image.png","detail":"auto"}]}|}
    ; {|{"role":"user","content":[{"type":"input_file","file_url":"https://remote.test/doc.pdf"}]}|}
    ; {|{"role":"user","content":[{"type":"input_image","image_url":"data:image/png;base64,???","detail":"auto"}]}|}
    ; {|{"type":"function_call","call_id":"c","name":"lookup","arguments":"{}","caller":{"type":"program","caller_id":"remote"}}|}
    ]
    ~f:(fun h ->
      printf
        "reject:%b\n"
        (Result.is_error
           (D.Prepared.create
              p
              ~model:"anything"
              ~history:[ json h ]
              ~tools:[]
              ~settings:[])));
  [%expect
    {|
exact-replay:true
reject:true
reject:true
reject:true
reject:true
|}]
;;

let%expect_test "header, entity and frame bounds hold independently" =
  Eio_main.run (fun env ->
    with_server
      env
      (fun flow _ -> write flow normal)
      (fun sw endpoint ->
         run_print (driver ~max_header_bytes:128 env) ~sw (prepare (profile endpoint)));
    with_server
      env
      (fun flow _ -> write flow normal)
      (fun sw endpoint ->
         run_print (driver ~max_body_bytes:100 env) ~sw (prepare (profile endpoint)));
    with_server
      env
      (fun flow _ -> write flow (":" ^ String.make 200 'x' ^ "\n\n" ^ terminal ()))
      (fun sw endpoint ->
         run_print (driver ~max_frame_bytes:64 env) ~sw (prepare (profile endpoint)));
    with_server
      env
      (fun flow _ ->
         write
           flow
           ~headers:(List.init 12 ~f:(fun _ -> "X-Extra", String.make 20 'x'))
           normal)
      (fun sw endpoint ->
         run_print (driver ~max_header_bytes:128 env) ~sw (prepare (profile endpoint)));
    Eio.Switch.run (fun sw ->
      run_print
        (driver ~max_request_bytes:16 env)
        ~sw
        (prepare (profile "http://127.0.0.1:1/v1/responses"))));
  [%expect
    {|
Completed
terminals:1 matching:true
(Possibly_submitted Body_limit)
terminals:1 matching:true
(Possibly_submitted Body_limit)
terminals:1 matching:true
(Possibly_submitted Invalid_http)
terminals:1 matching:true
(Definitely_not_submitted Body_limit)
terminals:1 matching:true
|}]
;;

let%expect_test
    "strict HTTP framing rejects ambiguous metadata, encodings and premature EOF"
  =
  Eio_main.run (fun env ->
    List.iter
      [ "HTTP/1.1 200 OK\r\n\
         Content-Type: text/event-stream\r\n\
         Content-Length: 20\r\n\
         Content-Length: 20\r\n\
         \r\n\
         PRIVATE BODY"
      ; "HTTP/1.1 200 OK\r\n\
         Content-Type: text/event-stream\r\n\
         Content-Length: 20\r\n\
         Transfer-Encoding: chunked\r\n\
         \r\n\
         PRIVATE BODY"
      ; "HTTP/1.1 200 OK\r\n\
         Content-Type: text/event-stream\r\n\
         Content-Encoding: gzip\r\n\
         \r\n\
         PRIVATE BODY"
      ; "HTTP/1.1 100 Continue\r\n\r\n"
      ; "HTTP/1.1 204 No Content\r\n\r\n"
      ; "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\nPRIVATE BODY"
      ; "HTTP/1.1 200 OK\r\n\
         Content-Type: text/event-stream\r\n\
         Content-Length: 900\r\n\
         \r\n"
        ^ created
      ; "HTTP/1.1 200 OK\r\n\
         Content-Type: text/event-stream\r\n\
         Transfer-Encoding: chunked\r\n\
         \r\n\
         not-hex\r\n\
         PRIVATE BODY"
      ]
      ~f:(fun raw ->
        with_server
          env
          (fun flow _ -> Eio.Flow.copy_string raw flow)
          (fun sw endpoint -> run_print (driver env) ~sw (prepare (profile endpoint)))));
  [%expect
    {|
(Possibly_submitted Invalid_http)
terminals:1 matching:true
(Possibly_submitted Invalid_http)
terminals:1 matching:true
(Possibly_submitted Invalid_http)
terminals:1 matching:true
(Possibly_submitted Invalid_http)
terminals:1 matching:true
(Possibly_submitted (Http_status 204))
terminals:1 matching:true
(Possibly_submitted Invalid_content_type)
terminals:1 matching:true
(Response_started Connection)
terminals:1 matching:true
(Possibly_submitted Invalid_http)
terminals:1 matching:true
|}]
;;

let chunk data = sprintf "%x\r\n%s\r\n" (String.length data) data

let%expect_test "chunked streaming is incremental and aggregate body/trailer limits apply"
  =
  Eio_main.run (fun env ->
    let header =
      "HTTP/1.1 200 OK\r\n\
       Content-Type: text/event-stream\r\n\
       Transfer-Encoding: chunked\r\n\
       \r\n"
    in
    List.iter
      [ chunk created ^ chunk (terminal ()) ^ "0\r\n\r\n", 2_000_000, 128
      ; chunk created ^ chunk (":" ^ String.make 200 'x' ^ "\n\n") ^ "0\r\n\r\n", 180, 128
      ; ( chunk created
          ^ "0\r\n"
          ^ String.concat (List.init 12 ~f:(fun _ -> "X-Trailer: PRIVATE TRAILER\r\n"))
          ^ "\r\n"
        , 2_000_000
        , 128 )
      ]
      ~f:(fun (body, max_body_bytes, max_header_bytes) ->
        with_server
          env
          (fun flow _ -> Eio.Flow.copy_string (header ^ body) flow)
          (fun sw endpoint ->
             run_print
               (driver ~max_body_bytes ~max_header_bytes env)
               ~sw
               (prepare (profile endpoint)))));
  [%expect
    {|
Completed
terminals:1 matching:true
(Response_started Body_limit)
terminals:1 matching:true
(Response_started Invalid_http)
terminals:1 matching:true
|}]
;;

let%expect_test
    "terminal-only completed calls retain finalization evidence without duplicate \
     terminals"
  =
  Eio_main.run (fun env ->
    let output =
      {|[{"type":"function_call","id":"f","call_id":"c","name":"lookup","arguments":"{}","status":"completed"}]|}
    in
    with_server
      env
      (fun flow _ -> write flow (terminal ~output ()))
      (fun sw endpoint ->
         let finalized = ref 0 in
         let terminals = ref 0 in
         ignore
           (run
              (driver env)
              ~sw
              ~auth
              ~prepared:(prepare (profile endpoint))
              ~on_event:(function
                | Finalized items -> finalized := !finalized + List.length items
                | Terminal _ -> incr terminals
                | Update _ -> failwith "unexpected update")
            |> unwrap
            : D.Terminal.t);
         printf "finalized:%d terminals:%d\n" !finalized !terminals));
  [%expect {| finalized:1 terminals:1 |}]
;;

let%expect_test "external cancellation propagates during auth and after first publication"
  =
  Eio_main.run (fun env ->
    List.iter [ false; true ] ~f:(fun streaming ->
      let started, signal_started = Eio.Promise.create () in
      let attempt ~sw endpoint =
        let context, signal_context = Eio.Promise.create () in
        let events = ref 0 in
        let terminals = ref 0 in
        Eio.Fiber.both
          (fun () ->
             let cancelled =
               try
                 Eio.Cancel.sub (fun cancel ->
                   Eio.Promise.resolve signal_context cancel;
                   ignore
                     (run
                        (driver env)
                        ~sw
                        ~prepared:(prepare (profile endpoint))
                        ~auth:(fun ~sw:_ _ ->
                          if streaming
                          then auth ~sw ()
                          else (
                            Eio.Promise.resolve signal_started ();
                            Eio.Fiber.await_cancel ()))
                        ~on_event:(function
                          | Terminal _ -> incr terminals
                          | Update _ | Finalized _ ->
                            incr events;
                            Eio.Promise.resolve signal_started ();
                            Eio.Fiber.await_cancel ())
                      : (D.Terminal.t, D.Auth.error) Result.t));
                 false
               with
               | Eio.Cancel.Cancelled _ -> true
             in
             printf "cancelled:%b events:%d terminals:%d\n" cancelled !events !terminals)
          (fun () ->
             Eio.Promise.await started;
             Eio.Cancel.cancel (Eio.Promise.await context) Exit)
      in
      if streaming
      then
        with_server
          env
          (fun flow _ -> write flow normal)
          (fun sw endpoint -> attempt ~sw endpoint)
      else Eio.Switch.run (fun sw -> attempt ~sw "http://127.0.0.1:1/v1/responses")));
  [%expect
    {|
cancelled:true events:0 terminals:0
cancelled:true events:1 terminals:0
|}]
;;

let%expect_test "fake-clock authentication deadline fails before any event" =
  Eio_mock.Backend.run_full (fun mock_env ->
    Eio.Switch.run (fun sw ->
      let events = ref 0 in
      let d =
        D.create
          ~net:(Eio_mock.Net.make "unused-auth-net")
          ~clock:(Eio.Stdenv.clock mock_env)
          ~timeout_seconds:1.
          ()
        |> ok
      in
      let result =
        run
          d
          ~sw
          ~prepared:(prepare (profile "http://127.0.0.1:1/v1/responses"))
          ~auth:(fun ~sw:_ _ -> Eio.Fiber.await_cancel ())
          ~on_event:(fun _ -> incr events)
      in
      printf
        "auth-timeout:%b events:%d\n"
        (match result with
         | Error D.Auth.Timed_out -> true
         | _ -> false)
        !events));
  [%expect
    {|
+mock time is now 1
auth-timeout:true events:0
|}]
;;

let%expect_test
    "HTTPS rejects an untrusted server certificate before sending HTTP credentials"
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let cert = X509.Certificate.decode_pem Tls_fixture.certificate |> unwrap in
      let key = X509.Private_key.decode_pem Tls_fixture.private_key |> unwrap in
      let config =
        Tls.Config.server ~certificates:(`Single ([ cert ], key)) () |> unwrap
      in
      let address =
        List.hd_exn
          (Eio.Net.getaddrinfo_stream ~service:"0" (Eio.Stdenv.net env) "localhost")
      in
      let listener =
        Eio.Net.listen ~sw ~reuse_addr:true ~backlog:4 (Eio.Stdenv.net env) address
      in
      let port =
        match Eio.Net.listening_addr listener with
        | `Tcp (_, port) -> port
        | _ -> assert false
      in
      let http_received = ref false in
      let server_done, signal_done = Eio.Promise.create () in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        let flow, _ = Eio.Net.accept ~sw listener in
        (try
           let tls = Tls_eio.server_of_flow config flow in
           ignore (Eio.Flow.single_read tls (Cstruct.create 1) : int);
           http_received := true
         with
         | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ | End_of_file -> ());
        Eio.Promise.resolve signal_done ();
        `Stop_daemon);
      let prepared =
        prepare (profile (sprintf "https://localhost:%d/v1/responses" port))
      in
      run_print (driver env) ~sw prepared;
      Eio.Promise.await server_done;
      printf "credential-http-received:%b\n" !http_received));
  [%expect
    {|
(Definitely_not_submitted Connection)
terminals:1 matching:true
credential-http-received:false
|}]
;;

let%expect_test "permissive Jsonaf numeric parsing never admits invalid outgoing JSON" =
  printf
    "parser-accepts-leading-zero:%b\n"
    (match Or_error.try_with (fun () -> Jsonaf.of_string "01") with
     | Ok (`Number "01") -> true
     | _ -> false);
  let p = profile "https://api.example.test/v1/responses" in
  List.iter
    [ "01"; "-01"; "+1"; ".1"; "1."; "1e"; "1e+"; "NaN"; "Infinity"; "0"; "-0"; "1.2e-3" ]
    ~f:(fun number ->
      let history =
        [ `Object
            [ "role", `String "user"
            ; "content", `String "private"
            ; "future", `Number number
            ]
        ]
      in
      printf
        "%s:%b\n"
        number
        (Result.is_ok (D.Prepared.create p ~model:"x" ~history ~tools:[] ~settings:[])));
  [%expect
    {|
parser-accepts-leading-zero:true
01:false
-01:false
+1:false
.1:false
1.:false
1e:false
1e+:false
NaN:false
Infinity:false
0:true
-0:true
1.2e-3:true
|}]
;;

let%expect_test "fake-clock timeout closes attempt socket after publication without retry"
  =
  Eio_mock.Backend.run_full (fun env ->
    List.iter [ false; true ] ~f:(fun blocked_callback ->
      let net = Eio_mock.Net.make "inference-net" in
      let flow =
        Eio_mock.Flow.make
          ~pp:(fun f _ -> Fmt.string f "<private bytes>")
          "attempt-socket"
      in
      let connects = ref 0 in
      Eio_mock.Net.on_getaddrinfo
        net
        [ `Return [ `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) ] ];
      Eio_mock.Net.on_connect
        net
        [ `Run
            (fun () ->
              incr connects;
              flow)
        ];
      Eio_mock.Flow.on_read
        flow
        [ `Return
            ("HTTP/1.1 200 OK\r\n\
              Content-Type: text/event-stream\r\n\
              Connection: close\r\n\
              \r\n"
             ^ created)
        ; `Run (fun () -> Eio.Fiber.await_cancel ())
        ];
      let driver =
        D.create ~net ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:1. () |> ok
      in
      let events = ref 0 in
      let terminals = ref 0 in
      let emitted = ref None in
      let result =
        D.run
          driver
          ~auth
          ~prepared:(prepare (profile "http://127.0.0.1:80/v1/responses"))
          ~on_event:(function
            | Update _ ->
              incr events;
              if blocked_callback then Eio.Fiber.await_cancel ()
            | Finalized _ -> ()
            | Terminal terminal ->
              incr terminals;
              emitted := Some terminal;
              show_terminal terminal)
        |> unwrap
      in
      printf
        "events:%d terminals:%d connects:%d matching:%b\n"
        !events
        !terminals
        !connects
        (Option.exists !emitted ~f:(fun terminal -> phys_equal terminal result))));
  [%expect
    {|
    +inference-net: getaddrinfo ~service:80 127.0.0.1
    +inference-net: connect to tcp:127.0.0.1:80
    +attempt-socket: wrote <private bytes>
    +attempt-socket: wrote <private bytes>
    +attempt-socket: read <private bytes>
    +mock time is now 1
    +attempt-socket: closed
    (Response_started Timeout)
    +inference-net: getaddrinfo ~service:80 127.0.0.1
    +inference-net: connect to tcp:127.0.0.1:80
    +attempt-socket: wrote <private bytes>
    +attempt-socket: wrote <private bytes>
    +attempt-socket: read <private bytes>
    +mock time is now 2
    +attempt-socket: closed
    events:1 terminals:1 connects:1 matching:true
    (Response_started Timeout)
    events:1 terminals:1 connects:1 matching:true
    |}]
;;

let%expect_test "terminal consumer exception propagates after attempt cleanup" =
  Eio_main.run (fun env ->
    with_server
      env
      (fun flow _ -> write flow (terminal ()))
      (fun _sw endpoint ->
         let raised = Failure "terminal consumer failure" in
         let entered = ref 0 in
         let propagated =
           try
             ignore
               (D.run
                  (driver env)
                  ~auth
                  ~prepared:(prepare (profile endpoint))
                  ~on_event:(function
                    | Terminal _ ->
                      incr entered;
                      raise raised
                    | Update _ | Finalized _ -> ())
                : (D.Terminal.t, D.Auth.error) Result.t);
             false
           with
           | ex -> phys_equal ex raised
         in
         printf "original:%b terminal-callbacks:%d\n" propagated !entered));
  [%expect {| original:true terminal-callbacks:1 |}]
;;

let%expect_test
    "provider-owned references, async tools and name collisions reject at preparation"
  =
  let caps =
    capabilities
      ~extra:
        [ D.Capability.Function_tools, Supported
        ; Custom_tools, Supported
        ; Document_input, Supported
        ]
      ()
  in
  let p = profile ~caps "https://api.example.test/v1/responses" in
  let tool =
    R.Tool.function_
      ~name:"same"
      ~parameters:(Value (json {|{"type":"object"}|}))
      ~strict:(Value true)
      ()
    |> ok
  in
  let async = R.Tool.custom ~name:"later" ~async:(Value true) () |> ok in
  List.iter
    [ [ tool; tool ]; [ async ] ]
    ~f:(fun tools ->
      printf
        "tools-rejected:%b\n"
        (Result.is_error (D.Prepared.create p ~model:"x" ~history:[] ~tools ~settings:[])));
  printf
    "hosted-tool-rejected:%b\n"
    (Result.is_error (R.Tool.of_jsonaf (json {|{"type":"web_search"}|})));
  printf
    "provider-file-id-rejected:%b\n"
    (Result.is_error
       (D.Prepared.create
          p
          ~model:"x"
          ~history:
            [ json
                {|{"role":"user","content":[{"type":"input_file","file_id":"remote-file"}]}|}
            ]
          ~tools:[]
          ~settings:[]));
  [%expect
    {|
tools-rejected:true
tools-rejected:true
hosted-tool-rejected:true
provider-file-id-rejected:true
|}]
;;
