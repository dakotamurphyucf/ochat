open! Core
module D = Openai.Responses_driver
module O = Inference.Observation

let ok result =
  Result.map_error result ~f:(fun _ -> "fixture admission") |> Result.ok_or_failwith
;;

let run_env f =
  Eio_main.run (fun env ->
    (Mirage_crypto_rng_eio.run [@alert "-deprecated"])
      (module Mirage_crypto_rng.Fortuna)
      env
      (fun () -> f env))
;;

let accept nonce =
  Digestif.SHA1.(
    digest_string (nonce ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") |> to_raw_string)
  |> Base64.encode_exn
;;

let headers reader =
  let first = Eio.Buf_read.line reader in
  let rec loop fields =
    let line = Eio.Buf_read.line reader in
    if String.is_empty line
    then first, fields
    else (
      let key, value = String.lsplit2 line ~on:':' |> Option.value_exn in
      loop ((String.lowercase key, String.strip value) :: fields))
  in
  loop []
;;

let upgrade flow fields =
  let nonce = List.Assoc.find_exn fields ~equal:String.equal "sec-websocket-key" in
  Eio.Flow.copy_string
    (sprintf
       "HTTP/1.1 101 Switching Protocols\r\n\
        Upgrade: websocket\r\n\
        Connection: Upgrade\r\n\
        Sec-WebSocket-Accept: %s\r\n\
        \r\n"
       (accept nonce))
    flow
;;

let client_message reader =
  let header = Eio.Buf_read.take 2 reader in
  assert (Char.to_int header.[0] = 0x81);
  let short = Char.to_int header.[1] land 127 in
  assert (Char.to_int header.[1] land 128 <> 0);
  let len =
    if short < 126
    then short
    else (
      let bytes = Eio.Buf_read.take (if short = 126 then 2 else 8) reader in
      String.fold bytes ~init:0 ~f:(fun acc c -> (acc lsl 8) lor Char.to_int c))
  in
  let mask = Eio.Buf_read.take 4 reader in
  let payload = Eio.Buf_read.take len reader in
  String.mapi payload ~f:(fun i c ->
    Char.of_int_exn (Char.to_int c lxor Char.to_int mask.[i mod 4]))
  |> Jsonaf.of_string
;;

let server_frame ?(first = 0x81) text =
  let len = String.length text in
  let header =
    if len < 126
    then String.of_char_list [ Char.of_int_exn first; Char.of_int_exn len ]
    else
      String.of_char_list
        [ Char.of_int_exn first
        ; Char.of_int_exn 126
        ; Char.of_int_exn (len lsr 8)
        ; Char.of_int_exn (len land 255)
        ]
  in
  header ^ text
;;

let completed =
  {|{"type":"response.completed","sequence_number":1,"response":{"object":"response","id":"response-one","status":"completed","output":[]}}|}
;;

let with_server env handler f =
  Eio.Switch.run (fun sw ->
    let socket =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:8
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
        Eio.Fiber.fork_daemon ~sw (fun () ->
          Exn.protect
            ~finally:(fun () -> Eio.Flow.close flow)
            ~f:(fun () ->
              try handler flow with
              | End_of_file -> ());
          `Stop_daemon)
      done);
    f sw (sprintf "http://127.0.0.1:%d/v1/responses" port))
;;

let profile endpoint =
  let capabilities =
    D.Capability.create
      ~baseline:[ Text_input, Supported; Websocket, Supported ]
      ~models:[]
    |> ok
  in
  D.Profile.create ~id:"ws" ~account:(Some "account") ~endpoint ~capabilities ~defaults:[]
  |> ok
;;

let prepared profile input =
  D.Prepared.create profile ~model:"qualified-model" ~history:input ~tools:[] ~settings:[]
  |> ok
;;

let input text = `Object [ "role", `String "user"; "content", `String text ]

let driver env =
  D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:2. ()
  |> ok
;;

let lease ?revision () =
  let lease = D.Auth.bearer "fixture-token" |> ok in
  let lease =
    D.Auth.with_identity
      lease
      ~owner:"host-owner"
      ~generation:1L
      ~check_current:(fun () -> Ok ())
    |> ok
  in
  match revision with
  | None -> lease
  | Some revision -> D.Auth.with_credential_revision lease revision |> ok
;;

let dispatch driver session auth prepared policy =
  D.run_with_transport
    driver
    ~session:(Some session)
    ~policy
    ~auth
    ~prepared
    ~on_selected:(fun selected fallback ->
      print_s
        [%sexp
          (selected : O.Transport_selection.transport)
        , (fallback : O.Transport_selection.fallback_reason option)])
    ~on_event:ignore
;;

let%expect_test
    "same epoch revision rotation reconnects with full history; stable revision continues"
  =
  run_env (fun env ->
    let connections = ref 0 in
    let requests = ref [] in
    with_server
      env
      (fun flow ->
         incr connections;
         let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
         let _, fields = headers reader in
         upgrade flow fields;
         while true do
           let body = client_message reader in
           requests := body :: !requests;
           Eio.Flow.copy_string (server_frame completed) flow
         done)
      (fun sw endpoint ->
         let driver = driver env in
         let profile = profile endpoint in
         let session = D.Websocket_session.create ~sw in
         let revision = ref (Some "credential-op-1") in
         let auth ~sw:_ _ = Ok (lease ?revision:!revision ()) in
         let run history =
           dispatch driver session auth (prepared profile history) Require_websocket
           |> ok
           |> ignore
         in
         run [ input "first" ];
         run [ input "first"; input "second" ];
         revision := Some "credential-op-2";
         run [ input "first"; input "second"; input "third" ];
         revision := None;
         run [ input "fourth" ];
         run [ input "fifth" ];
         D.Websocket_session.close session;
         print_s [%sexp (!connections : int)];
         List.iter (List.rev !requests) ~f:(fun json ->
           let fields =
             match json with
             | `Object fields -> fields
             | _ -> assert false
           in
           let continued =
             List.Assoc.mem fields ~equal:String.equal "previous_response_id"
           in
           let count =
             match List.Assoc.find_exn fields ~equal:String.equal "input" with
             | `Array input -> List.length input
             | _ -> assert false
           in
           print_s [%sexp (continued : bool), (count : int)])));
  [%expect
    {|
    (Websocket ())
    (Websocket ())
    (Websocket ())
    (Websocket ())
    (Websocket ())
    4
    (false 1)
    (true 1)
    (false 3)
    (false 1)
    (false 1)
    |}]
;;

let%expect_test "authentication upgrade statuses never fall back" =
  run_env (fun env ->
    List.iter [ 401; 403 ] ~f:(fun status ->
      let connections = ref 0 in
      with_server
        env
        (fun flow ->
           incr connections;
           let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
           ignore (headers reader : string * (string * string) list);
           Eio.Flow.copy_string
             (sprintf "HTTP/1.1 %d Rejected\r\nContent-Length: 0\r\n\r\n" status)
             flow)
        (fun sw endpoint ->
           let session = D.Websocket_session.create ~sw in
           let auth ~sw:_ _ = Ok (lease ~revision:"op" ()) in
           let result =
             dispatch
               (driver env)
               session
               auth
               (prepared (profile endpoint) [])
               Prefer_websocket
           in
           D.Websocket_session.close session;
           (match result with
            | Error error -> print_s [%sexp (error : D.Auth.error)]
            | Ok _ -> failwith "auth failure retried");
           print_s [%sexp (!connections : int)])));
  [%expect
    {|
    Reauthorization_required
    1
    Denied
    1
    |}]
;;

let%expect_test "oversized advertised incoming length rejected without payload or retry" =
  run_env (fun env ->
    with_server
      env
      (fun flow ->
         let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
         let _, fields = headers reader in
         upgrade flow fields;
         ignore (client_message reader : Jsonaf.t);
         (* 2 MiB advertised, no payload: request bound16MiB must not enlarge incoming1MiB cap. *)
         Eio.Flow.copy_string "\129\127\000\000\000\000\000\032\000\000" flow;
         ignore (Eio.Buf_read.take 1 reader : string))
      (fun sw endpoint ->
         let session = D.Websocket_session.create ~sw in
         let auth ~sw:_ _ = Ok (lease ~revision:"op" ()) in
         let result =
           dispatch
             (driver env)
             session
             auth
             (prepared (profile endpoint) [])
             Prefer_websocket
           |> ok
         in
         D.Websocket_session.close session;
         match result with
         | Failed { delivery; reason } ->
           print_s [%sexp (delivery : D.Terminal.delivery), (reason : D.Terminal.failure)]
         | Provider _ -> failwith "oversized message accepted"));
  [%expect
    {|
    (Websocket ())
    (Possibly_submitted Body_limit)
    |}]
;;

let%expect_test
    "fragmented text with interleaved ping produces the same validated outcome"
  =
  run_env (fun env ->
    with_server
      env
      (fun flow ->
         let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
         let _, fields = headers reader in
         upgrade flow fields;
         ignore (client_message reader : Jsonaf.t);
         let split = String.length completed / 2 in
         let first = String.prefix completed split in
         let last = String.drop_prefix completed split in
         Eio.Flow.copy_string
           (server_frame ~first:0x01 first
            ^ server_frame ~first:0x89 "ping"
            ^ server_frame ~first:0x80 last)
           flow;
         let pong = Eio.Buf_read.take 2 reader in
         assert (Char.to_int pong.[0] = 0x8a);
         assert (Char.to_int pong.[1] = 0x84);
         let mask = Eio.Buf_read.take 4 reader in
         let payload = Eio.Buf_read.take 4 reader in
         let unmasked =
           String.mapi payload ~f:(fun i c ->
             Char.of_int_exn (Char.to_int c lxor Char.to_int mask.[i]))
         in
         assert (String.equal unmasked "ping");
         ignore (Eio.Buf_read.take 1 reader : string))
      (fun sw endpoint ->
         let session = D.Websocket_session.create ~sw in
         let auth ~sw:_ _ = Ok (lease ~revision:"op" ()) in
         let outcome =
           dispatch
             (driver env)
             session
             auth
             (prepared (profile endpoint) [])
             Require_websocket
           |> ok
         in
         D.Websocket_session.close session;
         match outcome with
         | Provider (Response { terminal = Completed; _ }) ->
           print_endline "completed; pong validated"
         | Provider _ | Failed _ -> failwith "fragmented response lost"));
  [%expect
    {|
    (Websocket ())
    completed; pong validated
    |}]
;;

let%expect_test "malformed frames and bounded fragment/control floods never resend" =
  let cases =
    [ "reserved bits", server_frame ~first:0xc1 completed
    ; "masked server", "\129\128"
    ; "nonminimal length", "\129\126\000\001"
    ; "continuation without start", server_frame ~first:0x80 "x"
    ; "fragmented control", server_frame ~first:0x09 ""
    ; "binary", server_frame ~first:0x82 ""
    ; "invalid utf8", server_frame "\255"
    ; "invalid close", server_frame ~first:0x88 "x"
    ; ( "control flood"
      , String.concat (List.init 1025 ~f:(fun _ -> server_frame ~first:0x8a "")) )
    ; ( "fragment flood"
      , server_frame ~first:0x01 ""
        ^ String.concat (List.init 4096 ~f:(fun _ -> server_frame ~first:0x00 "")) )
    ]
  in
  run_env (fun env ->
    List.iter cases ~f:(fun (label, bytes) ->
      let connections = ref 0 in
      with_server
        env
        (fun flow ->
           incr connections;
           let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
           let _, fields = headers reader in
           upgrade flow fields;
           ignore (client_message reader : Jsonaf.t);
           Eio.Flow.copy_string bytes flow;
           ignore (Eio.Buf_read.take 1 reader : string))
        (fun sw endpoint ->
           let session = D.Websocket_session.create ~sw in
           let auth ~sw:_ _ = Ok (lease ~revision:"op" ()) in
           let result =
             D.run_with_transport
               (driver env)
               ~session:(Some session)
               ~policy:Prefer_websocket
               ~auth
               ~prepared:(prepared (profile endpoint) [])
               ~on_selected:(fun _ _ -> ())
               ~on_event:ignore
             |> ok
           in
           D.Websocket_session.close session;
           match result with
           | Failed { delivery; reason } ->
             print_s
               [%sexp
                 (label : string)
               , (delivery : D.Terminal.delivery)
               , (reason : D.Terminal.failure)
               , (!connections : int)]
           | Provider _ -> failwith "invalid frame accepted")));
  [%expect
    {|
    ("reserved bits" Possibly_submitted Protocol 1)
    ("masked server" Possibly_submitted Protocol 1)
    ("nonminimal length" Possibly_submitted Protocol 1)
    ("continuation without start" Possibly_submitted Protocol 1)
    ("fragmented control" Possibly_submitted Protocol 1)
    (binary Possibly_submitted Protocol 1)
    ("invalid utf8" Possibly_submitted Protocol 1)
    ("invalid close" Possibly_submitted Protocol 1)
    ("control flood" Possibly_submitted Body_limit 1)
    ("fragment flood" Possibly_submitted Body_limit 1)
    |}]
;;

let%expect_test
    "fresh authorization on a reused channel denies second inference without resend"
  =
  run_env (fun env ->
    let received = ref 0 in
    with_server
      env
      (fun flow ->
         let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
         let _, fields = headers reader in
         upgrade flow fields;
         while true do
           ignore (client_message reader : Jsonaf.t);
           incr received;
           Eio.Flow.copy_string (server_frame completed) flow
         done)
      (fun sw endpoint ->
         let session = D.Websocket_session.create ~sw in
         let lookups = ref 0 in
         let auth ~sw:_ _ =
           incr lookups;
           D.Auth.with_identity
             (lease ~revision:"op" ())
             ~owner:"host-owner"
             ~generation:1L
             ~check_current:(fun () -> if !lookups > 1 then Error Denied else Ok ())
         in
         ignore
           (dispatch
              (driver env)
              session
              auth
              (prepared (profile endpoint) [ input "first" ])
              Prefer_websocket
            |> ok
            : D.Terminal.t);
         let result =
           dispatch
             (driver env)
             session
             auth
             (prepared (profile endpoint) [ input "first"; input "second" ])
             Prefer_websocket
         in
         D.Websocket_session.close session;
         (match result with
          | Error error -> print_s [%sexp (error : D.Auth.error)]
          | Ok _ -> failwith "revoked source lease submitted");
         print_s [%sexp (!lookups : int), (!received : int)]));
  [%expect
    {|
    (Websocket ())
    Denied
    (2 1)
    |}]
;;

let%expect_test "prefer fallback preserves admission fingerprint and reports actual SSE" =
  run_env (fun env ->
    let calls = ref [] in
    with_server
      env
      (fun flow ->
         let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
         let first, fields = headers reader in
         let is_upgrade = String.is_prefix first ~prefix:"GET " in
         calls := is_upgrade :: !calls;
         if is_upgrade
         then
           Eio.Flow.copy_string
             "HTTP/1.1 400 Not Supported\r\nContent-Length: 0\r\n\r\n"
             flow
         else (
           let length =
             List.Assoc.find_exn fields ~equal:String.equal "content-length"
             |> Int.of_string
           in
           ignore (Eio.Buf_read.take length reader : string);
           let body = "data: " ^ completed ^ "\n\n" in
           Eio.Flow.copy_string
             (sprintf
                "HTTP/1.1 200 OK\r\n\
                 Content-Type: text/event-stream\r\n\
                 Content-Length: %d\r\n\
                 \r\n\
                 %s"
                (String.length body)
                body)
             flow))
      (fun sw endpoint ->
         let profile = profile endpoint in
         let target =
           Openai.Inference_adapter.capture_target
             profile
             ~profile_revision:None
             ~model:"qualified-model"
             ~settings:[]
             ~limits:Document_schema.Limits.default
           |> ok
         in
         let auth ~sw:_ _ = Ok (lease ~revision:"op" ()) in
         let adapter =
           Openai.Inference_adapter.create
             (driver env)
             ~profile
             ~profile_revision:None
             ~auth:(Openai.Inference_adapter.Auth_source.Static auth)
             ~limits:Inference_runtime.Limits.default
           |> ok
         in
         let context =
           Inference_runtime.Context.create adapter ~target
           |> ok
           |> fun context ->
           Inference_runtime.Context.with_transport_policy context Prefer_websocket
         in
         let session = Inference_runtime.Session.create ~sw in
         let context = Inference_runtime.Context.with_session context session |> ok in
         let request =
           Inference.Request.create
             ~target
             ~history:[]
             ~tools:[]
             ~assets:[]
             ~limits:Document_schema.Limits.default
           |> ok
         in
         let prepared =
           Inference_runtime.Context.prepare context ~preparation_id:"fallback" request
           |> ok
         in
         let fingerprint = Inference_runtime.Prepared.fingerprint prepared in
         let scope =
           Transcript.Scope.create
             ~source:(Transcript.Source_id.of_string "mock" |> ok)
             ~attempt:(Transcript.Attempt_id.of_string "fallback" |> ok)
             ~relation:Root
           |> ok
         in
         let accounting_id = O.Observation_id.of_string "fallback-usage" |> ok in
         let attempt =
           Inference_runtime.Prepared.start prepared ~scope ~accounting_id |> ok
         in
         let selected = ref None in
         let receipt =
           Inference_runtime.Attempt.run
             attempt
             ~sw
             ~on_event:ignore
             ~on_observation:(fun observation ->
               match O.payload observation with
               | Transport_selection selection -> selected := Some selection
               | _ -> ())
           |> ok
         in
         Inference_runtime.Session.close session;
         assert (
           String.equal fingerprint (Inference_runtime.Prepared.fingerprint prepared));
         let configuration = Inference_runtime.Prepared.configuration prepared in
         print_s
           [%sexp (O.Configuration.transport configuration : O.Configuration.transport)];
         let selection = Option.value_exn !selected in
         print_s
           [%sexp
             (O.Transport_selection.selected selection : O.Transport_selection.transport)
           , (O.Transport_selection.fallback selection
              : O.Transport_selection.fallback_reason option)];
         print_s
           [%sexp
             (Inference.Event.Terminal.outcome
                (Inference_runtime.Receipt.terminal receipt)
              : Inference.Event.Terminal.outcome)];
         print_s [%sexp (List.rev !calls : bool list)]));
  [%expect
    {|
    Websocket
    (Http_sse (Upgrade))
    Completed
    (true false)
    |}]
;;

let%expect_test "edited history, model and resolved asset bytes invalidate continuation" =
  run_env (fun env ->
    let continued = ref [] in
    with_server
      env
      (fun flow ->
         let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
         let _, fields = headers reader in
         upgrade flow fields;
         while true do
           let body = client_message reader in
           let fields =
             match body with
             | `Object fields -> fields
             | _ -> assert false
           in
           continued
           := List.Assoc.mem fields ~equal:String.equal "previous_response_id"
              :: !continued;
           Eio.Flow.copy_string (server_frame completed) flow
         done)
      (fun sw endpoint ->
         let driver = driver env in
         let profile = profile endpoint in
         let session = D.Websocket_session.create ~sw in
         let auth ~sw:_ _ = Ok (lease ~revision:"op" ()) in
         let asset bytes =
           Inference.Request.Asset.create
             ~reference:"image"
             ~kind:Image
             ~media_type:"image/png"
             ~bytes
             ~max_bytes:100
           |> ok
         in
         let run model history assets =
           let prepared =
             D.Prepared.create profile ~model ~history ~tools:[] ~settings:[] |> ok
           in
           D.run_with_transport
             ~cache_assets:assets
             driver
             ~session:(Some session)
             ~policy:Require_websocket
             ~auth
             ~prepared
             ~on_selected:(fun _ _ -> ())
             ~on_event:ignore
           |> ok
           |> ignore
         in
         run "model" [ input "first" ] [ asset "old" ];
         run "model" [ input "first"; input "second" ] [ asset "old" ];
         run "model" [ input "edited"; input "second" ] [ asset "old" ];
         run
           "other-model"
           [ input "edited"; input "second"; input "third" ]
           [ asset "old" ];
         run
           "other-model"
           [ input "edited"; input "second"; input "third"; input "fourth" ]
           [ asset "new" ];
         D.Websocket_session.close session;
         print_s [%sexp (List.rev !continued : bool list)]));
  [%expect {| (false true false false false) |}]
;;

let%expect_test "cancellation during response.create write never falls back to SSE" =
  run_env (fun env ->
    let started, resolve_started = Eio.Promise.create () in
    let connections = ref 0 in
    with_server
      env
      (fun flow ->
         incr connections;
         let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
         let _, fields = headers reader in
         upgrade flow fields;
         ignore (Eio.Buf_read.take 2 reader : string);
         Eio.Promise.resolve resolve_started ();
         Eio.Fiber.await_cancel ())
      (fun sw endpoint ->
         let session = D.Websocket_session.create ~sw in
         let auth ~sw:_ _ = Ok (lease ~revision:"op" ()) in
         let selected = ref [] in
         Eio.Fiber.first
           (fun () ->
              D.run_with_transport
                (driver env)
                ~session:(Some session)
                ~policy:Prefer_websocket
                ~auth
                ~prepared:
                  (prepared
                     (profile endpoint)
                     [ input (String.make (8 * 1024 * 1024) 'x') ])
                ~on_selected:(fun transport _ -> selected := transport :: !selected)
                ~on_event:ignore
              |> ok
              |> ignore;
              failwith "large write unexpectedly completed")
           (fun () -> Eio.Promise.await started);
         D.Websocket_session.close session;
         print_s
           [%sexp
             (!connections : int)
           , (List.rev !selected : O.Transport_selection.transport list)]));
  [%expect {| (1 (Websocket)) |}]
;;

let%expect_test "concurrent detached calls own independent ephemeral channels" =
  run_env (fun env ->
    let both, resolve_both = Eio.Promise.create () in
    let connections = ref 0 in
    let full_requests = ref 0 in
    with_server
      env
      (fun flow ->
         incr connections;
         let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
         let _, fields = headers reader in
         upgrade flow fields;
         let body = client_message reader in
         let fields =
           match body with
           | `Object fields -> fields
           | _ -> assert false
         in
         if not (List.Assoc.mem fields ~equal:String.equal "previous_response_id")
         then incr full_requests;
         if !full_requests = 2 then Eio.Promise.resolve resolve_both ();
         Eio.Promise.await both;
         Eio.Flow.copy_string (server_frame completed) flow)
      (fun _sw endpoint ->
         let driver = driver env in
         let profile = profile endpoint in
         let auth ~sw:_ _ = Ok (lease ~revision:"same-op" ()) in
         let run text =
           D.run_with_transport
             driver
             ~session:None
             ~policy:Require_websocket
             ~auth
             ~prepared:(prepared profile [ input text ])
             ~on_selected:(fun _ _ -> ())
             ~on_event:ignore
           |> ok
           |> ignore
         in
         Eio.Fiber.both (fun () -> run "child-one") (fun () -> run "child-two");
         print_s [%sexp (!connections : int), (!full_requests : int)]));
  [%expect {| (2 2) |}]
;;
