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

let headers ?first_byte reader =
  let first = Option.value first_byte ~default:"" ^ Eio.Buf_read.line reader in
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

let client_message ?first_byte reader =
  let header =
    Option.value first_byte ~default:""
    ^ Eio.Buf_read.take (if Option.is_some first_byte then 1 else 2) reader
  in
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

exception Submission_revoked of unit ref

let%test_unit "HTTP and WebSocket recheck the owner after transport selection waits" =
  run_env (fun env ->
    List.iter [ O.Transport_policy.Http_sse; Require_websocket ] ~f:(fun policy ->
      List.iter [ false; true ] ~f:(fun revoke ->
        let submitted = ref 0 in
        let server_done, resolve_server_done = Eio.Promise.create () in
        with_server
          env
          (fun flow ->
             Exn.protect
               ~finally:(fun () -> Eio.Promise.resolve resolve_server_done ())
               ~f:(fun () ->
                 let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
                 if O.Transport_policy.equal policy Require_websocket
                 then (
                   let _, fields = headers reader in
                   upgrade flow fields;
                   let first_byte = Eio.Buf_read.take 1 reader in
                   incr submitted;
                   ignore (client_message ~first_byte reader : Jsonaf.t);
                   Eio.Flow.copy_string (server_frame completed) flow)
                 else (
                   let first_byte = Eio.Buf_read.take 1 reader in
                   incr submitted;
                   let _, fields = headers ~first_byte reader in
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
                     flow)))
          (fun sw endpoint ->
             let session = D.Websocket_session.create ~sw in
             let selected, resolve_selected = Eio.Promise.create () in
             let resume, resolve_resume = Eio.Promise.create () in
             let current = ref true in
             let refreshes = ref 0 in
             let marker = ref () in
             let original_backtrace = Stdlib.Printexc.get_callstack 16 in
             let guard =
               Inference_runtime.Submission_guard.create ~refresh:(fun () ->
                 incr refreshes;
                 fun () ->
                   if not !current
                   then
                     Stdlib.Printexc.raise_with_backtrace
                       (Submission_revoked marker)
                       original_backtrace)
             in
             let rejected = ref false in
             Eio.Fiber.both
               (fun () ->
                  try
                    ignore
                      (D.run_with_transport
                         ~submission_guard:guard
                         (driver env)
                         ~session:(Some session)
                         ~policy
                         ~auth:(fun ~sw:_ _ -> Ok (lease ()))
                         ~prepared:(prepared (profile endpoint) [ input "guarded" ])
                         ~on_selected:(fun _ _ ->
                           Eio.Promise.resolve resolve_selected ();
                           Eio.Promise.await resume)
                         ~on_event:ignore
                       |> ok
                       : D.Terminal.t)
                  with
                  | Submission_revoked observed ->
                    assert (phys_equal observed marker);
                    let caught =
                      Stdlib.Printexc.raw_backtrace_to_string
                        (Stdlib.Printexc.get_raw_backtrace ())
                    in
                    let original =
                      Stdlib.Printexc.raw_backtrace_to_string original_backtrace
                    in
                    assert (not (String.is_empty original));
                    assert (
                      String.equal
                        (String.split_lines caught |> List.hd_exn)
                        (String.split_lines original |> List.hd_exn));
                    rejected := true)
               (fun () ->
                  Eio.Promise.await selected;
                  if revoke then current := false;
                  Eio.Promise.resolve resolve_resume ());
             D.Websocket_session.close session;
             Eio.Promise.await server_done;
             assert (Bool.equal !rejected revoke);
             assert (!refreshes = 1);
             assert (!submitted = if revoke then 0 else 1)))))
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

let%expect_test
    "graph root selection reuses B across operations and leaves moderator A alive"
  =
  run_env (fun env ->
    let connections = ref 0 in
    let bodies = ref [] in
    with_server
      env
      (fun flow ->
         incr connections;
         let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
         let _, fields = headers reader in
         upgrade flow fields;
         while true do
           let body = client_message reader in
           bodies := !bodies @ [ body ];
           Eio.Flow.copy_string (server_frame completed) flow
         done)
      (fun sw endpoint ->
         let module Runtime = Inference_runtime in
         let module R = Inference.Request in
         let selected_profile = profile endpoint in
         let driver = driver env in
         let resolve revision model =
           let target =
             Openai.Inference_adapter.capture_target
               selected_profile
               ~profile_revision:(Some revision)
               ~model
               ~settings:[]
               ~limits:Document_schema.Limits.default
             |> ok
           in
           let adapter =
             Openai.Inference_adapter.create
               driver
               ~profile:selected_profile
               ~profile_revision:(Some revision)
               ~auth:(Static (fun ~sw:_ _ -> Ok (lease ~revision:"credential" ())))
               ~limits:Runtime.Limits.default
             |> ok
           in
           Runtime.Context.create adapter ~target
           |> ok
           |> fun context ->
           Runtime.Context.with_transport_policy context Require_websocket
         in
         let owner = Runtime.Session.create ~sw in
         let original =
           Runtime.Context.with_session (resolve "1" "model-A") owner |> ok
         in
         let root = Chat_response.Root_binding.create owner in
         let item =
           Agent_session.History_codec.user_text
             ~id:(History_entry.Id.create ~namespace:"root-binding" ~sequence:0 |> ok)
             "input"
         in
         let execute context =
           let request =
             R.create
               ~target:(Runtime.Context.target context)
               ~history:[ item ]
               ~tools:[]
               ~assets:[]
               ~limits:Document_schema.Limits.default
             |> ok
           in
           let prepared =
             Runtime.Context.prepare context ~preparation_id:"root" request |> ok
           in
           let attempt =
             Runtime.Prepared.start
               prepared
               ~scope:Adapter_tests.scope
               ~accounting_id:Adapter_tests.accounting_id
             |> ok
           in
           Runtime.Attempt.run attempt ~sw ~on_event:ignore ~on_observation:ignore
           |> ok
           |> ignore
         in
         let capture revision model =
           Chat_response.Root_binding.with_context
             root
             ~resolved:(resolve revision model)
             ~f:execute
           |> ok
         in
         capture "1" "model-A";
         capture "1" "model-B";
         capture "1" "model-B";
         assert (!connections = 1);
         let has_previous body =
           Option.is_some (Jsonaf.member "previous_response_id" body)
         in
         assert (not (has_previous (List.nth_exn !bodies 1)));
         assert (has_previous (List.nth_exn !bodies 2));
         capture "2" "model-B";
         assert (!connections = 2);
         execute original;
         assert (!connections = 3);
         Chat_response.Root_binding.close root;
         assert (not (Runtime.Session.is_closed owner));
         Runtime.Session.close owner;
         print_endline
           "B reuses qualified WS across operations; changed envelope/revision resets; \
            moderator A survives"));
  [%expect
    {|B reuses qualified WS across operations; changed envelope/revision resets; moderator A survives|}]
;;

module Runtime = Inference_runtime
module Delivery = Runtime.Protected_delivery
module Payload = History_entry.Payload

let protected_text = "private fixture text"

let protected_history =
  let id sequence = History_entry.Id.create ~namespace:"protected-wire" ~sequence |> ok in
  let entry sequence view =
    let metadata = { Payload.Metadata.empty with call_id = Value "call-protected" } in
    let semantic = Payload.Semantic.create view ~metadata |> ok in
    History_entry.create_with_id ~id:(id sequence) (Payload.authored semantic)
  in
  let initial = Agent_session.History_codec.user_text ~id:(id 0) "initial" in
  let call =
    entry
      1
      (Call
         { kind = Function
         ; name = "read_note"
         ; namespace = Absent
         ; input_bytes = "{}"
         ; async = Absent
         })
  in
  let output =
    entry
      2
      (Result
         { kind = Function
         ; relation = Bound (History_entry.id call)
         ; output = Text "safe receipt"
         })
  in
  initial, [ initial; call; output ], call, output
;;

let protected_context
      ?(check_current = fun () -> Ok ())
      ?(cache_settings = [])
      env
      ~sw
      ~policy
      endpoint
  =
  let capabilities =
    D.Capability.create
      ~baseline:
        (D.Capability.
           [ Text_input, Supported; Function_tools, Supported; Websocket, Supported ]
         @ List.map cache_settings ~f:(fun (name, _) ->
           D.Capability.(Setting name, Supported)))
      ~models:[]
    |> ok
  in
  let profile =
    D.Profile.create
      ~id:"ws"
      ~account:(Some "account")
      ~endpoint
      ~capabilities
      ~defaults:[]
    |> ok
  in
  let target =
    Openai.Inference_adapter.capture_target
      profile
      ~profile_revision:None
      ~model:"qualified-model"
      ~settings:
        (List.map cache_settings ~f:(fun (name, value) ->
           D.Setting.create ~name ~value:(Value value) ~provenance:Captured_prompt |> ok))
      ~limits:Document_schema.Limits.default
    |> ok
  in
  let adapter =
    Openai.Inference_adapter.create
      (driver env)
      ~profile
      ~profile_revision:None
      ~auth:
        (Static
           (fun ~sw:_ _ ->
             D.Auth.with_identity
               (lease ~revision:"fixture-revision" ())
               ~owner:"host-owner"
               ~generation:1L
               ~check_current))
      ~limits:Runtime.Limits.default
    |> ok
  in
  let owner = Runtime.Session.create ~sw in
  let context = Runtime.Context.create adapter ~target |> ok in
  Runtime.Context.with_session
    (Runtime.Context.with_transport_policy context policy)
    owner
  |> ok
;;

let prepare_protected
      context
      ~id
      ?(protected_delivery = [])
      ?(protect_tool_arguments = false)
      history
  =
  let request =
    Inference.Request.create
      ~target:(Runtime.Context.target context)
      ~history
      ~tools:[]
      ~assets:[]
      ~limits:Document_schema.Limits.default
    |> ok
  in
  Runtime.Context.prepare
    context
    ~preparation_id:id
    ~protected_delivery
    ~protect_tool_arguments
    request
  |> ok
;;

let protected_scope ?(relation = Transcript.Scope.Root) id =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string "host" |> ok)
    ~attempt:(Transcript.Attempt_id.of_string id |> ok)
    ~relation
  |> ok
;;

let protected_attempt prepared ~scope =
  Runtime.Prepared.start prepared ~scope ~accounting_id:Adapter_tests.accounting_id |> ok
;;

let protected_plan ~call ~output ~refresh =
  Delivery.create ~call ~output ~materialize:(fun () -> Ok (Text protected_text)) ~refresh
;;

let verify_private_body body =
  match Jsonaf.member "input" body with
  | Some (`Array inputs) ->
    let is_tool_output item =
      Option.equal
        Document_schema.Json.equal
        (Jsonaf.member "type" item)
        (Some (`String "function_call_output"))
    in
    let output = List.find_exn inputs ~f:is_tool_output in
    assert (
      Option.equal
        Document_schema.Json.equal
        (Jsonaf.member "call_id" output)
        (Some (`String "call-protected")));
    assert (
      Option.equal
        Document_schema.Json.equal
        (Jsonaf.member "output" output)
        (Some (`String protected_text)))
  | _ -> assert false
;;

let%test_unit
    "both confidential legs use fresh WebSockets and ordinary prefixes still work"
  =
  run_env (fun env ->
    let initial, history, call, output = protected_history in
    let connections = ref 0 in
    let writes = ref [] in
    with_server
      env
      (fun flow ->
         incr connections;
         let connection = !connections in
         let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
         let _, fields = headers reader in
         upgrade flow fields;
         while true do
           let body = client_message reader in
           writes := (connection, body) :: !writes;
           Eio.Flow.copy_string
             (server_frame
                {|{"type":"future.echo","sequence_number":0,"opaque":"private fixture text"}|})
             flow;
           Eio.Flow.copy_string (server_frame completed) flow
         done)
      (fun sw endpoint ->
         let cache_settings =
           [ "prompt_cache_key", `String "ordinary fixture partition"
           ; "prompt_cache_retention", `String "24h"
           ; "prompt_cache_options", `Object [ "mode", `String "explicit" ]
           ]
         in
         let context =
           protected_context ~cache_settings env ~sw ~policy:Require_websocket endpoint
         in
         let execute ?relation ?(isolated = true) ?(private_input = false) prepared id =
           let module C = O.Configuration in
           let config = Runtime.Prepared.configuration prepared in
           let cache_setting name =
             List.find_exn (C.settings config) ~f:(fun setting ->
               C.Name.equal setting.name name)
           in
           let retention = cache_setting C.Name.Prompt_cache_retention in
           let key = cache_setting C.Name.Prompt_cache_key in
           (match isolated, retention.selection, key.selection with
            | true, Omitted, Omitted ->
              assert (C.withheld_settings config = 0);
              let policy =
                Adapter_tests.prepared_capacity_policy (Runtime.Prepared.target prepared)
              in
              let assessment = Runtime.Prepared.assess_capacity prepared ~policy in
              assert (
                match
                  private_input, Runtime.Context_capacity.Assessment.view assessment
                with
                | true, Unavailable Source_missing | false, Assessed _ -> true
                | _ -> false)
            | false, Value (Cache_retention `Hours_24), Withheld ->
              assert (C.withheld_settings config = 1)
            | _ -> assert false);
           let scope = protected_scope ?relation id in
           let attempt = protected_attempt prepared ~scope in
           let exposed_echo = ref false in
           let on_event event =
             match Inference.Event.view event with
             | Live live ->
               (match Transcript.Stream.view live with
                | Unknown_event { provider_kind; _ }
                  when String.equal provider_kind "future.echo" -> exposed_echo := true
                | _ -> ())
             | _ -> ()
           in
           let receipt =
             Runtime.Attempt.run attempt ~sw ~on_event ~on_observation:ignore |> ok
           in
           assert (Bool.equal !exposed_echo (not isolated));
           assert (
             Inference.Event.Terminal.equal_outcome
               (Inference.Event.Terminal.outcome (Runtime.Receipt.terminal receipt))
               Completed)
         in
         execute ~isolated:false (prepare_protected context ~id:"seed" [ initial ]) "seed";
         execute
           (prepare_protected
              context
              ~id:"generating"
              ~protect_tool_arguments:true
              [ initial ])
           "generating";
         let parent =
           { Transcript.Scope.scope = Transcript.Scope.key (protected_scope "generating")
           ; call_entry_id = None
           ; call_alias = None
           }
         in
         execute
           ~relation:(Nested parent)
           (prepare_protected
              (Runtime.Context.detach context)
              ~id:"nested"
              ~protect_tool_arguments:true
              [ initial ])
           "nested";
         let refreshes = ref 0 in
         let plan =
           protected_plan ~call ~output ~refresh:(fun ~scope ->
             assert (Transcript.Scope.equal scope (protected_scope "consuming"));
             incr refreshes;
             fun () -> true)
         in
         let consuming =
           prepare_protected context ~id:"consuming" ~protected_delivery:[ plan ] history
         in
         assert (
           String.equal (Runtime.Prepared.fingerprint consuming) "isolated:consuming");
         execute ~private_input:true consuming "consuming";
         Delivery.retire plan;
         let next =
           Agent_session.History_codec.user_text
             ~id:(History_entry.Id.create ~namespace:"protected-wire" ~sequence:3 |> ok)
             "next"
         in
         execute
           ~isolated:false
           (prepare_protected context ~id:"ordinary" [ initial; next ])
           "ordinary";
         let writes = List.rev !writes in
         assert (List.equal Int.equal (List.map writes ~f:fst) [ 1; 2; 3; 4; 1 ]);
         assert (
           List.equal
             Bool.equal
             (List.map writes ~f:(fun (_, body) ->
                Option.is_some (Jsonaf.member "previous_response_id" body)))
             [ false; false; false; false; true ]);
         verify_private_body (snd (List.nth_exn writes 3));
         List.iteri writes ~f:(fun index (connection, body) ->
           let wire = Jsonaf.to_string body in
           assert (
             Option.equal
               Document_schema.Json.equal
               (Jsonaf.member "store" body)
               (Some `False));
           List.iter cache_settings ~f:(fun (name, value) ->
             assert (
               Option.equal
                 Document_schema.Json.equal
                 (Jsonaf.member name body)
                 (if connection = 1 then Some value else None)));
           assert (not (String.is_substring wire ~substring:"safe receipt"));
           assert (
             Bool.equal (String.is_substring wire ~substring:protected_text) (index = 3)));
         let view =
           Payload.semantic (History_entry.payload output) |> Payload.Semantic.view
         in
         (match view with
          | Result { output = Text text; _ } -> assert (String.equal text "safe receipt")
          | _ -> assert false);
         assert (!refreshes = 1)))
;;

let%test_unit
    "late authority and auth failure or cancellation send no private application bytes"
  =
  run_env (fun env ->
    List.iter [ O.Transport_policy.Http_sse; Require_websocket ] ~f:(fun policy ->
      List.iter [ `Revoke_source; `Cancel; `Revoke_auth ] ~f:(fun change ->
        let _, history, call, output = protected_history in
        let submitted = ref false in
        let done_, resolve_done = Eio.Promise.create () in
        with_server
          env
          (fun flow ->
             Exn.protect
               ~finally:(fun () -> Eio.Promise.resolve resolve_done ())
               ~f:(fun () ->
                 let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
                 if O.Transport_policy.equal policy Require_websocket
                 then (
                   let _, fields = headers reader in
                   upgrade flow fields);
                 ignore (Eio.Buf_read.take 1 reader : string);
                 submitted := true))
          (fun sw endpoint ->
             let auth_current = ref true in
             let check_current () =
               if !auth_current then Ok () else Error D.Auth.Denied
             in
             let context = protected_context ~check_current env ~sw ~policy endpoint in
             let current = ref true in
             let refreshes = ref 0 in
             let plan =
               protected_plan ~call ~output ~refresh:(fun ~scope:_ ->
                 incr refreshes;
                 (match change with
                  | `Revoke_auth ->
                    Eio.Fiber.yield ();
                    auth_current := false
                  | `Revoke_source | `Cancel -> ());
                 fun () -> !current)
             in
             let prepared =
               prepare_protected
                 context
                 ~id:"private"
                 ~protected_delivery:[ plan ]
                 history
             in
             let scope = protected_scope "private" in
             let attempt = protected_attempt prepared ~scope in
             let selected, resolve_selected = Eio.Promise.create () in
             let dispatch () =
               Runtime.Attempt.run
                 attempt
                 ~sw
                 ~on_event:ignore
                 ~on_observation:(fun observation ->
                   match O.payload observation with
                   | Transport_selection _ ->
                     (match change with
                      | `Revoke_source -> current := false
                      | `Cancel ->
                        Eio.Promise.resolve resolve_selected ();
                        Eio.Fiber.await_cancel ()
                      | `Revoke_auth -> ())
                   | _ -> ())
               |> ok
             in
             (match change with
              | `Cancel ->
                Eio.Fiber.first
                  (fun () -> ignore (dispatch () : Runtime.Receipt.t))
                  (fun () -> Eio.Promise.await selected)
              | `Revoke_source ->
                (try
                   ignore (dispatch () : Runtime.Receipt.t);
                   assert false
                 with
                 | Delivery.Rejected Unavailable -> ())
              | `Revoke_auth ->
                let terminal = Runtime.Receipt.terminal (dispatch ()) in
                let module Terminal = Inference.Event.Terminal in
                assert (
                  Terminal.equal_outcome
                    (Terminal.outcome terminal)
                    (Failed (Authentication Denied)));
                assert (
                  Terminal.equal_delivery
                    (Terminal.delivery terminal)
                    Definitely_not_submitted));
             Eio.Promise.await done_;
             assert (not !submitted);
             assert (
               !refreshes
               =
               match change with
               | `Cancel -> 0
               | `Revoke_source | `Revoke_auth -> 1);
             assert (
               match
                 Runtime.Prepared.start
                   prepared
                   ~scope
                   ~accounting_id:Adapter_tests.accounting_id
               with
               | Error (Protected_delivery Expired) -> true
               | _ -> false);
             Delivery.retire plan))))
;;

let%test_unit
    "protected fallback is allowed before the application write and never after it"
  =
  run_env (fun env ->
    List.iter [ false; true ] ~f:(fun upgraded ->
      let _, history, call, output = protected_history in
      let connections = ref [] in
      let writes = ref 0 in
      with_server
        env
        (fun flow ->
           let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
           let first, fields = headers reader in
           let websocket = String.is_prefix first ~prefix:"GET " in
           connections := websocket :: !connections;
           if websocket
           then
             if upgraded
             then (
               upgrade flow fields;
               verify_private_body (client_message reader);
               incr writes)
             else
               Eio.Flow.copy_string
                 "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\n\r\n"
                 flow
           else (
             let length =
               List.Assoc.find_exn fields ~equal:String.equal "content-length"
               |> Int.of_string
             in
             verify_private_body (Eio.Buf_read.take length reader |> Jsonaf.of_string);
             incr writes;
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
           let refreshes = ref 0 in
           let context = protected_context env ~sw ~policy:Prefer_websocket endpoint in
           let plan =
             protected_plan ~call ~output ~refresh:(fun ~scope:_ ->
               incr refreshes;
               fun () -> true)
           in
           let prepared =
             prepare_protected
               context
               ~id:"fallback-private"
               ~protected_delivery:[ plan ]
               history
           in
           let scope = protected_scope "fallback-private" in
           let attempt = protected_attempt prepared ~scope in
           let receipt =
             Runtime.Attempt.run attempt ~sw ~on_event:ignore ~on_observation:ignore |> ok
           in
           Delivery.retire plan;
           let terminal = Runtime.Receipt.terminal receipt in
           let module Terminal = Inference.Event.Terminal in
           if upgraded
           then (
             assert (List.equal Bool.equal !connections [ true ]);
             assert (
               Terminal.equal_delivery (Terminal.delivery terminal) Possibly_submitted))
           else (
             assert (List.equal Bool.equal !connections [ false; true ]);
             assert (Terminal.equal_outcome (Terminal.outcome terminal) Completed));
           assert (!writes = 1);
           assert (!refreshes = 1))))
;;
