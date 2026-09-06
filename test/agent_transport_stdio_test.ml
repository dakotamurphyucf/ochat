open! Core

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let temporary_root env =
  let suffix =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  let root = Filename.concat "/tmp" ("ochat-stdio-test-" ^ suffix) in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
  root
;;

let with_embedded f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>You are a stdio transport test agent.</developer>";
        Eio.Switch.run (fun sw ->
          let options =
            Agent_server.Embedded.
              { prompt_file
              ; workspace
              ; tool_dir = workspace
              ; home = root
              ; data_root = None
              ; start_immediately = false
              ; permission_profile = Agent_server.Embedded.default_permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 128
              }
          in
          let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
          Exn.protect
            ~f:(fun () -> f sw embedded)
            ~finally:(fun () -> Agent_server.Embedded.close embedded)))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)))
;;

let initialize_line =
  {|{"jsonrpc":"2.0","id":1,"method":"protocol.initialize","params":{"implementation":{"name":"stdio-test","version":"1"},"protocol_min":{"major":1,"minor":0},"protocol_max":{"major":1,"minor":0},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":1048576}}|}
;;

let summarize_response line =
  match Jsonaf.of_string line |> Agent_protocol.Envelope.of_json with
  | Error error -> "decode:" ^ Agent_protocol.Error.code_to_string error.code
  | Ok (Response response) ->
    let id =
      response.id |> Agent_protocol.Envelope.Request_id.to_json |> Jsonaf.to_string
    in
    (match response.outcome with
     | Ok _ -> "success:" ^ id
     | Error error ->
       "failure:" ^ id ^ ":" ^ Agent_protocol.Error.code_to_string error.code)
  | Ok (Request _) -> "request"
  | Ok (Notification _) -> "notification"
;;

let output_lines buffer =
  Buffer.contents buffer |> String.split_lines |> List.filter ~f:(Fn.non String.is_empty)
;;

let%expect_test "stdio is ordered, request-aware, notification-safe, and closes on EOF" =
  with_embedded (fun sw embedded ->
    let input =
      String.concat
        ~sep:"\n"
        [ initialize_line
        ; {|{"jsonrpc":"2.0","id":"ping","method":"protocol.ping","params":{}}|}
        ; {|{"jsonrpc":"1.0","id":3,"method":"protocol.ping","params":{}}|}
        ; {|{"jsonrpc":"2.0","method":"protocol.ping","params":{}}|}
        ; ""
        ]
    in
    let output = Buffer.create 1_024 in
    let errors = ref [] in
    let close_count = ref 0 in
    Agent_transport_stdio.Server.run
      ~sw
      ~dispatcher:(Agent_server.Embedded.dispatcher embedded)
      ~close_connection:(fun context ->
        Int.incr close_count;
        Agent_server.Embedded.close_connection embedded context)
      ~principal:(Agent_server.Embedded.principal embedded)
      ~connection_id:"stdio-contract"
      ~input:(Eio.Flow.string_source input)
      ~output:(Eio.Flow.buffer_sink output)
      ~max_line_length:4_096
      ~outgoing_capacity:16
      ~max_attachments:64
      ~on_error:(fun error -> errors := error.code :: !errors);
    output_lines output |> List.map ~f:summarize_response |> List.iter ~f:print_endline;
    print_s
      [%sexp
        { close_count = (!close_count : int)
        ; errors = (List.rev !errors : Agent_protocol.Error.code list)
        }]);
  [%expect
    {|
    success:1
    success:"ping"
    failure:3:invalid_request
    ((close_count 1) (errors (Invalid_request)))
    |}]
;;

let%expect_test "stdio rejects an oversized line and still closes the connection" =
  with_embedded (fun sw embedded ->
    let output = Buffer.create 128 in
    let errors = ref [] in
    let close_count = ref 0 in
    Agent_transport_stdio.Server.run
      ~sw
      ~dispatcher:(Agent_server.Embedded.dispatcher embedded)
      ~close_connection:(fun context ->
        Int.incr close_count;
        Agent_server.Embedded.close_connection embedded context)
      ~principal:(Agent_server.Embedded.principal embedded)
      ~connection_id:"stdio-oversized"
      ~input:(Eio.Flow.string_source (String.make 128 'x'))
      ~output:(Eio.Flow.buffer_sink output)
      ~max_line_length:32
      ~outgoing_capacity:4
      ~max_attachments:64
      ~on_error:(fun error -> errors := error.code :: !errors);
    print_s
      [%sexp
        { close_count = (!close_count : int)
        ; output = (Buffer.contents output : string)
        ; errors = (List.rev !errors : Agent_protocol.Error.code list)
        }]);
  [%expect {| ((close_count 1) (output "") (errors (Invalid_request))) |}]
;;

let%expect_test "stdio gateway forwards typed requests and closes on EOF" =
  with_embedded (fun sw embedded ->
    let input =
      String.concat
        ~sep:"\n"
        [ initialize_line
        ; {|{"jsonrpc":"2.0","id":"gateway-ping","method":"protocol.ping","params":{}}|}
        ; ""
        ]
    in
    let output = Buffer.create 1_024 in
    let errors = ref [] in
    Agent_transport_stdio.Gateway.run
      ~sw
      ~connection:(Agent_server.Embedded.connect embedded)
      ~input:(Eio.Flow.string_source input)
      ~output:(Eio.Flow.buffer_sink output)
      ~max_line_length:4_096
      ~outgoing_capacity:16
      ~on_error:(fun error -> errors := error.code :: !errors);
    output_lines output |> List.map ~f:summarize_response |> List.iter ~f:print_endline;
    print_s [%sexp (List.rev !errors : Agent_protocol.Error.code list)]);
  [%expect
    {|
    success:1
    success:"gateway-ping"
    ()
    |}]
;;
