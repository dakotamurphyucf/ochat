open Core

type fault =
  | Valid
  | Interrupted
  | Malformed_json
  | Invalid_base64
  | Wrong_digest
  | Wrong_offset
  | Wrong_next_offset
  | Metadata_change
  | Early_eof
  | No_progress

let faults =
  [ "interrupted", Interrupted
  ; "malformed-json", Malformed_json
  ; "invalid-base64", Invalid_base64
  ; "wrong-digest", Wrong_digest
  ; "wrong-offset", Wrong_offset
  ; "wrong-next-offset", Wrong_next_offset
  ; "metadata-change", Metadata_change
  ; "early-eof", Early_eof
  ; "no-progress", No_progress
  ]
;;

let require condition message = if not condition then failwith message

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let string_ok = function
  | Ok value -> value
  | Error message -> failwith message
;;

let contents = "first-second"

let blob () =
  Agent_protocol.Blob.Metadata.create
    ~id:(Agent_protocol.Id.Blob.create ())
    ~kind:File
    ~media_type:"application/json"
    ~byte_length:(Int64.of_int (String.length contents))
    ~digest:Digestif.SHA256.(digest_string contents |> to_hex)
    ()
  |> protocol_ok
;;

let rec content_length reader length =
  match Eio.Buf_read.line reader |> String.strip with
  | "" -> length
  | line ->
    let length =
      match String.lsplit2 line ~on:':' with
      | Some (name, value) when String.Caseless.equal name "content-length" ->
        Int.of_string (String.strip value)
      | _ -> length
    in
    content_length reader length
;;

let read_request flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:65536 in
  let line = Eio.Buf_read.line reader in
  require
    (String.is_prefix line ~prefix:"POST /v1/rpc ")
    "fault fixture received a non-RPC request";
  let length = content_length reader 0 in
  let body = Eio.Buf_read.take length reader |> Jsonaf.of_string in
  match Agent_protocol.Envelope.of_json body |> protocol_ok with
  | Request request ->
    require
      (String.equal request.method_ "blob.read")
      "fault fixture received a non-blob request";
    request.id, Agent_protocol.Blob.Read_request.of_json request.params |> protocol_ok
  | _ -> failwith "expected blob read request envelope"
;;

let chunk blob first =
  let data = if first then "first-" else "second" in
  Agent_protocol.Blob.Chunk.
    { blob
    ; offset = (if first then 0L else 6L)
    ; next_offset = (if first then 6L else 12L)
    ; data_base64 = Base64.encode_exn data
    ; eof = not first
    }
;;

let corrupt fault (chunk : Agent_protocol.Blob.Chunk.t) =
  match fault with
  | Valid | Interrupted | Malformed_json -> chunk
  | Invalid_base64 -> { chunk with data_base64 = "!not-base64!" }
  | Wrong_digest -> { chunk with data_base64 = Base64.encode_exn "broken" }
  | Wrong_offset -> { chunk with offset = 5L }
  | Wrong_next_offset -> { chunk with next_offset = 11L }
  | Metadata_change -> { chunk with blob = { chunk.blob with media_type = "text/plain" } }
  | Early_eof -> { chunk with data_base64 = Base64.encode_exn "sec"; next_offset = 9L }
  | No_progress -> { chunk with data_base64 = ""; next_offset = 6L; eof = false }
;;

let body fault blob first id =
  match fault, first with
  | Malformed_json, false -> "{invalid-json"
  | _ ->
    let chunk = chunk blob first in
    let chunk = if first then chunk else corrupt fault chunk in
    Agent_protocol.Method_result.Blob_read chunk
    |> Agent_protocol.Method_result.to_json
    |> Agent_protocol.Envelope.success ~id
    |> Agent_protocol.Envelope.to_json
    |> Jsonaf.to_string
;;

let respond flow fault blob first id =
  let body = body fault blob first id in
  let transmitted =
    match fault, first with
    | Interrupted, false -> String.prefix body 20
    | _ -> body
  in
  let headers =
    sprintf
      "HTTP/1.1 200 OK\r\n\
       Content-Type: application/json\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n"
      (String.length body)
  in
  Eio.Flow.copy_string (headers ^ transmitted) flow;
  Eio.Flow.shutdown flow `Send
;;

let listen ~sw env =
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:false
      ~reuse_port:false
      ~backlog:8
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> failwith "expected TCP fault server"
  in
  socket, port
;;

let serve ~sw socket fault blob on_partial requests =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    while true do
      Eio.Switch.run (fun client_sw ->
        let flow, _ = Eio.Net.accept ~sw:client_sw socket in
        let id, request = read_request flow in
        let first = Int64.equal request.offset 0L in
        Int.incr requests;
        require
          (first || Int64.equal request.offset 6L)
          "download requested an unexpected cursor";
        if not first then on_partial ();
        respond flow fault blob first id)
    done;
    `Stop_daemon)
;;

let connection client =
  Agent_client.Transport.create
    ~request:(fun command ->
      Http_driver.request client command
      |> Result.map ~f:(fun response -> response.result))
    ~next_notification:(fun () -> None)
    ~close:(fun () -> ())
  |> Agent_client.Connection.create
;;

let check_original path existing =
  if existing
  then
    require
      (String.equal (Eio.Path.load path) "previous-export")
      "failure replaced the existing export"
  else require (not (Eio.Path.is_file path)) "failure installed a partial export"
;;

let partial_assertion parent path existing () =
  check_original path existing;
  let partials =
    Eio.Path.read_dir parent
    |> List.filter ~f:(String.is_substring ~substring:".ochat-part-")
  in
  require (List.length partials = 1) "download did not create exactly one staging file";
  let staging = Eio.Path.(parent / List.hd_exn partials) |> Eio.Path.load in
  require (String.equal staging "first-") "fault did not occur after a real partial write"
;;

let assert_result fault result =
  match fault with
  | Valid -> Or_error.ok_exn result
  | Interrupted
  | Malformed_json
  | Invalid_base64
  | Wrong_digest
  | Wrong_offset
  | Wrong_next_offset
  | Metadata_change
  | Early_eof
  | No_progress ->
    require (Result.is_error result) "atomic installer accepted corrupt HTTP transfer"
;;

let download ~sw env parent path existing fault =
  let socket, port = listen ~sw env in
  let blob = blob () in
  let requests = ref 0 in
  serve ~sw socket fault blob (partial_assertion parent path existing) requests;
  let client = Http_driver.create ~sw ~env ~port ~token:None |> string_ok in
  let connection = connection client in
  let download output =
    Agent_client.Blob_download.download
      ~connection
      ~session_id:(Agent_protocol.Id.Session.create ())
      ~attachment_id:(Agent_protocol.Id.Attachment.create ())
      ~blob
      ~output
  in
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Agent_client.Blob_download.install_atomic ~path ~download)
  in
  assert_result fault result;
  require
    (!requests >= 2)
    "failure fixture did not reach the interrupted second HTTP response";
  Http_driver.shutdown client
;;

let assert_target fault parent path existing =
  let installed =
    match fault with
    | Valid -> true
    | _ -> false
  in
  if installed
  then
    require
      (String.equal (Eio.Path.load path) contents)
      "two-chunk HTTP success installed incorrect content"
  else check_original path existing;
  let expected = if existing || installed then [ "export.json" ] else [] in
  require
    (List.equal String.equal (Eio.Path.read_dir parent) expected)
    "atomic installer leaked a staging file"
;;

let test env environment name fault existing =
  let name = name ^ if existing then "-existing" else "-absent" in
  let parent =
    Filename.concat (Temporary_environment.roots environment).temporary name
    |> Temporary_environment.path environment
  in
  Eio.Path.mkdir ~perm:0o700 parent;
  let path = Eio.Path.(parent / "export.json") in
  if existing then Eio.Path.save ~create:(`Exclusive 0o600) path "previous-export";
  Eio.Switch.run (fun sw -> download ~sw env parent path existing fault);
  assert_target fault parent path existing
;;

let run env environment =
  List.iter (("valid-control", Valid) :: faults) ~f:(fun (name, fault) ->
    List.iter [ true; false ] ~f:(fun existing ->
      test env environment name fault existing))
;;

let cancellation_target environment name existing =
  let name = name ^ if existing then "-existing" else "-absent" in
  let parent =
    Filename.concat (Temporary_environment.roots environment).temporary name
    |> Temporary_environment.path environment
  in
  Eio.Path.mkdir ~perm:0o700 parent;
  let path = Eio.Path.(parent / "export.json") in
  if existing then Eio.Path.save ~create:(`Exclusive 0o600) path "previous-export";
  parent, path
;;

let assert_cancelled expected ~path ~download =
  match Agent_client.Blob_download.install_atomic ~path ~download with
  | Ok () -> failwith "cancelled atomic installer returned success"
  | Error _ -> failwith "atomic installer swallowed Eio cancellation as Error"
  | exception Eio.Cancel.Cancelled actual ->
    require (phys_equal actual expected) "atomic installer changed the cancellation cause"
;;

let test_callback_cancellation environment existing =
  let parent, path = cancellation_target environment "callback-cancel" existing in
  let cause = Failure "cancel atomic export callback" in
  let download output =
    Eio.Flow.copy_string "first-" output;
    partial_assertion parent path existing ();
    raise (Eio.Cancel.Cancelled cause)
  in
  assert_cancelled cause ~path ~download;
  assert_target Interrupted parent path existing
;;

let test_callback_failure environment existing =
  let parent, path = cancellation_target environment "callback-error" existing in
  let download output =
    Eio.Flow.copy_string "first-" output;
    partial_assertion parent path existing ();
    failwith "ordinary atomic export IO failure"
  in
  let result = Agent_client.Blob_download.install_atomic ~path ~download in
  (match result with
   | Ok () -> failwith "ordinary callback exception was not converted to Error"
   | Error error ->
     require
       (String.is_substring
          (Error.to_string_hum error)
          ~substring:"ordinary atomic export IO failure")
       "callback failure regression failed for an unrelated reason");
  assert_target Interrupted parent path existing
;;

let first_http_chunk client blob =
  let command =
    Agent_protocol.Command.Blob_read
      { session_id = Agent_protocol.Id.Session.create ()
      ; attachment_id = Agent_protocol.Id.Attachment.create ()
      ; blob_id = blob.Agent_protocol.Blob.Metadata.id
      ; offset = 0L
      ; max_bytes = 6
      }
  in
  match (Http_driver.request client command |> protocol_ok).result with
  | Blob_read chunk ->
    let data =
      Base64.decode chunk.data_base64
      |> Result.map_error ~f:(fun (`Msg message) -> message)
      |> string_ok
    in
    require
      (String.equal data "first-" && (not chunk.eof) && Int64.equal chunk.next_offset 6L)
      "cancellation fixture did not receive a valid nonterminal HTTP chunk";
    data
  | _ -> failwith "expected initial HTTP blob chunk"
;;

let cancel_partial client blob parent path existing =
  let context, context_resolver = Eio.Promise.create () in
  let staged, staged_resolver = Eio.Promise.create () in
  let never, _never_resolver = Eio.Promise.create () in
  let cause = Failure "cancel partially downloaded HTTP export" in
  let download output =
    Eio.Flow.copy_string (first_http_chunk client blob) output;
    Eio.Promise.resolve staged_resolver ();
    Eio.Promise.await never
  in
  Eio.Fiber.both
    (fun () ->
       Eio.Cancel.sub (fun cancellation ->
         Eio.Promise.resolve context_resolver cancellation;
         assert_cancelled cause ~path ~download))
    (fun () ->
       let cancellation = Eio.Promise.await context in
       Eio.Promise.await staged;
       partial_assertion parent path existing ();
       Eio.Cancel.cancel cancellation cause)
;;

let test_http_cancellation env environment existing =
  let parent, path = cancellation_target environment "http-cancel" existing in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    Eio.Switch.run (fun sw ->
      let socket, port = listen ~sw env in
      let blob = blob () in
      let requests = ref 0 in
      serve ~sw socket Valid blob (fun () -> ()) requests;
      let client = Http_driver.create ~sw ~env ~port ~token:None |> string_ok in
      Exn.protect
        ~f:(fun () -> cancel_partial client blob parent path existing)
        ~finally:(fun () -> Http_driver.shutdown client);
      require
        (!requests = 1)
        "cancelled installer did not stop after its first HTTP chunk"));
  assert_target Interrupted parent path existing
;;

let run_cancellation env environment =
  List.iter [ true; false ] ~f:(fun existing ->
    test_callback_cancellation environment existing;
    test_callback_failure environment existing;
    test_http_cancellation env environment existing)
;;
