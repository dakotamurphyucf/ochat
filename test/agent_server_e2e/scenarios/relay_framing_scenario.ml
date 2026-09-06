open Core
module F = Support.Background_fixture

let payload = "event: response.completed\ndata: {}\n\n"

let response length consumed =
  let sent = ref false in
  let stream =
    Piaf.Stream.from ~f:(fun () ->
      if !sent
      then (
        consumed := true;
        None)
      else (
        sent := true;
        Some payload))
  in
  let body = Piaf.Body.of_string_stream ~length stream in
  Piaf.Response.create
    ~body
    ~headers:(Piaf.Headers.of_list [ "content-type", "text/event-stream" ])
    `OK
;;

let fixture ~sw env length consumed ~normalize =
  let port = F.reserve_port env in
  let handler ({ Piaf.Server.request; _ } : _ Piaf.Server.ctx) =
    ignore (Piaf.Body.drain request.body : (unit, Piaf.Error.t) result);
    let response = response length consumed in
    if normalize then Support.Live_openai_proxy.response_for_http1 response else response
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun server_sw ->
      let config = Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
      let server = Piaf.Server.create ~config handler in
      ignore (Piaf.Server.Command.start ~sw:server_sw env server : Piaf.Server.Command.t));
    `Stop_daemon);
  port
;;

let read env port =
  Eio.Time.with_timeout (Eio.Stdenv.clock env) 1. (fun () ->
    Eio.Switch.run (fun sw ->
      Ok
        (Io.Net.post
           Io.Net.Default
           ~net:(Eio.Stdenv.net env)
           ~host:(sprintf "http://127.0.0.1:%d" port)
           ~path:"/v1/responses"
           ~headers:(Http.Header.init ())
           ~sw
           "{}")))
;;

let check env length ~normalize =
  Eio.Switch.run (fun sw ->
    let consumed = ref false in
    let port = fixture ~sw env length consumed ~normalize in
    let result = read env port in
    F.require !consumed "fixture did not finish producing its complete body";
    match normalize, length, result with
    | false, `Unknown, Error `Timeout ->
      "unknown length: full body sent; reader timed out"
    | false, `Chunked, Ok actual when String.equal actual payload ->
      "chunked: identical body completed without closing connection"
    | true, `Unknown, Ok actual when String.equal actual payload ->
      "relay normalizer: unknown-length body completed with chunked framing"
    | _ -> failwith "unexpected relay framing diagnostic result")
;;

let run env =
  List.iter [ `Unknown; `Chunked ] ~f:(fun length ->
    Eio.Flow.copy_string
      (check env length ~normalize:false ^ "\n")
      (Eio.Stdenv.stdout env));
  Eio.Flow.copy_string (check env `Unknown ~normalize:true ^ "\n") (Eio.Stdenv.stdout env)
;;
