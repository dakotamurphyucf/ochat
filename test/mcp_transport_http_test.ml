open Core

(* Test the HTTP transport implementation using a minimal in-process
   echo server built with Piaf.  The server simply returns the exact
   JSON payload it receives in the request body, which is enough to
   verify the round-trip behaviour of [Mcp_transport_http]. *)

module Http = Mcp_transport_http

exception Completed of string

let port = 8123

let start_echo_server env =
  Eio.Switch.run
  @@ fun sw ->
  let open Piaf in
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let config = Server.Config.create address in
  let handler ({ Server.Handler.request; _ } : _ Server.Handler.ctx) : Response.t =
    let body_string =
      match Body.to_string (Request.body request) with
      | Ok s -> s
      | Error _ -> "{}"
    in
    let headers = Headers.of_list [ "content-type", "application/json" ] in
    Response.of_string ~headers ~body:body_string (Piaf.Status.of_code 200)
  in
  let server = Server.create ~config handler in
  ignore (Server.Command.start ~sw env server : Server.Command.t)
;;

let%expect_test "HTTP transport round-trip" =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Start the echo server. *)
    Eio.Fiber.fork ~sw (fun () ->
      try start_echo_server env with
      | _ -> ());
    (* Connect to the echo server using the HTTP transport. *)
    let uri = Printf.sprintf "http://127.0.0.1:%d/mcp" port in
    let transport = Http.connect ~sw ~env uri in
    let messages =
      [ Jsonaf.of_string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}"
      ; Jsonaf.of_string "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
      ; Jsonaf.of_string
          "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"value\":\"hi\"}}}"
      ]
    in
    List.iter messages ~f:(fun msg ->
      Http.send transport msg;
      let resp = Http.recv transport in
      match Jsonaf.member "method" resp with
      | Some (`String m) -> print_endline m
      | _ -> print_endline "no_method");
    Http.close transport;
    Eio.Switch.fail sw (Completed "Test completed")
  with
  | Completed _ ->
    ();
    (* Expect the output to show the methods called. *)
    [%expect
      {|
      initialize
      tools/list
      tools/call
      |}]
;;
