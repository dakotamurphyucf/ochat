open Core

(* Direct access to the stdio transport implementation under test. *)
module Stdio = Mcp_transport_stdio

let%expect_test "stdio transport round-trip via `cat`" =
  (* We run the test inside an Eio main loop to obtain an environment
     that includes a [process_mgr].  *)
  Eio_main.run
  @@ fun env ->
  (* Create a switch so that resources get cleaned up automatically even
     if the test raises. *)
  Eio.Switch.run
  @@ fun sw ->
  let transport = Stdio.connect ~sw ~env "stdio:cat" in
  let msg = Jsonaf.of_string "{\"foo\": 42}" in
  Stdio.send transport msg;
  let echo = Stdio.recv transport in
  (* Print the echoed JSON so that expect-test can verify it. *)
  print_s [%sexp (Jsonaf.to_string echo : string)];
  (* Close the transport. *)
  (* This should not raise, even if the child process has already exited. *)
  Stdio.close transport;
  [%expect {| "{\"foo\":42}" |}]
;;

let%expect_test "send/recv after explicit close raises" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let t = Mcp_transport_stdio.connect ~sw ~env "stdio:cat" in
  Mcp_transport_stdio.close t;
  (* After close, the transport should report closed. *)
  print_s [%sexp "closed =", (Mcp_transport_stdio.is_closed t : bool)];
  (match Result.try_with (fun () -> Mcp_transport_stdio.send t (`String "hi")) with
   | Error Mcp_transport_stdio.Connection_closed -> print_endline "send_closed"
   | _ -> print_endline "send_ok");
  (match Result.try_with (fun () -> Mcp_transport_stdio.recv t) with
   | Error Mcp_transport_stdio.Connection_closed -> print_endline "recv_closed"
   | _ -> print_endline "recv_ok");
  [%expect
    {|
    ("closed =" true)
    send_closed
    recv_closed
    |}]
;;

let%expect_test "EOF from child → Connection_closed" =
  (* Child prints a single JSON line and exits. *)
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let t = Mcp_transport_stdio.connect ~sw ~env "stdio:echo {\"bye\":true}" in
  let first = Mcp_transport_stdio.recv t in
  print_s [%sexp (first : Jsonaf.t)];
  (* Second recv must raise Connection_closed *)
  (match Result.try_with (fun () -> Mcp_transport_stdio.recv t) with
   | Error Mcp_transport_stdio.Connection_closed ->
     print_s [%sexp "closed =", (Mcp_transport_stdio.is_closed t : bool)]
   | _ -> print_endline "unexpected");
  [%expect
    {|
    (Object ((bye True)))
    ("closed =" true)
    |}]
;;

let%expect_test "connection stays open while child lives" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let t = Stdio.connect ~sw ~env "stdio:cat" in
  (* Send first message *)
  Stdio.send t (Jsonaf.of_string "{\"n\":1}");
  let r1 = Stdio.recv t in
  print_s [%sexp (r1 : Jsonaf.t)];
  (* print_endline (Jsonaf.to_string r1); *)
  (* Transport should report still open. *)
  (* Print the closed state *)
  print_s [%sexp "closed =", (Stdio.is_closed t : bool)];
  (* Send another message *)
  Stdio.send t (Jsonaf.of_string "{\"n\":2}");
  let r2 = Stdio.recv t in
  print_s [%sexp (r2 : Jsonaf.t)];
  print_s [%sexp "closed =", (Stdio.is_closed t : bool)];
  (* Close the transport *)
  Stdio.close t;
  [%expect
    {|
    (Object ((n (Number 1))))
    ("closed =" false)
    (Object ((n (Number 2))))
    ("closed =" false)
    |}]
;;
