open Core

let assert_error promise =
  match Eio.Promise.peek promise with
  | Some (Error _) -> ()
  | Some (Ok _) -> failwith "teardown unexpectedly succeeded"
  | None -> failwith "orphan promise during protected teardown"
;;

let protected_teardown ~close_first (_env : Eio_unix.Stdenv.base) =
  let observed = ref false in
  (try
     Eio.Switch.run (fun sw ->
       let client =
         Mcp_client.For_testing.create
           ~sw
           ~send:(fun _ -> ())
           ~recv:(fun () -> Eio.Fiber.await_cancel ())
           ~close:(fun () -> ())
       in
       let original = Mcp_client.list_tools_async client in
       if close_first then Mcp_client.close client;
       Eio.Switch.fail sw (Failure "test shutdown");
       Eio.Cancel.protect (fun () ->
         assert_error (Mcp_client.list_tools_async client);
         assert_error
           (Mcp_client.call_tool_async client ~name:"test" ~arguments:(`Object []));
         assert_error original;
         Fixture.check
           (Mcp_client.For_testing.pending_count client = 0)
           "teardown leaked requests";
         observed := true))
   with
   | Failure message when String.equal message "test shutdown" -> ());
  Fixture.check !observed "protected teardown assertions did not run"
;;

let cases =
  [ "mcp.closed-client-protected-teardown", protected_teardown ~close_first:true
  ; "mcp.cancelled-switch-protected-teardown", protected_teardown ~close_first:false
  ]
;;
