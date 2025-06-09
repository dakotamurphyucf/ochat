open Core
module JT = Mcp_types.Jsonrpc
module Tool = Mcp_types.Tool
module Tools_list_result = Mcp_types.Tools_list_result
module Tool_result = Mcp_types.Tool_result

(* --------------------------------------------------------------------- *)
(* Minimal HTTP stub server that implements the subset of MCP methods     *)
(* required by the client test-suite:                                    *)
(*   initialize, tools/list, tools/call (echo|reverse|add)               *)
(* --------------------------------------------------------------------- *)
exception Completed of string

let tool_schema : Jsonaf.t =
  `Object
    [ "type", `String "object"
    ; "properties", `Object [ "value", `Object [ "type", `String "string" ] ]
    ; "required", `Array [ `String "value" ]
    ]
;;

let echo_tool : Tool.t =
  { name = "echo"
  ; description = Some "Return the input value unchanged"
  ; input_schema = tool_schema
  }
;;

let reverse_tool : Tool.t =
  { name = "reverse"
  ; description = Some "Return the reversed string"
  ; input_schema = tool_schema
  }
;;

let add_schema : Jsonaf.t =
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          [ "a", `Object [ "type", `String "number" ]
          ; "b", `Object [ "type", `String "number" ]
          ] )
    ; "required", `Array [ `String "a"; `String "b" ]
    ]
;;

let add_tool : Tool.t = { name = "add"; description = None; input_schema = add_schema }

let tools_list_json () =
  Tools_list_result.(
    jsonaf_of_t { tools = [ echo_tool; reverse_tool; add_tool ]; next_cursor = None })
;;

let handle_request (req_json : Jsonaf.t) : Jsonaf.t option =
  match JT.request_of_jsonaf req_json with
  | exception _ -> None
  | { JT.id; method_; params; _ } ->
    let response_ok result = JT.ok ~id result |> JT.jsonaf_of_response in
    let make_error code msg =
      JT.error ~id ~code ~message:msg () |> JT.jsonaf_of_response
    in
    (match method_ with
     | "initialize" ->
       let result =
         `Object
           [ "protocolVersion", `String "2025-03-26"
           ; "capabilities", `Object [ "tools", `Object [ "listChanged", `False ] ]
           ; ( "serverInfo"
             , `Object [ "name", `String "stub_http"; "version", `String "0.1" ] )
           ]
       in
       Some (response_ok result)
     | "tools/list" -> Some (response_ok (tools_list_json ()))
     | "tools/call" ->
       (match params with
        | Some (`Object kvs) ->
          let name =
            List.Assoc.find_exn kvs ~equal:String.equal "name" |> Jsonaf.string_exn
          in
          let arguments = List.Assoc.find kvs ~equal:String.equal "arguments" in
          let respond_text s =
            let result : Tool_result.t = { content = [ Text s ]; is_error = false } in
            Some (response_ok (Tool_result.jsonaf_of_t result))
          in
          (match name with
           | "echo" ->
             let value =
               match arguments with
               | Some (`Object a_kvs) ->
                 (match List.Assoc.find a_kvs ~equal:String.equal "value" with
                  | Some (`String s) -> s
                  | _ -> "")
               | _ -> ""
             in
             respond_text value
           | "reverse" ->
             let value =
               match arguments with
               | Some (`Object a_kvs) ->
                 (match List.Assoc.find a_kvs ~equal:String.equal "value" with
                  | Some (`String s) -> String.rev s
                  | _ -> "")
               | _ -> ""
             in
             respond_text value
           | "add" ->
             let sum =
               match arguments with
               | Some (`Object kvs) ->
                 let get_num name =
                   match List.Assoc.find kvs ~equal:String.equal name with
                   | Some (`Number n) -> Float.of_string n
                   | Some (`String s) -> Float.of_string s
                   | _ -> 0.
                 in
                 get_num "a" +. get_num "b"
               | _ -> 0.
             in
             respond_text (Float.to_string sum)
           | _ -> Some (make_error (-32000) "unknown tool"))
        | _ -> Some (make_error (-32602) "invalid params"))
     | _ -> Some (make_error (-32601) "method not found"))
;;

(* --------------------------------------------------------------------- *)
(* Start the HTTP server (Piaf) inside the test switch                   *)
(* --------------------------------------------------------------------- *)

let port = 8124

let start_http_server ~sw env =
  let open Piaf in
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let config = Server.Config.create address in
  let handler ({ Server.Handler.request; _ } : _ Server.Handler.ctx) : Response.t =
    let body_str =
      match Body.to_string (Request.body request) with
      | Ok s -> s
      | Error _ -> "{}"
    in
    let response_json =
      match Result.try_with (fun () -> Jsonaf.of_string body_str) with
      | Ok j -> Option.value (handle_request j) ~default:(`String "null")
      | Error _ -> `String "null"
    in
    let headers = Headers.of_list [ "content-type", "application/json" ] in
    Response.of_string
      ~headers
      ~body:(Jsonaf_ext.to_string response_json)
      (Piaf.Status.of_code 200)
  in
  let server = Server.create ~config handler in
  ignore (Server.Command.start ~sw env server : Server.Command.t)
;;

(* --------------------------------------------------------------------- *)
(* Helper to create a connected client (HTTP transport)                  *)
(* --------------------------------------------------------------------- *)

let with_client_http f =
  try
    Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
        (* start server inside switch *)
        Eio.Fiber.fork ~sw (fun () ->
          try start_http_server ~sw env with
          | _ -> ());
        (* connect client to the server *)
        let uri = sprintf "http://127.0.0.1:%d/mcp" port in
        let client = Mcp_client.connect ~sw ~env ~uri in
        Fun.protect
          ~finally:(fun () ->
            Mcp_client.close client;
            Eio.Switch.fail sw (Completed "Test completed"))
          (fun () -> f client)))
  with
  | Completed _ -> ()
;;

(* --------------------------------------------------------------------- *)
(* Test cases – mirror those from the stdio variant                      *)
(* --------------------------------------------------------------------- *)

let%expect_test "list_tools over HTTP" =
  with_client_http (fun client ->
    let tools = Result.ok_or_failwith (Mcp_client.list_tools client) in
    print_s [%sexp (List.map tools ~f:(fun t -> t.name) : string list)]);
  [%expect "(echo reverse add)"]
;;

let%expect_test "call_tool echo over HTTP" =
  with_client_http (fun client ->
    let args = `Object [ "value", `String "hi" ] in
    let res =
      Result.ok_or_failwith (Mcp_client.call_tool client ~name:"echo" ~arguments:args)
    in
    print_s [%sexp (res : Mcp_types.Tool_result.t)]);
  [%expect "((content ((Text hi))) (is_error false))"]
;;

let%expect_test "unknown tool error over HTTP" =
  with_client_http (fun client ->
    let args = `Object [] in
    match Mcp_client.call_tool client ~name:"does_not_exist" ~arguments:args with
    | Ok _ -> print_endline "unexpected ok"
    | Error e -> print_endline ("error:" ^ e));
  [%expect {| error:RPC error -32000 – unknown tool |}]
;;
