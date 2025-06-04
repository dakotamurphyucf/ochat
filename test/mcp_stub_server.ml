open Core
module JT = Mcp_types.Jsonrpc
module Tool = Mcp_types.Tool
module Tools_list_result = Mcp_types.Tools_list_result
module Tool_result = Mcp_types.Tool_result

let tool_schema : Jsonaf.t =
  `Object
    [ "type", `String "object"
    ; "properties", `Object [ "value", `Object [ "type", `String "string" ] ]
    ; "required", `Array [ `String "value" ]
    ]
;;

let echo_tool : Tool.t =
  { name = "echo"; description = Some "Return the input value unchanged"; input_schema = tool_schema }

let reverse_tool : Tool.t =
  { name = "reverse"; description = Some "Return the reversed string"; input_schema = tool_schema }

let add_schema : Jsonaf.t =
  `Object
    [ "type", `String "object"
    ; "properties"
      , `Object
          [ ( "a", `Object [ "type", `String "number" ] )
          ; ( "b", `Object [ "type", `String "number" ] )
          ]
    ; "required", `Array [ `String "a"; `String "b" ]
    ]

let add_tool : Tool.t = { name = "add"; description = None; input_schema = add_schema }

let tools_list_json () =
  Tools_list_result.(jsonaf_of_t { tools = [ echo_tool; reverse_tool; add_tool ]; next_cursor = None })
;;

let write_json oc (json : Jsonaf.t) =
  Out_channel.output_string oc (Jsonaf_ext.to_string json ^ "\n");
  Out_channel.flush oc
;;

let handle_request req_json : Jsonaf.t option =
  match JT.request_of_jsonaf req_json with
  | exception _ -> None
  | { JT.id; method_; params = _; _ } ->
    let response_ok result = JT.ok ~id result |> JT.jsonaf_of_response in
    (match method_ with
     | "initialize" ->
       let result =
         `Object
           [ "protocolVersion", `String "2025-03-26"
           ; "capabilities", `Object [ "tools", `Object [ "listChanged", `False ] ]
           ; "serverInfo", `Object [ "name", `String "stub"; "version", `String "0.1" ]
           ]
       in
       Some (response_ok result)
     | "tools/list" -> Some (response_ok (tools_list_json ()))
     | "tools/call" ->
       (match req_json |> Jsonaf.member_exn "params" with
        | `Object kvs ->
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
               | Some (`Object a_kvs) -> (
                   match List.Assoc.find a_kvs ~equal:String.equal "value" with
                   | Some (`String s) -> s
                   | _ -> "")
               | _ -> ""
             in
             respond_text value
           | "reverse" ->
             let value =
               match arguments with
               | Some (`Object a_kvs) -> (
                   match List.Assoc.find a_kvs ~equal:String.equal "value" with
                   | Some (`String s) -> String.rev s
                   | _ -> "")
               | _ -> ""
             in
             respond_text value
           | "add" ->
             let nums =
               match arguments with
               | Some (`Object kvs) ->
                 let get name =
                   match List.Assoc.find kvs ~equal:String.equal name with
                   | Some (`Number n) -> Float.of_string n
                   | Some (`String s) -> Float.of_string s
                   | _ -> 0.
                 in
                 get "a" +. get "b"
               | _ -> 0.
             in
             respond_text (string_of_float nums)
           | _ ->
             let err = JT.error ~id ~code:(-32000) ~message:"unknown tool" () in
             Some (JT.jsonaf_of_response err))
        | _ -> None)
     | _ ->
       let err = JT.error ~id ~code:(-32601) ~message:"method not found" () in
       Some (JT.jsonaf_of_response err))
;;

let rec main_loop ic oc =
  match In_channel.input_line ic with
  | None -> ()
  | Some line when String.is_empty line -> main_loop ic oc
  | Some line ->
    (match Result.try_with (fun () -> Jsonaf.of_string line) with
     | Error _ -> ()
     | Ok j ->
       (match handle_request j with
        | None -> ()
        | Some resp -> write_json oc resp));
    main_loop ic oc
;;

let () =
  (* Use stdio transport: read from stdin, write to stdout *)
  let ic = Stdio.stdin in
  let oc = Stdio.stdout in
  main_loop ic oc
;;
