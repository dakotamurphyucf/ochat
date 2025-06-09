open! Core
module JT = Mcp_types
module JR = JT.Jsonrpc

(*------------------------------------------------------------------*)

let protocol_version = "2025-03-26"

(*
  Capability negotiation payload.  We expose three server-side features:

  • tools   – with listChanged notifications
  • prompts – with listChanged notifications
  • logging – empty object (no sub-capabilities in the current spec)

  The existing [Mcp_types.Capability.t] record does not yet model the
  *logging* capability, so we assemble the JSON manually instead of going
  through the generated converter.  This keeps the public type untouched
  while staying 100 % spec-compliant.
*)

let capability_json : Jsonaf.t =
  Jsonaf.of_string
    {|{
      "tools":    { "listChanged": true },
      "prompts":  { "listChanged": true },
      "logging":  {}
    }|}
;;

let server_info_json : Jsonaf.t =
  Jsonaf.of_string "{\"name\":\"ocamlgpt-mcp-server\",\"version\":\"dev\"}"
;;

(*------------------------------------------------------------------*)

let initialise_response ~id =
  let body =
    Jsonaf.of_string
      (Printf.sprintf
         "{\"protocolVersion\":\"%s\",\"capabilities\":%s,\"serverInfo\":%s}"
         protocol_version
         (Jsonaf.to_string capability_json)
         (Jsonaf.to_string server_info_json))
  in
  JR.ok ~id body |> JR.jsonaf_of_response
;;

(*------------------------------------------------------------------*)

let tools_list_response ~core ~id =
  let tools = Mcp_server_core.list_tools core in
  let payload : JT.Tools_list_result.t = { tools; next_cursor = None } in
  let body = JT.Tools_list_result.jsonaf_of_t payload in
  JR.ok ~id body |> JR.jsonaf_of_response
;;

let tool_call_response ~core ~id ~params =
  let open Or_error.Let_syntax in
  let%bind () =
    match params with
    | Some (`Object _) -> Ok ()
    | Some _ -> Or_error.error_string "tools/call params must be object"
    | None -> Or_error.error_string "Missing params"
  in
  let obj =
    match params with
    | Some (`Object kvs) -> kvs
    | _ -> []
  in
  let name_opt =
    match List.Assoc.find obj ~equal:String.equal "name" with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let args_json =
    match List.Assoc.find obj ~equal:String.equal "arguments" with
    | Some j -> j
    | None -> `Null
  in
  let%bind name =
    match name_opt with
    | Some n -> Ok n
    | None -> Or_error.error_string "tools/call missing name"
  in
  let%bind handler, _spec =
    match Mcp_server_core.get_tool core name with
    | Some pair -> Ok pair
    | None -> Or_error.errorf "Unknown tool %s" name
  in
  match handler args_json with
  | Ok json_result ->
    let result : JT.Tool_result.t =
      { content = [ JT.Tool_result.Json json_result ]; is_error = false }
    in
    let body = JT.Tool_result.jsonaf_of_t result in
    Ok (JR.ok ~id body |> JR.jsonaf_of_response)
  | Error msg ->
    let result : JT.Tool_result.t =
      { content = [ JT.Tool_result.Text msg ]; is_error = true }
    in
    let body = JT.Tool_result.jsonaf_of_t result in
    Ok (JR.ok ~id body |> JR.jsonaf_of_response)
;;

let prompts_list_response ~core ~id =
  let prompts = Mcp_server_core.list_prompts core in
  let json_prompts =
    `Array
      (List.map prompts ~f:(fun (name, p) ->
         let fields =
           [ "name", `String name ]
           @
           match p.description with
           | None -> []
           | Some d -> [ "description", `String d ]
         in
         `Object fields))
  in
  let body = `Object [ "prompts", json_prompts ] in
  JR.ok ~id body |> JR.jsonaf_of_response
;;

let prompts_get_response ~core ~id ~params =
  match params with
  | Some (`Object obj) ->
    (match List.Assoc.find obj ~equal:String.equal "name" with
     | Some (`String name) ->
       (match Mcp_server_core.get_prompt core name with
        | None ->
          JR.error ~id ~code:(-32000) ~message:"Prompt not found" ()
          |> JR.jsonaf_of_response
        | Some p ->
          let body =
            `Object
              [ ( "description"
                , match p.description with
                  | None -> `Null
                  | Some d -> `String d )
              ; "messages", p.messages
              ]
          in
          JR.ok ~id body |> JR.jsonaf_of_response)
     | _ ->
       JR.error ~id ~code:(-32602) ~message:"Invalid params" () |> JR.jsonaf_of_response)
  | _ -> JR.error ~id ~code:(-32602) ~message:"Invalid params" () |> JR.jsonaf_of_response
;;

let ping_response ~id = JR.ok ~id (`Object []) |> JR.jsonaf_of_response

(* Handle a single request/notification, returning 0 or 1 response *)
let handle_single ~core json : Jsonaf.t list =
  (* Try request first *)
  match Or_error.try_with (fun () -> JR.request_of_jsonaf json) with
  | Ok req ->
    let id = req.id in
    let responses =
      match req.method_ with
      | "initialize" -> [ initialise_response ~id ]
      | "tools/list" -> [ tools_list_response ~core ~id ]
      | "tools/call" ->
        (match tool_call_response ~core ~id ~params:req.params with
         | Ok resp -> [ resp ]
         | Error err_msg ->
           [ JR.error ~id ~code:(-32000) ~message:(Error.to_string_hum err_msg) ()
             |> JR.jsonaf_of_response
           ])
      | "prompts/list" -> [ prompts_list_response ~core ~id ]
      | "prompts/get" -> [ prompts_get_response ~core ~id ~params:req.params ]
      | "ping" -> [ ping_response ~id ]
      | _ ->
        [ JR.error ~id ~code:(-32601) ~message:"Method not found" ()
          |> JR.jsonaf_of_response
        ]
    in
    responses
  | Error _ ->
    (* Could be notification or invalid *)
    (match Or_error.try_with (fun () -> JR.notification_of_jsonaf json) with
     | Ok _notif -> [] (* we ignore notifications for now *)
     | Error _ ->
       (* Malformed JSON – emit parse error *)
       let id = JR.Id.of_int 0 in
       [ JR.error ~id ~code:(-32700) ~message:"Parse error" () |> JR.jsonaf_of_response ])
;;

(*------------------------------------------------------------------*)

let handle ~core (json : Jsonaf.t) : Jsonaf.t list =
  match json with
  | `Array arr -> List.concat_map arr ~f:(fun j -> handle_single ~core j)
  | _ -> handle_single ~core json
;;

(*------------------------------------------------------------------*)
