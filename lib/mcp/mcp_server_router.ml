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
      "tools":     { "listChanged": true },
      "prompts":   { "listChanged": true },
      "resources": { "listChanged": true },
      "roots":     { "listChanged": false },
      "logging":   {}
    }|}
;;

let server_info_json : Jsonaf.t =
  Jsonaf.of_string "{\"name\":\"ocamlochat-mcp-server\",\"version\":\"dev\"}"
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
  let log_success data = Mcp_server_core.log core ~level:`Info ~logger:"tool" data in
  if Mcp_server_core.is_cancelled core ~id
  then (
    let result : JT.Tool_result.t =
      { content = [ JT.Tool_result.Text "Request cancelled" ]; is_error = true }
    in
    let body = JT.Tool_result.jsonaf_of_t result in
    Ok (JR.ok ~id body |> JR.jsonaf_of_response))
  else (
    match handler args_json with
    | Ok json_result ->
      let () =
        log_success (`Object [ "event", `String "tool_success"; "tool", `String name ])
      in
      let result : JT.Tool_result.t =
        { content = [ JT.Tool_result.Json json_result ]; is_error = false }
      in
      let body = JT.Tool_result.jsonaf_of_t result in
      Ok (JR.ok ~id body |> JR.jsonaf_of_response)
    | Error msg ->
      let () =
        Mcp_server_core.log
          core
          ~level:`Error
          ~logger:"tool"
          (`Object
              [ "event", `String "tool_error"
              ; "tool", `String name
              ; "message", `String msg
              ])
      in
      let result : JT.Tool_result.t =
        { content = [ JT.Tool_result.Text msg ]; is_error = true }
      in
      let body = JT.Tool_result.jsonaf_of_t result in
      Ok (JR.ok ~id body |> JR.jsonaf_of_response))
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

(*------------------------------------------------------------------*)
(* Roots                                                            *)

let parse_additional_roots () : string list =
  match Sys.getenv "MCP_ADDITIONAL_ROOTS" with
  | None -> []
  | Some v -> String.split v ~on:':' |> List.filter ~f:(fun s -> String.length s > 0)
;;

let roots_list_response ~env ~id : Jsonaf.t =
  let cwd_root : Jsonaf.t =
    let cwd = Eio.Stdenv.cwd env in
    (* Convert the current working directory to a file URI and name *)
    (* Note: Eio.Path.native_exn is used to ensure we get a native path string *)
    let cwd_string = Eio.Path.native_exn @@ cwd in
    let uri = "file://" ^ cwd_string in
    let name = Filename.basename cwd_string in
    `Object [ "uri", `String uri; "name", `String name ]
  in
  let extra_roots_json =
    parse_additional_roots ()
    |> List.map ~f:(fun path ->
      let uri =
        if String.is_prefix path ~prefix:"file://" then path else "file://" ^ path
      in
      let name = Filename.basename path in
      `Object [ "uri", `String uri; "name", `String name ])
  in
  let all_roots = cwd_root :: extra_roots_json in
  let body = `Object [ "roots", `Array all_roots ] in
  JR.ok ~id body |> JR.jsonaf_of_response
;;

(*------------------------------------------------------------------*)
(* Resources – MVP placeholders                                     *)

let resources_list_response ~env ~id =
  (* Use Eio's non-blocking filesystem helpers to enumerate the current
     working directory.  We keep the logic non-recursive and only include
     regular files so the response stays small. *)
  let cwd = env#cwd in
  let resources =
    let dir_listing =
      Or_error.try_with (fun () -> Eio.Path.read_dir cwd)
      |> Result.ok
      |> Option.value ~default:[]
    in
    dir_listing
    |> List.filter_map ~f:(fun fname ->
      let file_path = Eio.Path.(cwd / fname) in
      match Or_error.try_with (fun () -> Eio.Path.stat ~follow:true file_path) with
      | Error _ -> None
      | Ok stats ->
        (match stats.kind with
         | `Regular_file ->
           let size = Optint.Int63.to_int stats.size in
           let uri = "file://" ^ fname in
           let mime_type = Mime.guess_mime_type fname in
           let res : JT.Resource.t =
             { uri; name = fname; description = None; mime_type; size = Some size }
           in
           Some res
         | _ -> None))
  in
  let body =
    let resources_json = `Array (List.map resources ~f:JT.Resource.jsonaf_of_t) in
    `Object [ "resources", resources_json ]
  in
  JR.ok ~id body |> JR.jsonaf_of_response
;;

let resources_read_response ~env ~id ~_params =
  match _params with
  | Some (`Object kvs) ->
    (match List.Assoc.find kvs ~equal:String.equal "uri" with
     | Some (`String uri) when String.is_prefix uri ~prefix:"file://" ->
       let path_str = String.chop_prefix_exn uri ~prefix:"file://" in
       let file_path = Eio.Path.(env#cwd / path_str) in
       let open Core in
       (match
          Or_error.try_with (fun () ->
            Eio.Switch.run (fun sw ->
              let flow = Eio.Path.open_in ~sw file_path in
              let s = Eio.Buf_read.(parse_exn take_all) flow ~max_size:1_048_576 in
              if String.length s > 1_000_000 then `Too_big else `Ok s))
        with
        | Error _ ->
          JR.error ~id ~code:(-32002) ~message:"Resource not found" ()
          |> JR.jsonaf_of_response
        | Ok `Too_big ->
          JR.error ~id ~code:(-32002) ~message:"Resource too large (>1MB)" ()
          |> JR.jsonaf_of_response
        | Ok (`Ok contents) ->
          let mime_type, payload_field, is_text =
            let mime_guess = Mime.guess_mime_type path_str in
            let mime = Option.value mime_guess ~default:"application/octet-stream" in
            if Mime.is_text_mime mime
            then mime, `String contents, true
            else (
              let b64 = Base64.encode_exn contents in
              mime, `String b64, false)
          in
          let fields =
            [ "uri", `String uri; "mimeType", `String mime_type ]
            @ if is_text then [ "text", payload_field ] else [ "blob", payload_field ]
          in
          let entry_json = `Object fields in
          let body = `Object [ "contents", `Array [ entry_json ] ] in
          JR.ok ~id body |> JR.jsonaf_of_response)
     | _ ->
       JR.error ~id ~code:(-32602) ~message:"Invalid params" () |> JR.jsonaf_of_response)
  | _ -> JR.error ~id ~code:(-32602) ~message:"Invalid params" () |> JR.jsonaf_of_response
;;

(* Handle a single request/notification, returning 0 or 1 response *)
let handle_single ~core ~env json : Jsonaf.t list =
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
      | "resources/list" -> [ resources_list_response ~env ~id ]
      | "resources/read" -> [ resources_read_response ~env ~id ~_params:req.params ]
      | "roots/list" -> [ roots_list_response ~env ~id ]
      | _ ->
        [ JR.error ~id ~code:(-32601) ~message:"Method not found" ()
          |> JR.jsonaf_of_response
        ]
    in
    responses
  | Error _ ->
    (* Could be notification or invalid *)
    (match Or_error.try_with (fun () -> JR.notification_of_jsonaf json) with
     | Ok notif ->
       (match notif.method_ with
        | "notifications/cancelled" ->
          let () =
            match notif.params with
            | Some (`Object kvs) ->
              (match List.Assoc.find kvs ~equal:String.equal "requestId" with
               | Some id_json ->
                 let open Mcp_types.Jsonrpc.Id in
                 let id_opt : t option =
                   match id_json with
                   | `String s -> Some (String s)
                   | `Number num_str ->
                     (match Int.of_string_opt num_str with
                      | Some i -> Some (Int i)
                      | None -> None)
                   | _ -> None
                 in
                 Option.iter id_opt ~f:(fun id -> Mcp_server_core.cancel_request core ~id)
               | _ -> ())
            | _ -> ()
          in
          []
        | _ -> [])
     | Error _ ->
       (* Malformed JSON – emit parse error *)
       let id = JR.Id.of_int 0 in
       [ JR.error ~id ~code:(-32700) ~message:"Parse error" () |> JR.jsonaf_of_response ])
;;

(*------------------------------------------------------------------*)

let handle ~core ~env (json : Jsonaf.t) : Jsonaf.t list =
  match json with
  | `Array arr -> List.concat_map arr ~f:(fun j -> handle_single ~env ~core j)
  | _ -> handle_single ~core ~env json
;;

(*------------------------------------------------------------------*)
