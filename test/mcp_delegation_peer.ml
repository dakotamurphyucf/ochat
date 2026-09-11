open Core
module J = Mcp_types.Jsonrpc
module T = Mcp_types.Tool

let () =
  let root = (Sys.get_argv ()).(1) in
  let state = Filename.concat root "catalog" in
  let audit = Filename.concat root "calls" in
  let write json = printf "%s\n%!" (Jsonaf.to_string json) in
  let schema = `Object [ "type", `String "object" ] in
  let control : T.t =
    { name = "change_catalog"; description = None; input_schema = schema }
  in
  let tools () =
    match In_channel.read_all state with
    | "removed" -> [ control ]
    | mode ->
      let input_schema =
        match mode with
        | "schema" ->
          `Object [ "type", `String "object"; "required", `Array [ `String "new_field" ] ]
        | _ -> schema
      in
      { T.name = "echo"; description = Some "Pinned echo"; input_schema } :: [ control ]
  in
  In_channel.iter_lines In_channel.stdin ~f:(fun line ->
    let json = Jsonaf.of_string line in
    match J.request_of_jsonaf json with
    | exception _ -> ()
    | request ->
      let respond result = J.ok ~id:request.id result |> J.jsonaf_of_response |> write in
      (match request.method_ with
       | "initialize" -> respond (`Object [])
       | "tools/list" ->
         Mcp_types.Tools_list_result.{ tools = tools (); next_cursor = None }
         |> Mcp_types.Tools_list_result.jsonaf_of_t
         |> respond
       | "tools/call" ->
         let params = Jsonaf.member_exn "params" json in
         let name = Jsonaf.member_exn "name" params |> Jsonaf.string_exn in
         Out_channel.with_file audit ~append:true ~f:(fun channel ->
           Out_channel.output_string channel (name ^ "\n"));
         (match name with
          | "change_catalog" ->
            let mode =
              Jsonaf.member_exn "arguments" params
              |> Jsonaf.member_exn "mode"
              |> Jsonaf.string_exn
            in
            Out_channel.write_all state ~data:mode;
            J.notify ~method_:"notifications/tools/list_changed" ()
            |> J.jsonaf_of_notification
            |> write
          | _ -> ());
         Mcp_types.Tool_result.{ content = [ Text "peer-result" ]; is_error = false }
         |> Mcp_types.Tool_result.jsonaf_of_t
         |> respond
       | _ ->
         J.error ~id:request.id ~code:(-32601) ~message:"unknown method" ()
         |> J.jsonaf_of_response
         |> write))
;;
