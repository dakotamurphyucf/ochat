open Core
module J = Mcp_types.Jsonrpc
module T = Mcp_types.Tool

let run env =
  let root = (Sys.get_argv ()).(1) in
  let directory = Eio.Path.(Eio.Stdenv.fs env / root) in
  let state = Eio.Path.(directory / "catalog") in
  let audit = Eio.Path.(directory / "calls") in
  let append path text =
    Eio.Path.save ~append:true ~create:(`If_missing 0o600) path text
  in
  let write json =
    Eio.Flow.copy_string (Jsonaf.to_string json ^ "\n") (Eio.Stdenv.stdout env)
  in
  let catalog_gate () =
    if String.equal (Eio.Path.load state) "gate"
    then (
      (* One actual list request owns this gate; its schema stays unchanged. *)
      Eio.Path.save ~create:(`Or_truncate 0o600) state "original";
      Eio.Switch.run (fun sw ->
        let flow =
          Eio.Net.connect
            ~sw
            (Eio.Stdenv.net env)
            (`Unix (Filename.concat root "catalog-gate.sock"))
        in
        Eio.Flow.copy_string "entered\n" flow;
        let input = Eio.Buf_read.of_flow flow ~max_size:32 in
        match Eio.Buf_read.line input with
        | "release" -> ()
        | _ -> failwith "invalid catalog release"
        | exception End_of_file -> ()))
  in
  let schema = `Object [ "type", `String "object" ] in
  let control : T.t =
    { name = "change_catalog"; description = None; input_schema = schema }
  in
  let tools () =
    match Eio.Path.load state with
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
  let input = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~max_size:1_000_000 in
  let rec loop () =
    match Eio.Buf_read.line input with
    | exception End_of_file -> ()
    | line ->
      (let json = Jsonaf.of_string line in
       match J.request_of_jsonaf json with
       | exception _ -> ()
       | request ->
         let respond result =
           J.ok ~id:request.id result |> J.jsonaf_of_response |> write
         in
         (match request.method_ with
          | "initialize" ->
            append Eio.Path.(directory / "connections") "connected\n";
            respond (`Object [])
          | "tools/list" ->
            catalog_gate ();
            Mcp_types.Tools_list_result.{ tools = tools (); next_cursor = None }
            |> Mcp_types.Tools_list_result.jsonaf_of_t
            |> respond
          | "tools/call" ->
            let params = Jsonaf.member_exn "params" json in
            let name = Jsonaf.member_exn "name" params |> Jsonaf.string_exn in
            append audit (name ^ "\n");
            (match name with
             | "change_catalog" ->
               let mode =
                 Jsonaf.member_exn "arguments" params
                 |> Jsonaf.member_exn "mode"
                 |> Jsonaf.string_exn
               in
               Eio.Path.save ~create:(`Or_truncate 0o600) state mode;
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
            |> write));
      loop ()
  in
  loop ()
;;

let () = Eio_main.run run
