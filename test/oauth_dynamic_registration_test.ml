open Core

exception Done

(** Integration test verifying that the HTTP transport performs dynamic client
    registration when connecting to a fresh MCP server for which no OAuth2
    credentials are configured or cached.  The test runs the real
    [Mcp_server_http] implementation with [require_auth = true] so that the
    client must obtain a bearer token before any JSON-RPC request is accepted.

    The sequence validated here is:

    1. Environment is scrubbed so that no credentials are available and the
       XDG config directory points to a temporary location.
    2. The client issues an unauthenticated request → receives 401 → triggers
       discovery → dynamic registration → token exchange.
    3. After the handshake the client can list tools successfully, proving
       that the bearer token flowed.
    4. The dynamic credentials are persisted to
       [$XDG_CONFIG_HOME/ocamlochat/registered.json].  The test asserts the file
       exists and contains an entry for the server issuer URL.
*)

module JT = Mcp_types

let random_port () = 9100 + Random.int 400 (* keep out of the range used in other tests *)

let with_temp_config_dir f =
  let dir =
    let suffix = Printf.sprintf "ocamlochat-test-%06x" (Random.int 0xFFFFFF) in
    Filename.concat (Stdlib.Filename.get_temp_dir_name ()) suffix
  in
  Core_unix.mkdir_p dir;
  (* Point both XDG_CONFIG_HOME and HOME (fallback) to the temp dir so that the
     credential cache file lives in an isolated location. *)
  Core_unix.putenv ~key:"XDG_CONFIG_HOME" ~data:dir;
  Core_unix.putenv ~key:"HOME" ~data:dir;
  (* Disable any attempt to launch a browser during OAuth PKCE flows. *)
  (* Core_unix.putenv ~key:"OAUTH_NO_BROWSER" ~data:"1"; *)
  try
    let res = f dir in
    (* Cleanup: not strictly required inside the dune sandbox but keeps /tmp tidy. *)
    res
  with
  | ex ->
    (* best-effort removal *)
    (try Core_unix.chdir "/" with
     | _ -> ());
    (try Core_unix.system (sprintf "rm -rf %s" (Filename.quote dir)) |> ignore with
     | _ -> ());
    raise ex
;;

let make_echo_tool () : JT.Tool.t * Mcp_server_core.tool_handler =
  let schema_json =
    Jsonaf.of_string
      {|{"type":"object","properties":{"value":{"type":"string"}},"required":["value"]}|}
  in
  let spec : JT.Tool.t =
    { name = "echo"; description = Some "Echo back"; input_schema = schema_json }
  in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match Jsonaf.member "value" args with
    | Some (`String s) -> Ok (`String s)
    | _ -> Error "missing value"
  in
  spec, handler
;;

let%expect_test "dynamic client registration flow" =
  Random.init 42;
  let result =
    with_temp_config_dir (fun cfg_dir ->
      (* Sanity: the credential file should NOT exist yet. *)
      let cred_file = Filename.concat cfg_dir "ocamlochat/registered.json" in
      let file_exists p = Stdlib.Sys.file_exists p in
      assert (not (file_exists cred_file));
      let port = random_port () in
      Eio_main.run
      @@ fun env ->
      try
        Mirage_crypto_rng_unix.use_default ();
        Eio.Switch.run
        @@ fun sw ->
        (* Build server core with a single echo tool so that we can make a
           call after initialization. *)
        let core = Mcp_server_core.create () in
        let spec, handler = make_echo_tool () in
        Mcp_server_core.register_tool core spec handler;
        (* Launch the HTTP server with [require_auth = true] so the client is
           forced to perform OAuth2. *)
        Eio.Fiber.fork ~sw (fun () ->
          Mcp_server_http.run ~require_auth:true ~env ~core ~port);
        (* Client URI without any credentials or query parameters. *)
        let uri = sprintf "http://127.0.0.1:%d/mcp" port in
        (* Core_unix.putenv ~key:"MCP_CLIENT_ID" ~data:"dev-client";
        Core_unix.putenv ~key:"MCP_CLIENT_SECRET" ~data:"dev-secret"; *)
        let client = Mcp_client.connect ~sw ~env uri in
        (* After connect we should be able to list tools – proving that the
           bearer token was obtained and accepted by the server. *)
        let tools =
          match Mcp_client.list_tools client with
          | Ok ts -> ts
          | Error e -> failwithf "list_tools failed: %s" e ()
        in
        let names = List.map tools ~f:(fun t -> t.JT.Tool.name) in
        printf "tools=%s\n" (String.concat ~sep:"," names);
        (* Credential file should now exist and contain an entry for the issuer. *)
        let issuer = sprintf "http://127.0.0.1:%d" port in
        assert (file_exists cred_file);
        let contents = In_channel.read_all cred_file in
        (* Quick check: the issuer string appears in the file and a client_id field exists. *)
        if
          String.is_substring contents ~substring:issuer
          && String.is_substring contents ~substring:"client_id"
        then printf "credentials_persisted=yes\n"
        else printf "credentials_persisted=no\n";
        (* Cleanup *)
        Mcp_client.close client;
        (* Cancel switch so server terminates. *)
        Eio.Switch.fail sw Done
      with
      | Done -> ())
  in
  ignore result;
  [%expect
    {|
    tools=echo
    credentials_persisted=yes
  |}]
;;
