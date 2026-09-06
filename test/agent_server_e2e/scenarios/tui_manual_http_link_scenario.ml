open Core
module Temp = Support.Temporary_environment
module Link = Support.Tui_manual_link
module Unix_link = Tui_manual_link_scenario
module F = Support.Tui_fixture

let say env text = Eio.Flow.copy_string (text ^ "\n") (Eio.Stdenv.stdout env)
let endpoint port = sprintf "http://127.0.0.1:%d" port

let launcher_argument_exn source flag =
  let rec find = function
    | name :: _space :: value :: _ when String.equal name flag -> value
    | _ :: rest -> find rest
    | [] -> failwith "expected argument missing from fixture launcher"
  in
  find (String.split source ~on:'\'')
;;

let upstream_port_exn source =
  let uri = launcher_argument_exn source "--connect" |> Uri.of_string in
  F.require
    (Poly.equal (Uri.scheme uri) (Some "http")
     && Poly.equal (Uri.host uri) (Some "127.0.0.1")
     && String.is_empty (Uri.path uri)
     && Option.is_none (Uri.userinfo uri))
    "manual HTTP relay requires a loopback fixture endpoint";
  Uri.port uri |> Option.value_exn
;;

let launcher temporary source upstream_port port =
  let pattern = "'--connect' '" ^ endpoint upstream_port ^ "'" in
  F.require (String.is_substring source ~substring:pattern) "unexpected HTTP launcher";
  let contents =
    String.substr_replace_first
      source
      ~pattern
      ~with_:("'--connect' '" ^ endpoint port ^ "'")
  in
  let path = Filename.concat (Temp.roots temporary).sockets "http-relay-attach.sh" in
  Eio.Path.save ~create:(`Exclusive 0o700) (Temp.path temporary path) contents;
  path
;;

let preflight ~sw env temporary source port =
  let token_path = launcher_argument_exn source "--bearer-token-file" in
  let token = Eio.Path.load (Temp.path temporary token_path) |> String.strip in
  Temp.register_secret temporary token;
  let connection =
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri:(Uri.of_string (endpoint port))
      ~bearer_token:(Some token)
      ~notification_capacity:64
    |> F.ok
  in
  Exn.protect
    ~f:(fun () ->
      ignore
        (Support.Unix_driver.initialize connection |> F.ok
         : Agent_protocol.Initialize.Response.t);
      say env "http-link.authenticated-preflight=passed")
    ~finally:(fun () -> Agent_client.Connection.close connection)
;;

let self_check ~sw env =
  let listener =
    Eio.Net.listen
      ~sw
      ~backlog:4
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let upstream_port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> failwith "echo listener returned Unix address"
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Net.run_server
      listener
      (fun flow _ -> Eio.Flow.copy flow flow)
      ~on_error:(function
        | Eio.Io _ -> ()
        | exn -> Exn.reraise exn "TCP relay self-check"));
  let link, port = Link.start_tcp ~sw ~env ~upstream_port in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    Unix_link.self_check_connections
      ~sw
      env
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      link);
  say env "http-link.self-check=passed"
;;

let manual ~sw env temporary source_dir =
  let source =
    Filename.concat source_dir "http-attach.sh" |> Temp.path temporary |> Eio.Path.load
  in
  let upstream_port = upstream_port_exn source in
  let link, port = Link.start_tcp ~sw ~env ~upstream_port in
  let path = launcher temporary source upstream_port port in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    preflight ~sw env temporary source port);
  say env ("launcher.http-relay-attach=" ^ path);
  say env ("http-link.endpoint=" ^ endpoint port);
  Unix_link.operator env link
;;

let run env ~case =
  Temp.with_ ~scenario:"tui-manual-http-link" ~env (fun temporary ->
    Eio.Switch.run (fun sw ->
      match case with
      | Some "self-check" -> self_check ~sw env
      | Some source_dir -> manual ~sw env temporary source_dir
      | None -> failwith "HTTP relay requires --case FIXTURE_LAUNCHER_DIRECTORY"))
;;
