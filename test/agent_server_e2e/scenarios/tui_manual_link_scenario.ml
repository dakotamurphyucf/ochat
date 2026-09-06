open Core
module Temp = Support.Temporary_environment
module Link = Support.Tui_manual_link
module F = Support.Tui_fixture

let say env text = Eio.Flow.copy_string (text ^ "\n") (Eio.Stdenv.stdout env)

let status env link =
  say
    env
    (sprintf
       "link.status available=%b connections=%d"
       (Link.is_available link)
       (Link.connection_count link))
;;

let operator env link =
  let input = Eio.Buf_read.of_flow ~max_size:4096 (Eio.Stdenv.stdin env) in
  let rec loop () =
    match Eio.Buf_read.line input with
    | "quit" -> ()
    | line ->
      (match String.strip line with
       | "cut" ->
         Link.cut link;
         status env link
       | "resume" ->
         Link.resume link;
         status env link
       | "status" -> status env link
       | _ -> say env "Commands: cut | resume | status | quit");
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let launcher temporary source_dir listen_path =
  let source_path = Filename.concat source_dir "unix-attach.sh" in
  let upstream = Filename.concat source_dir "tui-manual.sock" in
  let source = Eio.Path.load (Temp.path temporary source_path) in
  let pattern = "unix://" ^ upstream in
  F.require (String.is_substring source ~substring:pattern) "unexpected manual launcher";
  let contents =
    String.substr_replace_all source ~pattern ~with_:("unix://" ^ listen_path)
  in
  let path = Filename.concat (Temp.roots temporary).sockets "unix-relay-attach.sh" in
  Eio.Path.save ~create:(`Exclusive 0o700) (Temp.path temporary path) contents;
  path, upstream
;;

let expect_eof reader =
  match Eio.Buf_read.line reader with
  | _ -> failwith "cut relay unexpectedly returned data"
  | exception End_of_file -> ()
;;

let exchange flow reader text =
  Eio.Flow.copy_string (text ^ "\n") flow;
  F.require (String.equal (Eio.Buf_read.line reader) text) "relay round trip differs"
;;

let self_check_connections ~sw env address link =
  let connect () = Eio.Net.connect ~sw (Eio.Stdenv.net env) address in
  let flow = connect () in
  let reader = Eio.Buf_read.of_flow ~max_size:1024 flow in
  exchange flow reader "before";
  Link.cut link;
  expect_eof reader;
  F.await env (fun () -> if Link.connection_count link = 0 then Some () else None);
  expect_eof (Eio.Buf_read.of_flow ~max_size:1024 (connect ()));
  Link.resume link;
  let resumed = connect () in
  exchange resumed (Eio.Buf_read.of_flow ~max_size:1024 resumed) "after";
  Link.cut link
;;

let self_check ~sw env temporary path =
  let upstream = Filename.concat (Temp.roots temporary).sockets "echo.sock" in
  let listener = Eio.Net.listen ~sw ~backlog:4 (Eio.Stdenv.net env) (`Unix upstream) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Net.run_server
      listener
      (fun flow _ -> Eio.Flow.copy flow flow)
      ~on_error:(function
        | Eio.Io _ -> ()
        | exn -> Exn.reraise exn "manual relay self-check"));
  let link = Link.start ~sw ~env ~listen_path:path ~upstream in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    self_check_connections ~sw env (`Unix path) link);
  say env "link.self-check=passed"
;;

let run env ~case =
  Temp.with_ ~scenario:"tui-manual-link" ~env (fun temporary ->
    Eio.Switch.run (fun sw ->
      let path = Filename.concat (Temp.roots temporary).sockets "relay.sock" in
      match case with
      | Some "self-check" -> self_check ~sw env temporary path
      | Some source_dir ->
        let launcher, upstream = launcher temporary source_dir path in
        let link = Link.start ~sw ~env ~listen_path:path ~upstream in
        say env ("launcher.unix-relay-attach=" ^ launcher);
        status env link;
        operator env link
      | None -> failwith "manual relay requires --case FIXTURE_LAUNCHER_DIRECTORY"))
;;
