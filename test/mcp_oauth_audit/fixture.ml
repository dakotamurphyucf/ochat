open Core

let check condition message = if not condition then failwith message

let error = function
  | Error message -> message
  | Ok _ -> failwith "expected Error"
;;

let expect_cancel f =
  match f () with
  | _ -> failwith "expected Eio cancellation"
  | exception Eio.Cancel.Cancelled _ -> ()
;;

let set_env key = function
  | Some data -> Core_unix.putenv ~key ~data
  | None -> Core_unix.unsetenv key
;;

let with_environment settings f =
  let saved = List.map settings ~f:(fun (key, _) -> key, Sys.getenv key) in
  List.iter settings ~f:(fun (key, value) -> set_env key value);
  Fun.protect f ~finally:(fun () ->
    List.iter saved ~f:(fun (key, value) -> set_env key value))
;;

let with_cache env f =
  let root =
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      Eio.Buf_read.take_all
      [ "mktemp"; "-d"; "/tmp/mcp-oauth-audit.XXXXXX" ]
    |> String.strip
  in
  let path = Eio.Path.(Eio.Stdenv.fs env / root) in
  Fun.protect
    (fun () ->
       with_environment
         [ "XDG_CONFIG_HOME", Some root
         ; "XDG_CACHE_HOME", Some root
         ; "MCP_CLIENT_ID", None
         ; "MCP_CLIENT_SECRET", None
         ; "OAUTH_NO_BROWSER", Some "1"
         ]
         (fun () -> f root))
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree path))
;;

let run env (name, test) =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () -> test env);
  Eio.Flow.copy_string ("PASS " ^ name ^ "\n") (Eio.Stdenv.stdout env)
;;
