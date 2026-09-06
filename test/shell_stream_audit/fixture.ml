open! Core
module S = Shell_access

let check condition message = if not condition then failwith message
let equal actual expected = check (String.equal actual expected) "unexpected safe output"

let executor_ok = function
  | Ok result -> result
  | Error error -> failwith (S.Executor.error_to_string error)
;;

let with_root env f =
  let bytes = Cstruct.create 12 in
  Eio.Flow.read_exact (Eio.Stdenv.secure_random env) bytes;
  let suffix =
    Cstruct.to_string bytes
    |> String.to_list
    |> List.map ~f:(fun byte -> sprintf "%02x" (Char.to_int byte))
    |> String.concat
  in
  let root = Eio.Path.(Eio.Stdenv.fs env / ("/tmp/ochat-shell-stream-" ^ suffix)) in
  Eio.Path.mkdir ~perm:0o700 root;
  Exn.protect
    ~f:(fun () -> f root)
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
;;

let save root name =
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) "ok"
;;

let await env predicate =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    let rec poll () =
      if not (predicate ())
      then (
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.002;
        poll ())
    in
    poll ())
;;

let config
      ?(interceptors = [])
      ?(secret_filter = S.Secret_filter.empty)
      ?(limits = S.Limits.default)
      ?(policy = S.Policy.create ~default:Allow [])
      ?(audit = S.Audit.ignore)
      env
      root
      backend
  =
  S.Executor.config
    ~env
    ~runtime_id:"stream-audit"
    ~manifest_sha256:"stream-fixture"
    ~cwd:root
    ~policy
    ~interceptors
    ~secret_filter
    ~limits
    ~audit
    ~backends:[ backend ]
    ~capabilities:
      S.Capabilities.
        { (development ~workspace:(Eio.Path.native_exn root)) with sandbox = Preferred }
    ()
;;

let invocation request : S.Executor.invocation =
  { request; input = Empty; rationale = None; origin = Host "stream-audit" }
;;

let command text = S.Command.create "/bin/echo" [ text ]
let request text = S.Request.command (command text) |> invocation

let sequence commands =
  let chain =
    S.Chain.create
      ~first:[ List.hd_exn commands ]
      ~rest:
        (List.map (List.tl_exn commands) ~f:(fun command -> S.Chain.Always, [ command ]))
    |> Result.ok_or_failwith
  in
  invocation (S.Request.Structured chain)
;;

let collect () =
  let stdout = Buffer.create 128
  and stderr = Buffer.create 128 in
  let on_progress (progress : S.Executor.progress) =
    check (Stdlib.String.is_valid_utf_8 progress.text) "invalid progress UTF-8";
    check (String.length progress.text <= 4096) "oversized progress event";
    Buffer.add_string
      (match progress.channel with
       | `Stdout -> stdout
       | `Stderr -> stderr)
      progress.text
  in
  stdout, stderr, on_progress
;;

let self () = Eio_posix.Low_level.realpath (Sys.get_argv ()).(0)

let child_command _env root mode =
  let executable = self () in
  check (not (Filename.is_relative executable)) "child executable must be absolute";
  S.Command.create executable [ "--child"; Eio.Path.native_exn root; mode ]
;;

let child_request env root mode =
  S.Request.command (child_command env root mode) |> invocation
;;
