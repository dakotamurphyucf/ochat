open Core
module Local = Chatml_local_source
module Roots = Chatml_local_roots

let ok_local = function
  | Ok value -> value
  | Error e -> raise_s [%sexp (e : Local.Error.t)]
;;

let ok_roots = function
  | Ok value -> value
  | Error e -> raise_s [%sexp (Roots.Error.code e : Roots.Error.code)]
;;

let admitted = function
  | Ok value -> value
  | Error (`Roots e) -> raise_s [%sexp (Roots.Error.code e : Roots.Error.code)]
  | Error (`Callback ()) -> failwith "unexpected callback failure"
;;

let limits
      ?(bytes = 2048)
      ?(components = 16)
      ?(anchors = 16)
      ?(roots = 256)
      ?(work = 100_000_000)
      ()
  =
  Roots.Limits.create
    ~max_locator_bytes:bytes
    ~max_locator_components:components
    ~max_anchors:anchors
    ~max_retained_roots:roots
    ~max_work_steps:work
  |> Or_error.ok_exn
;;

let anchor ?(limits = limits ()) root prefix =
  Roots.Anchor.create
    root
    ~host_prefix:prefix
    ~max_path_components:16
    ~limits
    ~checkpoint:ignore
  |> ok_roots
;;

let resolve ?base ?(checkpoint = ignore) t locator =
  Roots.resolve t ~base ~locator ~checkpoint
;;

let code result expected =
  match result with
  | Ok _ -> failwith "expected root failure"
  | Error e -> assert (Roots.Error.equal_code (Roots.Error.code e) expected)
;;

let path parts =
  Local.Directory_path.create parts ~max_bytes:2048 ~max_components:16 ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let member s = Source.Portable_path.create s |> Or_error.ok_exn

let read owner =
  Local.read owner ~member:(member "marker") ~max_bytes:4096 ~checkpoint:ignore
  |> ok_local
;;

let save path bytes = Eio.Path.save ~create:(`Or_truncate 0o644) path bytes

let with_workspace f =
  Eio_main.run (fun env ->
    let temporary =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.mkdtemp "/tmp/ochat-local-roots-XXXXXX")
    in
    let workspace = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Fun.protect
      ~finally:(fun () -> Eio.Path.rmtree workspace)
      (fun () -> Eio.Path.with_open_dir workspace (fun root -> f env workspace root)))
;;

let fd_count env =
  Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / "/dev/fd") |> List.length
;;

let interrupt_each ~env run =
  let count = ref 0 in
  run (fun () -> incr count);
  let before = fd_count env in
  for stop = 1 to !count do
    let token = Failure "root-checkpoint" in
    let seen = ref 0 in
    let interrupted =
      try
        run (fun () ->
          incr seen;
          if !seen = stop then raise token);
        false
      with
      | exn ->
        assert (phys_equal exn token);
        true
    in
    assert interrupted;
    assert (fd_count env = before)
  done
;;
