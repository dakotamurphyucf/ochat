open Core
module Local = Chatml_local_source

let member path = Source.Portable_path.create path |> Or_error.ok_exn

let ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected local source failure", (error : Local.Error.t)]
;;

let admitted = function
  | Ok value -> value
  | Error (`Filesystem error) -> raise_s [%sexp (error : Local.Error.t)]
  | Error (`Callback ()) -> failwith "unexpected callback rejection"
;;

let with_owner root f =
  Local.with_root root ~max_path_components:8 ~checkpoint:ignore ~f:(fun owner ->
    Ok (f owner))
  |> admitted
;;

let read ?(checkpoint = ignore) ?(max_bytes = 16_384) owner path =
  Local.read owner ~member:(member path) ~max_bytes ~checkpoint
;;

let names ?(checkpoint = ignore) ?(max_entries = 512) ?directory owner =
  Local.fold_directory
    owner
    ~directory:(Option.map directory ~f:member)
    ~max_entries
    ~init:[]
    ~checkpoint
    ~f:(fun names ~name -> Ok (name :: names))
  |> admitted
  |> fun (names, count) -> List.sort names ~compare:String.compare, count
;;

let with_workspace f =
  Eio_main.run (fun env ->
    let temporary =
      Eio_unix.run_in_systhread (fun () ->
        Core_unix.mkdtemp "/tmp/ochat-local-source-XXXXXX")
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Fun.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      (fun () ->
         let workspace = Eio.Path.(anchor / "workspace") in
         Eio.Path.mkdir ~perm:0o755 workspace;
         Eio.Path.with_open_dir workspace (fun root -> f env anchor workspace root)))
;;

let save path bytes = Eio.Path.save ~create:(`Or_truncate 0o644) path bytes

let error result operation code =
  match result with
  | Ok _ -> failwith "expected local source rejection"
  | Error error ->
    assert (Local.Error.equal_operation (Local.Error.operation error) operation);
    assert (Local.Error.equal_code (Local.Error.code error) code)
;;

let filesystem_error result operation code =
  match result with
  | Error (`Filesystem failure) -> error (Error failure) operation code
  | Ok _ | Error (`Callback _) -> failwith "expected filesystem rejection"
;;

let descriptor_count env =
  Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / "/dev/fd") |> List.length
;;

let retained_or_rejected result ~inside =
  match result with
  | Error _ -> ()
  | Ok bytes -> assert (String.equal bytes inside)
;;

(* Every observed cooperative boundary is an independent interruption point.
   Compare live native descriptors without forcing GC, then reuse the same owner. *)
let interrupt_each ~env ~run ~reuse =
  let polls = ref 0 in
  run (fun () -> incr polls);
  assert (!polls > 0);
  let before = descriptor_count env in
  for stop = 1 to !polls do
    let token = Failure "local-source-checkpoint" in
    let visited = ref 0 in
    let interrupted =
      try
        run (fun () ->
          incr visited;
          if !visited = stop then raise token);
        false
      with
      | exception_ ->
        assert (phys_equal token exception_);
        true
    in
    assert interrupted;
    assert (descriptor_count env = before);
    reuse ();
    assert (descriptor_count env = before)
  done
;;
