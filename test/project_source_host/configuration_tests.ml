open Core
open Support

let%expect_test "local-only admission has no Git authority and independent finite work" =
  let configured =
    Policy.configure Policy.default Configuration.local_only |> Or_error.ok_exn
  in
  print_s
    [%sexp
      (Configuration.local_only : Configuration.t)
    , (Option.is_none (Policy.Configured.git_probe_limits configured) : bool)
    , (Time_ns.Span.to_sec (Policy.attempt_wall_time Policy.default) : float)];
  let metadata = 16_384
  and pack = 1_048_576 in
  let payload = Int.max ((3 * metadata) + 7) ((4 * metadata) + (8 * pack)) in
  print_s [%sexp (payload : int), ((4 * payload) + 1_024 : int)];
  [%expect
    {|
    (((external_roots ()) (git ())) true 120)
    (8454144 33817600)
    |}]
;;

let%test_unit "raw and final administrative admission have exact independent bounds" =
  let roots count bytes =
    { Configuration.external_roots =
        List.init count ~f:(fun i ->
          "/" ^ String.make (bytes - 1) (Char.of_int_exn (65 + i)))
    ; git = None
    }
  in
  assert (Result.is_ok (Configuration.validate (roots 16 4_096)));
  assert (Result.is_error (Configuration.validate_raw (roots 17 4_096)));
  assert (Result.is_error (Configuration.validate_raw (roots 33 1)));
  assert (Result.is_error (Configuration.validate_raw (roots 1 4_097)));
  assert (
    Result.is_error
      (Configuration.validate { external_roots = [ "/same"; "/same" ]; git = None }));
  assert (
    Result.is_error
      (Configuration.validate_raw { external_roots = [ "/bad\000path" ]; git = None }));
  let relative = { Configuration.external_roots = [ "../shared" ]; git = None } in
  assert (Result.is_ok (Configuration.validate_raw relative));
  assert (Result.is_error (Configuration.validate relative));
  let git memory =
    { Configuration.external_roots = []
    ; git =
        Some
          { helper_directory = "/qualified/helpers"
          ; ca_bundle = None
          ; memory_bytes_per_process = memory
          ; repositories = []
          }
    }
  in
  assert (Result.is_error (Configuration.validate_raw (git 0)));
  assert (Result.is_ok (Configuration.validate (git 1)));
  List.iter [ 0.; -1. ] ~f:(fun seconds ->
    assert (
      Result.is_error
        (Policy.create
           ~attempt_wall_time:(Time_ns.Span.of_sec seconds)
           ~max_semantic_work_steps:1
           ~max_native_work_steps:1
           ~max_acquisition_work_steps:1)))
;;

exception Checkpoint of int ref

let%test_unit
    "trusted header admission is exact and callback failures are not filesystem failures"
  =
  with_workspace (fun env root _ ->
    let path = Eio.Path.(root / "header") in
    Eio.Path.save ~create:(`Exclusive 0o600) path (String.make 4_096 'x');
    let read maximum checkpoint =
      Project_source_header.read
        env#fs
        ~path:(Eio.Path.native_exn path)
        ~max_bytes:maximum
        ~checkpoint
    in
    assert (String.length (read 4_096 ignore |> ok) = 4_096);
    (match read 4_095 ignore with
     | Error Too_large -> ()
     | _ -> assert false);
    Eio.Path.save ~create:(`Or_truncate 0o600) path (String.make 4_097 'x');
    (match read 4_096 ignore with
     | Error Too_large -> ()
     | _ -> assert false);
    let marker = Checkpoint (ref 3) in
    (match read 4_096 (fun () -> raise marker) with
     | exception exn -> assert (phys_equal exn marker)
     | Ok _ | Error _ -> assert false);
    let io =
      try
        ignore (Eio.Path.load Eio.Path.(root / "missing") : string);
        assert false
      with
      | Eio.Io _ as exn -> exn
    in
    match read 4_096 (fun () -> raise io) with
    | exception exn -> assert (phys_equal exn io)
    | Ok _ | Error _ -> assert false)
;;
