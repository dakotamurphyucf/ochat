open! Core
module Checkpoint = Chatml_compilation_checkpoint

let%expect_test "compiler scheduling checks every call and yields at a bounded cadence" =
  let count = 257 in
  let calls = ref 0 in
  let checks = ref [] in
  let yields = ref [] in
  let checkpoint =
    Checkpoint.create
      ~check:(fun () -> checks := !calls :: !checks)
      ~yield:(fun () -> yields := !calls :: !yields)
  in
  for call = 1 to count do
    calls := call;
    Checkpoint.run checkpoint
  done;
  let yields = List.rev !yields in
  List.iter
    (List.range 1 (count + 1))
    ~f:(fun call ->
      let expected = if List.mem yields call ~equal:Int.equal then 2 else 1 in
      [%test_eq: int] (List.count !checks ~f:(Int.equal call)) expected);
  (* A second invocation must yield immediately even when the first owner has
     just yielded. Its scheduling state cannot be shared between compilers. *)
  let independent_yields = ref 0 in
  let independent =
    Checkpoint.create ~check:(fun () -> ()) ~yield:(fun () -> Int.incr independent_yields)
  in
  Checkpoint.run independent;
  [%test_eq: int] !independent_yields 1;
  print_s [%sexp (yields : int list)];
  [%expect {| (1 65 129 193 257) |}]
;;

let%expect_test "checkpoint failures propagate before more work or cadence advancement" =
  let exception Stop in
  (* Inject failure before scheduling, inside the yield, and after it. This
     models cancellation or deadline expiry at each boundary without sleeps. *)
  List.iter [ 1; 2; 3 ] ~f:(fun fail_at ->
    let events = ref [] in
    let enabled = ref true in
    let calls = ref 0 in
    let visit event =
      events := event :: !events;
      Int.incr calls;
      if !enabled && !calls = fail_at then raise Stop
    in
    let checkpoint =
      Checkpoint.create ~check:(fun () -> visit "check") ~yield:(fun () -> visit "yield")
    in
    let resumed = ref false in
    (match
       Checkpoint.run checkpoint;
       resumed := true
     with
     | () -> failwith "checkpoint swallowed the exception"
     | exception Stop -> ());
    assert (not !resumed);
    [%test_eq: string list]
      (List.rev !events)
      (List.take [ "check"; "yield"; "check" ] fail_at);
    enabled := false;
    events := [];
    Checkpoint.run checkpoint;
    [%test_eq: string list] (List.rev !events) [ "check"; "yield"; "check" ];
    (* A failed ordinary check likewise cannot consume a scheduling slot. *)
    for _ = 1 to Checkpoint.max_checkpoints_between_yields - 2 do
      Checkpoint.run checkpoint
    done;
    enabled := true;
    calls := fail_at - 1;
    (match Checkpoint.run checkpoint with
     | () -> failwith "ordinary checkpoint swallowed the exception"
     | exception Stop -> ());
    enabled := false;
    events := [];
    Checkpoint.run checkpoint;
    [%test_eq: string list] (List.rev !events) [ "check" ];
    events := [];
    Checkpoint.run checkpoint;
    [%test_eq: string list] (List.rev !events) [ "check"; "yield"; "check" ]);
  [%expect {| |}]
;;
