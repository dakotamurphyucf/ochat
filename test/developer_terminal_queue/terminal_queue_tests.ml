open! Core
module Q = Agent_server.Developer_terminal_queue

let ok = function
  | Ok value -> value
  | Error _ -> failwith "terminal queue rejected valid model operation"
;;

let%test_unit "bounded completion order and retained head agree with input order" =
  List.iter
    [ [ 0 ]
    ; [ 0; 1 ]
    ; [ 1; 0 ]
    ; [ 0; 1; 2 ]
    ; [ 0; 2; 1 ]
    ; [ 1; 0; 2 ]
    ; [ 1; 2; 0 ]
    ; [ 2; 0; 1 ]
    ; [ 2; 1; 0 ]
    ]
    ~f:(fun completion_order ->
      let count = List.length completion_order in
      let queue = Q.create ~max_native_occupancy:count |> ok in
      let initial = Q.effect_witness queue |> Option.value_exn in
      let slots = List.init count ~f:(fun _ -> Q.reserve queue ~kind:Native |> ok) in
      assert (Result.is_error (Q.reserve queue ~kind:Native));
      List.iter completion_order ~f:(fun index ->
        Q.complete queue (List.nth_exn slots index) ~intent:index |> ok);
      assert (not (Q.check_effect_witness queue initial));
      assert (Option.is_none (Q.effect_witness queue));
      assert (Result.is_error (Q.reserve queue ~kind:Native));
      List.iter completion_order ~f:(fun expected ->
        let first = Q.take_head queue |> ok |> Option.value_exn in
        assert (Int.equal (Q.payload first) expected);
        assert (Result.is_error (Q.take_head queue));
        Q.retain queue first |> ok;
        let recovered = Q.take_head queue |> ok |> Option.value_exn in
        assert (Q.same_head first recovered);
        assert (Int.equal (Q.payload recovered) expected);
        assert (Result.is_error (Q.settle queue first));
        Q.settle queue recovered |> ok);
      assert (Q.is_empty queue);
      assert (not (Q.check_effect_witness queue initial));
      assert (Option.is_some (Q.effect_witness queue));
      assert (Option.is_none (Q.take_head queue |> ok)))
;;

let%test_unit "physical slot and drain cannot be borrowed from another queue" =
  let first = Q.create ~max_native_occupancy:1 |> ok in
  let other = Q.create ~max_native_occupancy:1 |> ok in
  let slot = Q.reserve first ~kind:Native |> ok in
  assert (Result.is_error (Q.complete other slot ~intent:1));
  assert (Result.is_error (Q.abandon other slot));
  Q.complete first slot ~intent:1 |> ok;
  let head = Q.take_head first |> ok |> Option.value_exn in
  assert (Result.is_error (Q.retain other head));
  assert (Result.is_error (Q.settle other head));
  Q.settle first head |> ok
;;

let%test_unit "root slot is separate; close retains completed custody" =
  let queue = Q.create ~max_native_occupancy:1 |> ok in
  let root = Q.reserve queue ~kind:Root |> ok in
  let native = Q.reserve queue ~kind:Native |> ok in
  assert (Result.is_error (Q.reserve queue ~kind:Root));
  Q.complete queue native ~intent:"native" |> ok;
  Q.complete queue root ~intent:"root" |> ok;
  Q.begin_close queue |> ok;
  assert (Result.is_error (Q.reserve queue ~kind:Native));
  assert (Option.is_none (Q.effect_witness queue));
  List.iter [ "native"; "root" ] ~f:(fun expected ->
    let drain = Q.take_head queue |> ok |> Option.value_exn in
    assert (String.equal expected (Q.payload drain));
    Q.settle queue drain |> ok);
  assert (Q.is_empty queue);
  assert (Option.is_none (Q.effect_witness queue))
;;
