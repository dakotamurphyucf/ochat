open Core
module Operation = Git_catalog_operation

exception Stop

let%test_unit "cancelled owner rejects before worker admission without hanging cleanup" =
  Eio_mock.Backend.run (fun () ->
    let called = ref false in
    (try
       Eio.Switch.run (fun sw ->
         let owner = Operation.create sw in
         Eio.Switch.fail sw Stop;
         Fun.protect
           ~finally:(fun () -> Operation.close owner)
           (fun () ->
              ignore (Operation.run owner ~f:(fun () -> called := true) : (_, _) Result.t)))
     with
     | Stop -> ());
    assert (not !called))
;;

let%test_unit "worker failures preserve exception identity and permit owner reuse" =
  Eio_mock.Backend.run (fun () ->
    Eio.Switch.run (fun sw ->
      let owner = Operation.create sw in
      let exception Marker of int ref in
      let marker = Marker (ref 1) in
      (match Operation.run owner ~f:(fun () -> raise marker) with
       | exception exn -> assert (phys_equal exn marker)
       | Ok _ | Error _ -> failwith "worker exception lost");
      assert (not (Operation.is_active owner));
      (match Operation.run owner ~f:(fun () -> 7) with
       | Ok value -> assert (Int.equal value 7)
       | Error _ -> failwith "owner not reusable after joined failure");
      Operation.close owner))
;;
