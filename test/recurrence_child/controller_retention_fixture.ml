open! Core
open Support

let await_started started worker =
  Eio.Fiber.first
    (fun () -> Eio.Promise.await started)
    (fun () ->
       ignore (Eio.Promise.await_exn worker);
       failwith "worker completed before the retention boundary")
;;

let with_retention sw registry (entry : Registry.entry) f =
  let started, start = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let worker =
    Eio.Fiber.fork_promise ~sw (fun () ->
      match
        Actor.with_quiescent_state entry.actor ~f:(fun _ ->
          Registry.with_loaded_retention
            registry
            ~actor:entry.actor
            ~runtime:entry.runtime
            ~handle:(Option.value_exn entry.store_handle)
            (fun () ->
               Eio.Promise.resolve start ();
               Eio.Promise.await released))
        |> protocol_ok
      with
      | Some (Some ()) -> ()
      | Some None | None -> failwith "the exact quiescent controller was not retained")
  in
  protect_cleanup
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        Eio.Promise.resolve release ();
        Eio.Promise.await_exn worker))
    (fun () ->
       await_started started worker;
       f ())
;;
