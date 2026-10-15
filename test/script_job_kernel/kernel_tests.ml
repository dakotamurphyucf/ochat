open! Core

let () = Mirage_crypto_rng_unix.use_default ()

module K = Agent_session.Script_job_kernel
module P = Agent_protocol
module C = Chat_response.Tool_capability
module L = Chatml.Chatml_lang

let ok = function
  | Ok value -> value
  | Error _ -> failwith "fixture rejected"
;;

let selected owner =
  let module Definition = struct
    type input = string

    let name = "tool"
    let description = None
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let native =
    Ochat_function.create_function
      (module Definition)
      (fun _ -> Openai.Responses.Tool_output.Output.Text "unused")
  in
  C.create
    ~owner
    ~resource_fingerprint:(String.make 64 'a')
    [ String.make 64 'b', native ]
  |> ok
;;

let job number = P.Id.Job.of_string ("job_kernel_" ^ Int.to_string number) |> ok

let start id =
  L.
    { op = "Job.start_tool"
    ; args = [ VString (P.Id.Job.to_string id); VString "tool"; VUnit ]
    }
;;

let%test_unit "lexical selected starts and aborts follow a bounded reference model" =
  Eio_main.run (fun _env ->
    for count = 1 to 4 do
      let capabilities = selected ("owner_" ^ Int.to_string count) in
      let current = ref capabilities in
      let aborted = ref [] in
      let ids = List.init count ~f:job in
      let result =
        K.with_scope
          ~selected:capabilities
          ~current_capabilities:(fun () -> !current)
          ~abort:(fun id -> aborted := id :: !aborted)
          ~error:Fn.id
          (fun scope ->
             if Int.equal count 1
             then (
               K.rebind_selected
                 scope
                 ~selected:capabilities
                 ~current_capabilities:(fun () -> !current)
               |> ok;
               assert (
                 Result.is_error
                   (K.rebind_selected
                      scope
                      ~selected:capabilities
                      ~current_capabilities:(fun () -> !current))));
             List.iter ids ~f:(fun id -> K.record_start scope id |> ok);
             assert (
               Result.is_error
                 (K.rebind_selected
                    scope
                    ~selected:capabilities
                    ~current_capabilities:(fun () -> !current)));
             let selected_ids = List.filteri ids ~f:(fun index _ -> index % 2 = 0) in
             let effects = List.map selected_ids ~f:start in
             let actual, ordinary = K.split_starts scope effects |> ok in
             assert (List.equal P.Id.Job.equal actual selected_ids);
             assert (List.is_empty ordinary);
             assert (Result.is_error (K.record_start scope (List.hd_exn ids)));
             assert (Result.is_error (K.split_starts scope [ start (job 99) ]));
             assert (
               List.for_all ids ~f:(fun id ->
                 Result.is_ok (K.validate_work scope (P.Invocation.Job id))));
             current := selected "replacement";
             assert (Result.is_error (K.record_start scope (job 100)));
             Error "selected binding changed")
      in
      assert (Result.is_error result);
      assert (List.equal P.Id.Job.equal (List.rev !aborted) (List.rev ids))
    done)
;;

let%test_unit "all provisional starts are attempted on cleanup and original cause wins" =
  Eio_main.run (fun _env ->
    let capabilities = selected "owner" in
    let first = job 1 in
    let second = job 2 in
    let callback_failure = Failure "callback" in
    let cleanup_failure = Failure "cleanup" in
    let attempted = ref [] in
    let run callback =
      K.with_scope
        ~selected:capabilities
        ~current_capabilities:(fun () -> capabilities)
        ~abort:(fun id ->
          attempted := id :: !attempted;
          if P.Id.Job.equal id second then raise cleanup_failure)
        ~error:Fn.id
        (fun scope ->
           K.record_start scope first |> ok;
           K.record_start scope second |> ok;
           callback ())
    in
    (match run (fun () -> raise callback_failure) with
     | exception actual -> assert (phys_equal actual callback_failure)
     | Ok _ | Error _ -> failwith "callback exception was lost");
    assert (List.equal P.Id.Job.equal !attempted [ first; second ]);
    attempted := [];
    (match run (fun () -> Error "ordinary error") with
     | exception actual -> assert (phys_equal actual cleanup_failure)
     | Ok _ | Error _ -> failwith "cleanup exception was lost");
    assert (List.equal P.Id.Job.equal !attempted [ first; second ]))
;;

let%test_unit "acknowledgment is lexical and cancellation retains its cause" =
  Eio_main.run (fun _env ->
    let capabilities = selected "owner" in
    let id = job 1 in
    let aborted = ref 0 in
    let late_ack = ref None in
    let scope callback =
      K.with_scope
        ~selected:capabilities
        ~current_capabilities:(fun () -> capabilities)
        ~abort:(fun _ -> Int.incr aborted)
        ~error:Fn.id
        (fun active ->
           K.record_start active id |> ok;
           callback active)
    in
    let unacknowledged =
      scope (fun active ->
        late_ack := Some (K.mark_prepared active |> ok);
        Ok ())
    in
    assert (Result.is_error unacknowledged);
    assert (Int.equal !aborted 1);
    (match Option.value_exn !late_ack () with
     | exception Invalid_argument _ -> ()
     | () -> failwith "late acknowledgment revived the scope");
    let acknowledged =
      scope (fun active ->
        let ack = K.mark_prepared active |> ok in
        ack ();
        (match ack () with
         | exception Invalid_argument _ -> ()
         | () -> failwith "repeated acknowledgment revived the scope");
        Ok ())
    in
    assert (Result.is_ok acknowledged);
    assert (Int.equal !aborted 1);
    let original = Failure "cancelled" in
    (match
       Eio.Cancel.sub (fun cancellation ->
         scope (fun _active ->
           Eio.Cancel.cancel cancellation original;
           Eio.Fiber.check ();
           Ok ()))
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual original)
     | Ok _ | Error _ -> failwith "cancellation identity was lost");
    assert (Int.equal !aborted 2))
;;
