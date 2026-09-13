open Core
open Chatml
module L = Chatml_lang
module R = Chatml_moderator_runtime
module S = Chatml_builtin_spec
module Surface = Chatml_builtin_surface

let ok_or_fail = Result.ok_or_failwith
let show_value = Chatml_builtin_modules.value_to_string
let context ~phase () = L.VRecord (String.Map.singleton "phase" (L.VString phase))

let show_effects effects =
  List.map effects ~f:(fun (eff : L.eff) ->
    eff.op ^ "(" ^ String.concat ~sep:", " (List.map eff.args ~f:show_value) ^ ")")
  |> String.concat ~sep:"; "
  |> fun effects -> "[" ^ effects ^ "]"
;;

let source =
  {|
let initial_state = ""
let work input =
  Task.bind(
    Task.catch(
      Task.bind(Reservation.reserve(input), fun discarded -> Task.fail("undo")),
      fun failure -> Task.pure("caught")),
    fun ignored -> Reservation.reserve(input))
let on_event ctx state event = work("same")
let main input = work(input)
|}
;;

let config ?(rollback = fun _ -> ()) ?(reserve = fun _ -> Ok ()) issued =
  let reserve_builtin : S.builtin =
    { name = "reserve"
    ; scheme = S.TFun ([ S.TString ], S.task_ty S.TString)
    ; impl = (fun args -> L.VTask (TPerform { op = "Reservation.reserve"; args }))
    }
  in
  let surface =
    Surface.merge
      { Surface.empty with
        modules = [ { name = "Reservation"; exports = [ reserve_builtin ] } ]
      }
      Surface.moderator_surface
  in
  let operation : R.op_def =
    { name = "Reservation.reserve"
    ; kind = Local_transactional_with_result { rollback }
    ; phase_check = (fun _ -> Ok ())
    ; perform =
        (fun _ args ->
          match args with
          | [ L.VString _ ] ->
            let open Result.Let_syntax in
            let id = "ticket-" ^ Int.to_string (!issued + 1) in
            let%map () = reserve id in
            incr issued;
            L.VString id
          | _ -> Error "expected reservation input")
    }
  in
  let config = R.default_runtime_config ~surface () in
  { config with operations = operation :: config.operations }
;;

let%expect_test
    "recorded results distinguish identical starts across catch rollback and rejected \
     commits"
  =
  let issued = ref 0 in
  let live = Hash_set.create (module String) in
  let rollback = function
    | [ L.VString id; L.VString "same" ] -> Hash_set.remove live id
    | _ -> failwith "rollback lost the reservation identity"
  in
  let reserve id =
    match Hash_set.is_empty live with
    | false -> Error "capacity exhausted"
    | true ->
      Hash_set.add live id;
      Ok ()
  in
  let config = config ~rollback ~reserve issued in
  let compiled = R.compile_script ~surface:config.surface ~source () |> ok_or_fail in
  let session =
    R.instantiate_session
      config
      compiled
      ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
    |> ok_or_fail
  in
  let installed = ref 0 in
  let run reject =
    R.handle_event
      session
      ~context:(context ~phase:"tool_invoked" ())
      ~event:L.VUnit
      ~prepare_transaction:(fun transaction ->
        print_endline (show_effects transaction.local_effects);
        match reject with
        | true -> Error "storage rejected"
        | false -> Ok (fun () -> incr installed))
  in
  (match run true with
   | Error message -> print_endline message
   | Ok _ -> failwith "rejected transaction committed");
  [%test_eq: int] 0 !installed;
  assert (List.is_empty (R.committed_local_effects session));
  (* The host aborts surviving reservations when the whole commit is rejected. *)
  Hash_set.clear live;
  (match R.current_state session with
   | L.VString "" -> ()
   | _ -> failwith "rejected state installed");
  run false |> ok_or_fail;
  [%test_eq: int] 4 !issued;
  [%test_eq: int] 1 !installed;
  assert (Hash_set.mem live "ticket-4" && Hash_set.length live = 1);
  print_endline (show_value (R.current_state session));
  print_endline (show_effects (R.committed_local_effects session));
  [%expect
    {|
    [Reservation.reserve(ticket-2, same)]
    storage rejected
    [Reservation.reserve(ticket-4, same)]
    ticket-4
    [Reservation.reserve(ticket-4, same)]
    |}]
;;

let%expect_test
    "nested catch cleanup releases only discarded reservations in reverse order"
  =
  let source =
    {|
let initial_state = ""
let on_event ctx state event =
  Task.bind(Reservation.reserve("same"), fun kept ->
    Task.catch(
      Task.bind(Reservation.reserve("same"), fun outer ->
        Task.catch(
          Task.bind(Reservation.reserve("same"), fun inner -> Task.fail("inner")),
          fun failure -> Task.fail("outer"))),
      fun failure -> Reservation.reserve("same")))
|}
  in
  let rolled_back = ref [] in
  let config =
    config (ref 0) ~rollback:(function
      | L.VString id :: _ -> rolled_back := !rolled_back @ [ id ]
      | _ -> failwith "missing recorded result")
  in
  let compiled = R.compile_script ~surface:config.surface ~source () |> ok_or_fail in
  let session =
    R.instantiate_session
      config
      compiled
      ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
    |> ok_or_fail
  in
  R.handle_event session ~context:(context ~phase:"tool_invoked" ()) ~event:L.VUnit
  |> ok_or_fail;
  print_s [%sexp (!rolled_back : string list)];
  print_endline (show_effects (R.committed_local_effects session));
  [%expect
    {|
    (ticket-3 ticket-2)
    [Reservation.reserve(ticket-1, same); Reservation.reserve(ticket-4, same)]
    |}]
;;

let%expect_test
    "standalone result-recording operations require an explicit result transaction"
  =
  let issued = ref 0 in
  let config = config issued in
  let compiled = R.compile_script ~surface:config.surface ~source () |> ok_or_fail in
  let run ?prepare_result () =
    R.run_entrypoint
      ?prepare_result
      config
      compiled
      ~entrypoint:"main"
      ~arguments:[ L.VString "same" ]
      ()
  in
  assert (Result.is_error (run ()));
  [%test_eq: int] 0 !issued;
  let installed = ref 0 in
  let prepare_result reject ~value ~local_effects =
    (match value, local_effects with
     | ( L.VString id
       , [ { L.op = "Reservation.reserve"
           ; args = [ L.VString recorded; L.VString "same" ]
           }
         ] )
       when String.equal id recorded -> ()
     | _ -> failwith "result was not associated with its exact surviving reservation");
    print_endline (show_effects local_effects);
    match reject with
    | true -> Error "result rejected"
    | false -> Ok (fun () -> incr installed)
  in
  (match run ~prepare_result:(prepare_result true) () with
   | Error message -> print_endline message
   | Ok _ -> failwith "invalid result installed");
  [%test_eq: int] 0 !installed;
  let result = run ~prepare_result:(prepare_result false) () |> ok_or_fail in
  [%test_eq: int] 1 !installed;
  [%test_eq: int] 4 !issued;
  print_endline (show_value result);
  [%expect
    {|
    [Reservation.reserve(ticket-2, same)]
    result rejected
    [Reservation.reserve(ticket-4, same)]
    ticket-4
    |}]
;;
