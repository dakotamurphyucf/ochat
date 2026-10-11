open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module R = Chatml_host_runtime
module X = Chatml_execution

let surface = Chatml.Chatml_builtin_surface.moderator_surface
let compile source = R.compile_script ~surface ~source () |> Result.ok_or_failwith

let outcome = function
  | Ok _ -> "ok"
  | Error error -> error.X.code
;;

let%expect_test "rendering an array task still checks cyclic captured input" =
  Eio_main.run (fun env ->
    let result =
      X.run
        ~env
        ~config:(R.default_runtime_config ~surface ())
        ~program:
          (compile
             {|let main input =
        let values = [`Null] in
        let pending = Task.map_array(values, fun value -> Task.pure(value)) in
        let ignored = values[0] <- `Array(values) in
        Task.pure(to_string(pending))|})
        ~entrypoint:"main"
        ~arguments:[ L.VUnit ]
        ()
    in
    print_endline (outcome result));
  [%expect {| chatml.value_limit |}]
;;

let%expect_test "snapshot allocation is charged before array task callbacks" =
  Eio_main.run (fun env ->
    let calls = ref 0 in
    B.set_print_sink (fun _ -> incr calls);
    Exn.protect ~finally:B.clear_print_sink ~f:(fun () ->
      List.iter [ "map_array"; "iter_array" ] ~f:(fun operation ->
        let source =
          "let main input = Task."
          ^ operation
          ^ "(input, fun value -> let ignored = print(value) in Task.pure(()))"
        in
        let result =
          X.run
            ~env
            ~policy:(Bounded { X.default_limits with allocation_bytes = 1024 })
            ~config:(R.default_runtime_config ~surface ())
            ~program:(compile source)
            ~entrypoint:"main"
            ~arguments:[ L.VArray (Array.create ~len:100 L.VUnit) ]
            ()
        in
        print_s [%sexp (operation : string), (outcome result : string), (!calls : int)])));
  [%expect
    {|
    (map_array chatml.allocation_limit 0)
    (iter_array chatml.allocation_limit 0)
    |}]
;;

let%expect_test "task catch cannot recover exhausted traversal fuel" =
  Eio_main.run (fun env ->
    let recovery_calls = ref 0 in
    B.set_print_sink (fun _ -> incr recovery_calls);
    Exn.protect ~finally:B.clear_print_sink ~f:(fun () ->
      let result =
        X.run
          ~env
          ~policy:(Bounded { X.default_limits with fuel = 300 })
          ~config:(R.default_runtime_config ~surface ())
          ~program:
            (compile
               {|let main input = Task.catch(
          Task.map_array(input, fun value -> Task.pure(value)),
          fun error -> let ignored = print(error) in Task.pure([]))|})
          ~entrypoint:"main"
          ~arguments:[ L.VArray (Array.create ~len:100 L.VUnit) ]
          ()
      in
      print_s [%sexp (outcome result : string), (!recovery_calls : int)]));
  [%expect {| (chatml.execution_limit 0) |}]
;;

let%expect_test "cancellation during a callback effect skips recovery and later callbacks"
  =
  Eio_main.run (fun env ->
    let names = ref [] in
    let handlers =
      { R.default_handlers with
        on_tool_call =
          (fun _ ~name ~args:_ ->
            names := name :: !names;
            raise (Eio.Cancel.Cancelled Exit))
      }
    in
    let result () =
      X.run
        ~env
        ~config:(R.default_runtime_config ~surface ~handlers ())
        ~program:
          (compile
             {|let main input = Task.catch(
        Task.map_array(["first", "second"], fun name -> Tool.call(name, `Null)),
        fun error -> Task.map(Tool.call("recovery", `Null), fun ignored -> []))|})
        ~entrypoint:"main"
        ~arguments:[ L.VUnit ]
        ()
    in
    (match result () with
     | _ -> failwith "cancellation was swallowed"
     | exception Eio.Cancel.Cancelled Exit -> ());
    print_s [%sexp (List.rev !names : string list)]);
  [%expect {| (first) |}]
;;

let%expect_test "table snapshots reserve allocation for the copied entries" =
  Eio_main.run (fun env ->
    let entry index =
      L.VRecord
        (String.Map.of_alist_exn
           [ "key", L.VString (Int.to_string index); "value", L.VUnit ])
    in
    let input = L.VRef (ref (L.VArray (Array.init 100 ~f:entry))) in
    let result =
      X.run
        ~env
        ~policy:(Bounded { X.default_limits with allocation_bytes = 2048 })
        ~config:(R.default_runtime_config ~surface ())
        ~program:(compile "let main input = Task.pure(Hashtbl.to_array(input))")
        ~entrypoint:"main"
        ~arguments:[ input ]
        ()
    in
    print_endline (outcome result));
  [%expect {| chatml.allocation_limit |}]
;;

let%expect_test "malformed unit callback result is rejected at the traversal boundary" =
  let state =
    Chatml.Chatml_task_array.create [| L.VInt 1 |] ~f:L.VUnit ~mode:Iter ~control:None
  in
  ignore (Chatml.Chatml_task_array.start state : Chatml.Chatml_task_array.Step.t);
  (match Chatml.Chatml_task_array.continue_with state (L.VInt 1) with
   | Ok _ -> failwith "non-unit result accepted"
   | Error message -> print_endline message);
  [%expect {| Task.iter_array: callback task must return unit |}]
;;
