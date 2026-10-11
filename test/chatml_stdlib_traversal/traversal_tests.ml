open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module R = Chatml_host_runtime
module X = Chatml_execution

let surface = Chatml.Chatml_builtin_surface.core_surface
let compile source = R.compile_script ~surface ~source () |> Result.ok_or_failwith

let run source =
  R.run_entrypoint
    (R.default_runtime_config ~surface ())
    (compile source)
    ~entrypoint:"main"
    ~arguments:[ L.VUnit ]
    ()
  |> Result.ok_or_failwith
  |> B.value_to_string
  |> print_endline
;;

let%expect_test "array tasks defer callbacks and own fresh snapshots on each run" =
  run
    {|let input = [1, 2, 3]
let calls = ref(0)
let pending = Task.map_array(input, fun n ->
  let ignored = calls := !calls + 1 in
  let ignored = input[1] <- 9 in Task.pure(n * 10))
let main ignored =
  let before = !calls in
  let ignored = input[0] <- 4 in
  let* first = pending in
  let* second = pending in
  let ignored = first[0] <- 99 in
  Task.pure({before = before; calls = !calls; first = first; second = second})|};
  [%expect
    {| { before = 0; calls = 6; first = [|99, 20, 30|]; second = [|40, 90, 30|] } |}]
;;

let%expect_test "empty traversal and sequential unit traversal" =
  run
    {|let order = ref(0)
let main ignored =
  let* empty = Task.map_array([], fun n -> fail("must not run")) in
  let* ignored = Task.iter_array([], fun n -> fail("must not run")) in
  let* ignored = Task.iter_array([1, 2, 3], fun n ->
    let ignored = order := !order * 10 + n in Task.pure(())) in
  Task.pure({size = Array.length(empty); order = !order})|};
  [%expect {| { order = 123; size = 0 } |}]
;;

let%expect_test "task failure stops callbacks and leaves explicit catch in charge" =
  run
    {|let seen = ref(0)
let main ignored =
  let pending = Task.map_array([1, 2, 3], fun n ->
    let ignored = seen := !seen * 10 + n in
    if n == 2 then Task.fail("stop") else Task.pure(n)) in
  let* values = Task.catch(pending, fun message -> Task.pure([String.length(message)])) in
  Task.pure({seen = !seen; values = values})|};
  [%expect {| { seen = 12; values = [|4|] } |}]
;;

let%expect_test "task snapshots retain shallow payload aliases" =
  run
    {|let cell = ref(1)
let main ignored =
  let* values = Task.map_array([cell, cell], fun shared ->
    let ignored = shared := !shared + 1 in Task.pure(shared)) in
  Task.pure([!values[0], !values[1], !cell])|};
  [%expect {| [|3, 3, 3|] |}]
;;

let%expect_test "traversal rejects incompatible callback signatures" =
  List.iter
    [ "let main input = Task.iter_array([1], fun n -> Task.pure(n))"
    ; "let main input = Task.map_array([1], fun n -> n + 1)"
    ; "let main input = Task.map_array([1], fun a b -> Task.pure(a + b))"
    ]
    ~f:(fun source ->
      match R.compile_script ~surface ~source () with
      | Error _ -> ()
      | Ok _ -> failwith "invalid callback accepted");
  print_endline "three invalid callback signatures rejected";
  [%expect {| three invalid callback signatures rejected |}]
;;

let%expect_test "table snapshots preserve insertion history" =
  run
    {|let table = Hashtbl.create()
let main ignored =
  let ignored = Hashtbl.set(table, "b", 1) in
  let ignored = Hashtbl.set(table, "a", 2) in
  let ignored = Hashtbl.set(table, "b", 3) in
  let before = Hashtbl.to_array(table) in
  let ignored = Hashtbl.remove(table, "b") in
  let ignored = Hashtbl.set(table, "b", 4) in
  let after = Hashtbl.to_array(table) in
  let ignored = before[0] <- {key = "changed"; value = 99} in
  Task.pure({before = before; after = after; b = Hashtbl.get(table, "b")})|};
  [%expect
    {| { after = [|{ key = a; value = 2 }, { key = b; value = 4 }|]; b = `Some(4); before = [|{ key = changed; value = 99 }, { key = a; value = 2 }|] } |}]
;;

let%expect_test "snapshot entries are fresh and payloads remain shared" =
  let cell = ref (L.VInt 1) in
  let entry =
    L.VRecord (String.Map.of_alist_exn [ "key", L.VString "a"; "value", L.VRef cell ])
  in
  let source = [| entry |] in
  let result = Chatml.Chatml_table_snapshot.to_array (L.VRef (ref (L.VArray source))) in
  [%test_eq: bool] false (phys_equal source result);
  [%test_eq: bool] false (phys_equal entry result.(0));
  source.(0) <- L.VUnit;
  cell := L.VInt 7;
  print_endline (B.value_to_string (L.VArray result));
  [%expect {| [|{ key = a; value = ref(7) }|] |}]
;;

let%expect_test "snapshot rejects structurally forged table entries" =
  let record fields = L.VRecord (String.Map.of_alist_exn fields) in
  let entry = record [ "key", L.VString "a"; "value", L.VInt 1 ] in
  List.iter
    [ [| L.VUnit |]
    ; [| record [ "key", L.VString "a" ] |]
    ; [| record [ "key", L.VInt 1; "value", L.VInt 2 ] |]
    ; [| record [ "key", L.VString "a"; "value", L.VInt 1; "extra", L.VUnit ] |]
    ; [| entry; entry |]
    ]
    ~f:(fun entries ->
      match Chatml.Chatml_table_snapshot.to_array (L.VRef (ref (L.VArray entries))) with
      | _ -> failwith "invalid table accepted"
      | exception Failure message ->
        [%test_eq: string]
          "Hashtbl.to_array: expected a table of unique string keys"
          message);
  print_endline "five malformed tables rejected";
  [%expect {| five malformed tables rejected |}]
;;

let%expect_test "bounded large traversal is stack safe and preserves order" =
  let result =
    R.run_entrypoint
      (R.default_runtime_config ~surface ())
      (compile {|let main input = Task.map_array(input, fun n -> Task.pure(n + 1))|})
      ~entrypoint:"main"
      ~arguments:[ L.VArray (Array.init 20_000 ~f:(fun n -> L.VInt n)) ]
      ()
    |> Result.ok_or_failwith
  in
  (match result with
   | L.VArray output ->
     Array.iteri output ~f:(fun index -> function
       | L.VInt n -> [%test_eq: int] (index + 1) n
       | _ -> failwith "wrong output type");
     print_s [%sexp (Array.length output : int)]
   | _ -> failwith "wrong result type");
  [%expect {| 20000 |}]
;;
