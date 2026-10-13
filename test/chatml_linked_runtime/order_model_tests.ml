open Core
module B = Chatml.Chatml_builtin_spec
module Surface = Chatml.Chatml_builtin_surface
module Runtime = Chatml_linked_runtime
module Graph = Chatml_dependency_graph
module Linked = Chatml_linked_program
module P = Chatml_prepared_program
module S = Chatml_source

(* Independent bounded DFS model, driven by integer adjacency, not emitted graph
   nodes/order or resolver annotations. Node numbers only point backwards; unused
   catalog nodes remain available without becoming initialization requests. *)
let expected dependencies root =
  let seen = Hash_set.create (module Int) in
  let completed = ref [] in
  let rec visit node =
    if not (Hash_set.mem seen node)
    then (
      Hash_set.add seen node;
      List.iter (List.nth_exn dependencies node) ~f:visit;
      completed := node :: !completed)
  in
  visit root;
  List.rev !completed
;;

let expected_for_roots dependencies ~checked roots =
  let seen = Hash_set.create (module Int) in
  let rec visit node =
    if not (Hash_set.mem seen node)
    then (
      Hash_set.add seen node;
      List.iter (List.nth_exn dependencies node) ~f:visit)
  in
  List.iter roots ~f:visit;
  List.filter checked ~f:(Hash_set.mem seen)
;;

let%test_unit "initialization follows actual reachable DAGs under inventory permutations" =
  List.iter
    [ [ []; []; [] ]
    ; [ []; [ 0 ]; [ 1 ] ]
    ; [ []; [ 0 ]; [ 0 ]; [ 1; 2 ] ]
    ; [ []; [ 0 ]; []; [ 0; 1; 2 ] ]
    ; [ []; [ 0 ]; [ 1 ]; [ 0 ]; [ 2; 3 ] ]
    ; [ []; [ 0 ]; [ 0; 1 ]; [ 0; 1; 2 ]; [ 1; 3 ] ]
    ]
    ~f:(fun dependencies ->
      let log = ref [] in
      let surface = Runtime_tests.marked_surface log Surface.empty in
      let contract =
        Support.native_contract ~surface ~requirements:[ "answer", B.TInt ] ()
      in
      let root = List.length dependencies - 1 in
      let name node = "M" ^ Int.to_string node in
      let path node = "node" ^ Int.to_string node ^ ".chatml" in
      let files =
        List.mapi dependencies ~f:(fun node deps ->
          let terms = "1" :: List.map deps ~f:(fun dep -> name dep ^ ".value") in
          ( path node
          , Printf.sprintf
              "let initialize = mark(%S) let value = %s%s"
              (name node)
              (String.concat ~sep:" + " terms)
              (if Int.equal node root then " let answer = value" else "")
          , List.map deps ~f:(fun dep -> name dep, path dep) ))
      in
      let entry = List.last_exn files in
      let catalog = List.take files root in
      List.iter
        [ catalog; List.rev catalog ]
        ~f:(fun catalog ->
          log := [];
          let program =
            Support.input contract (entry :: catalog) |> Support.compiled contract
          in
          Runtime.instantiate program ~surface ~entrypoints:[ "answer" ] ()
          |> Runtime_tests.initialized
          |> ignore;
          [%test_eq: string list]
            (List.rev !log)
            (List.map (expected dependencies root) ~f:name);
          let graph = Linked.graph program in
          let checked = expected dependencies root in
          [%test_eq: string list]
            (Support.paths (Graph.nodes graph))
            (List.map checked ~f:path);
          List.iter
            [ []
            ; [ root ]
            ; List.take checked 1
            ; List.rev (List.take checked 2)
            ; List.rev checked
            ]
            ~f:(fun roots ->
              let root_ids =
                List.map roots ~f:(fun node ->
                  let source_id =
                    S.Id.create
                      ~package_namespace:"linked-test"
                      ~relative_path:(path node)
                    |> Or_error.ok_exn
                  in
                  P.Module_id.of_implementation source_id)
              in
              let actual =
                Graph.runtime_order_for_roots
                  ~input_fingerprint:(Graph.input_fingerprint graph)
                  ~checked_order:(Graph.nodes graph)
                  ~edges:(Graph.edges graph)
                  ~roots:root_ids
                  ~checkpoint:(fun () -> ())
                |> function
                | Ok ids -> Support.paths ids
                | Error diagnostic -> failwith diagnostic.Chatml_diagnostic.message
              in
              [%test_eq: string list]
                actual
                (List.map (expected_for_roots dependencies ~checked roots) ~f:path))))
;;
