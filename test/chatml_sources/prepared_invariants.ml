open Core
module S = Chatml_source
module P = Chatml_prepared_program
module Model = Source_invariants

let checkpoint () = ()

module Input = struct
  type t =
    { entry : S.Id.t
    ; sources : S.t list
    ; definitions : P.Definition.t list
    ; scopes : P.Scope.t list
    ; environment : P.Environment.t
    ; provenance : P.Provenance.t
    ; compiler_contract : S.Digest.t
    }

  let admit ?(max_input_bytes = None) ?(checkpoint = checkpoint) t =
    P.create
      ~max_input_bytes
      ~compiler_contract:t.compiler_contract
      ~environment:t.environment
      ~provenance:t.provenance
      ~entry:t.entry
      ~sources:t.sources
      ~definitions:t.definitions
      ~scopes:t.scopes
      ~checkpoint
      ()
  ;;

  let source t name =
    List.find_exn t.sources ~f:(fun source ->
      String.equal (S.Id.relative_path (S.id source)) name)
  ;;

  let fixture () =
    let capture = Model.capture in
    let declaration = capture ~path:"project.json" "declaration" in
    let lock = capture ~path:"project.lock" "locked bytes" in
    let entry = capture ~path:"main.chatml" "Graph.total" in
    let implementation = capture ~path:"graph.chatml" "let total = 3" in
    let interface = capture ~path:"graph.chatmli" "val total : int" in
    let declared_at =
      S.locate declaration ~start_byte:0 ~end_byte:1 ~checkpoint |> Or_error.ok_exn
    in
    let definition =
      P.Definition.create
        ~implementation:(S.id implementation)
        ~interface:(Some (S.id interface))
        ~declared_at
      |> Or_error.ok_exn
    in
    let entry_definition =
      P.Definition.create ~implementation:(S.id entry) ~interface:None ~declared_at
      |> Or_error.ok_exn
    in
    let scope source bindings =
      P.Scope.create (S.id source) ~bindings ~selected_at:(Some declared_at) ~checkpoint
      |> Or_error.ok_exn
    in
    let module_id = P.Definition.id definition in
    let bindings =
      List.map [ "Graph"; "Again" ] ~f:(fun name ->
        P.Scope.{ name; module_id; declared_at })
    in
    let compiler_contract = S.Digest.of_contents "fixture contract" ~checkpoint in
    { entry = S.id entry
    ; sources = [ declaration; lock; entry; implementation; interface ]
    ; definitions = [ definition; entry_definition ]
    ; scopes = [ scope entry bindings; scope implementation []; scope interface [] ]
    ; environment =
        P.Environment.create
          ~name:"fixture"
          ~required_contract:compiler_contract
          ~declared_at:(Some declared_at)
        |> Or_error.ok_exn
    ; provenance = Project { declaration = S.id declaration; lock = Some (S.id lock) }
    ; compiler_contract
    }
  ;;
end

let admitted = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let rec permutations = function
  | [] -> [ [] ]
  | first :: rest ->
    let rec insert = function
      | [] -> [ [ first ] ]
      | head :: tail as all ->
        (first :: all) :: List.map (insert tail) ~f:(fun suffix -> head :: suffix)
    in
    List.concat_map (permutations rest) ~f:insert
;;

let%test_unit "canonical admission matches independent inventory ordering and byte sums" =
  let input = Input.fixture () in
  let original = Input.admit input |> admitted in
  let digest = P.fingerprint original |> S.Digest.to_hex in
  let bytes =
    List.sum
      (module Int)
      input.sources
      ~f:(fun source -> String.length (S.contents source))
  in
  List.iter (permutations input.sources) ~f:(fun sources ->
    List.iter (permutations input.scopes) ~f:(fun scopes ->
      let entry_scope =
        List.find_exn scopes ~f:(fun scope ->
          S.Id.equal (P.Scope.source scope) input.entry)
      in
      List.iter
        (permutations (P.Scope.bindings entry_scope))
        ~f:(fun bindings ->
          let replacement =
            P.Scope.create
              input.entry
              ~bindings
              ~selected_at:(P.Scope.selected_at entry_scope)
              ~checkpoint
            |> Or_error.ok_exn
          in
          let scopes =
            List.map scopes ~f:(fun scope ->
              if S.Id.equal (P.Scope.source scope) input.entry then replacement else scope)
          in
          List.iter (permutations input.definitions) ~f:(fun definitions ->
            let prepared =
              Input.admit { input with sources; scopes; definitions } |> admitted
            in
            [%test_eq: string] (P.fingerprint prepared |> S.Digest.to_hex) digest;
            [%test_eq: int]
              (P.Accounting.source_count (P.accounting prepared))
              (List.length sources);
            [%test_eq: int] (P.Accounting.source_bytes (P.accounting prepared)) bytes;
            [%test_eq: int]
              (P.Accounting.metadata_bytes (P.accounting prepared))
              (P.Accounting.metadata_bytes (P.accounting original))))));
  let accounting = P.accounting original in
  assert (P.Accounting.metadata_bytes accounting > 0);
  [%test_eq: int]
    (P.Accounting.input_bytes accounting)
    (bytes + P.Accounting.metadata_bytes accounting);
  let limit = P.Accounting.input_bytes accounting in
  let at_limit = Input.admit ~max_input_bytes:(Some limit) input |> admitted in
  [%test_eq: string] (P.fingerprint at_limit |> S.Digest.to_hex) digest;
  (match Input.admit ~max_input_bytes:(Some (limit - 1)) input with
   | Error { problem = Input_limit _; _ } -> ()
   | Error _ | Ok _ ->
     failwith "aggregate admission must enforce exact input-byte boundary");
  let extended =
    { input with sources = Model.capture ~path:"unused.chatml" "" :: input.sources }
  in
  let additional = Input.admit extended |> admitted in
  let additional_accounting = P.accounting additional in
  [%test_eq: int] (P.Accounting.source_bytes additional_accounting) bytes;
  [%test_eq: int]
    (P.Accounting.source_count additional_accounting)
    (List.length input.sources + 1);
  assert (
    P.Accounting.metadata_bytes additional_accounting
    > P.Accounting.metadata_bytes accounting);
  assert (not (S.Digest.equal (P.fingerprint additional) (P.fingerprint original)));
  (match Input.admit ~max_input_bytes:(Some limit) extended with
   | Error { problem = Input_limit _; _ } -> ()
   | Error _ | Ok _ -> failwith "unused empty sources must consume input allowance");
  Model.interrupt_every_checkpoint (fun ~checkpoint ->
    Input.admit ~checkpoint input |> admitted |> ignore);
  [%test_eq: string] (P.fingerprint original |> S.Digest.to_hex) digest
;;

let%test_unit "all associations are admitted and selected input changes identity" =
  let input = Input.fixture () in
  let original = Input.admit input |> admitted in
  let expect_rejected modified = assert (Result.is_error (Input.admit modified)) in
  List.iter input.sources ~f:(fun source ->
    expect_rejected { input with sources = source :: input.sources };
    expect_rejected
      { input with
        sources =
          List.filter input.sources ~f:(fun current ->
            not (S.Id.equal (S.id current) (S.id source)))
      });
  List.iter input.scopes ~f:(fun scope ->
    expect_rejected { input with scopes = scope :: input.scopes };
    expect_rejected
      { input with
        scopes =
          List.filter input.scopes ~f:(fun current ->
            not (S.Id.equal (P.Scope.source current) (P.Scope.source scope)))
      });
  expect_rejected { input with definitions = input.definitions @ input.definitions };
  expect_rejected { input with definitions = [] };
  List.iter [ "GRAPH.chatml"; "graph.chatml/nested.chatml" ] ~f:(fun path ->
    expect_rejected { input with sources = Model.capture ~path "extra" :: input.sources });
  let entry_scope = P.find_scope original input.entry |> Option.value_exn in
  let binding = List.hd_exn (P.Scope.bindings entry_scope) in
  assert (
    Result.is_error
      (P.Scope.create
         input.entry
         ~bindings:[ binding; binding ]
         ~selected_at:None
         ~checkpoint));
  let definition = List.hd_exn input.definitions in
  assert (
    Result.is_error
      (P.Definition.create
         ~implementation:(P.Definition.implementation definition)
         ~interface:(Some (P.Definition.implementation definition))
         ~declared_at:(P.Definition.declared_at definition)));
  let changed_contract = S.Digest.of_contents "different contract" ~checkpoint in
  expect_rejected { input with compiler_contract = changed_contract };
  let changed_environment =
    P.Environment.create
      ~name:"other"
      ~required_contract:input.compiler_contract
      ~declared_at:(P.Environment.declared_at input.environment)
    |> Or_error.ok_exn
  in
  let changed_contract_environment =
    P.Environment.create
      ~name:"fixture"
      ~required_contract:changed_contract
      ~declared_at:(P.Environment.declared_at input.environment)
    |> Or_error.ok_exn
  in
  let changed_binding_scope =
    P.Scope.create
      input.entry
      ~bindings:
        (List.map (P.Scope.bindings entry_scope) ~f:(fun current ->
           if String.equal current.P.Scope.name binding.name
           then { current with name = "Renamed" }
           else current))
      ~selected_at:(P.Scope.selected_at entry_scope)
      ~checkpoint
    |> Or_error.ok_exn
  in
  let changed_source = Model.capture ~path:"graph.chatml" "let total = 4" in
  let modified =
    [ { input with environment = changed_environment }
    ; { input with
        environment = changed_contract_environment
      ; compiler_contract = changed_contract
      }
    ; { input with entry = S.id (Input.source input "graph.chatml") }
    ; { input with
        provenance =
          Project { declaration = S.id (Input.source input "project.json"); lock = None }
      }
    ; { input with
        scopes =
          List.map input.scopes ~f:(fun scope ->
            if S.Id.equal (P.Scope.source scope) input.entry
            then changed_binding_scope
            else scope)
      }
    ; { input with
        sources =
          List.map input.sources ~f:(fun source ->
            if S.Id.equal (S.id source) (S.id changed_source)
            then changed_source
            else source)
      }
    ; { input with
        definitions =
          [ P.Definition.create
              ~implementation:(P.Definition.implementation definition)
              ~interface:None
              ~declared_at:(P.Definition.declared_at definition)
            |> Or_error.ok_exn
          ; List.nth_exn input.definitions 1
          ]
      }
    ]
  in
  List.iter modified ~f:(fun modified ->
    let prepared = Input.admit modified |> admitted in
    assert (not (S.Digest.equal (P.fingerprint original) (P.fingerprint prepared))));
  (* Replacing configuration bytes also invalidates all old declaration locations. *)
  let stale_declaration = Model.capture ~path:"project.json" "declaratioN" in
  expect_rejected
    { input with
      sources =
        List.map input.sources ~f:(fun source ->
          if S.Id.equal (S.id source) (S.id stale_declaration)
          then stale_declaration
          else source)
    }
;;

let%test_unit "single-file input is the explicit one-source empty-scope case" =
  let contract = S.Digest.of_contents "single contract" ~checkpoint in
  let text = "let main x = x" in
  let prepared =
    P.single_file ~compiler_contract:contract ~source:text ~checkpoint () |> admitted
  in
  let explicit =
    Input.
      { entry = P.entry prepared
      ; sources = P.sources prepared
      ; definitions = []
      ; scopes = P.scopes prepared
      ; environment = P.Environment.implicit ~required_contract:contract
      ; provenance = Implicit
      ; compiler_contract = contract
      }
    |> Input.admit
    |> admitted
  in
  assert (S.Digest.equal (P.fingerprint prepared) (P.fingerprint explicit));
  [%test_eq: int] (P.Accounting.source_count (P.accounting prepared)) 1;
  [%test_eq: string]
    (S.contents (P.find_source prepared (P.entry prepared) |> Option.value_exn))
    text;
  assert (List.is_empty (P.definitions prepared));
  assert (List.is_empty (P.Scope.bindings (List.hd_exn (P.scopes prepared))))
;;

let%expect_test "scope aliases select one captured module without executing it" =
  let input = Input.fixture () in
  let prepared = Input.admit input |> admitted in
  let scope = P.find_scope prepared input.entry |> Option.value_exn in
  let graph = P.Scope.find scope ~name:"Graph" |> Option.value_exn in
  let again = P.Scope.find scope ~name:"Again" |> Option.value_exn in
  print_s
    [%sexp
      (List.map (P.Scope.bindings scope) ~f:(fun binding -> binding.P.Scope.name)
       : string list)];
  print_s [%sexp (P.Module_id.equal graph.module_id again.module_id : bool)];
  print_s [%sexp (P.Accounting.source_count (P.accounting prepared) : int)];
  [%expect
    {|
    (Again Graph)
    true
    5
    |}]
;;
