open Core
module L = Chatml.Chatml_lang
module Frame_env = Chatml.Frame_env
module Linked = Chatml_linked_program
module P = Chatml_prepared_program
module C = Chatml.Chatml_compiler_contract
module B = Chatml.Chatml_builtin_spec

let%test_unit "shared weak annotations settle before immutable slot lowering" =
  let contract = Support.native_contract () in
  let input =
    Support.input
      contract
      [ "entry.chatml", "let number = Catalog.read() + 1", [ "Catalog", "catalog.chatml" ]
      ; ( "catalog.chatml"
        , "let cells = [] let read () = let result = cells[0] in result"
        , [] )
      ]
  in
  let linked = Support.compiled contract input in
  let catalog =
    List.find_exn (Linked.units linked) ~f:(fun unit ->
      String.equal
        (Chatml_source.Id.relative_path
           (Chatml_source.id (Linked.Unit.implementation unit)))
        "catalog.chatml")
  in
  let read =
    List.find_exn (Linked.Unit.program catalog).stmts ~f:(fun stmt ->
      match stmt.value with
      | L.RSLet ({ value = Name "read"; _ }, _) -> true
      | _ -> false)
  in
  (match read.value with
   | L.RSLet
       ( _
       , { value =
             RELambda
               (_, _, { value = RELetBlock (_, [ Frame_env.Slot Frame_env.SInt ], _); _ })
         ; _
         } ) -> ()
   | _ -> failwith "Dependency was lowered before importer specialized its weak result");
  let bad =
    Support.input
      contract
      [ ( "entry.chatml"
        , "open Catalog let (|left, right|) = (|cells, cells|) let first = \
           Array.set(left, 0, 1) let second = Array.set(right, 0, true)"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "let cells = []", []
      ]
  in
  assert (
    Result.is_error
      (Linked.compile bad ~contract ~max_dependency_depth:16 ~checkpoint:(fun () -> ())))
;;

let%test_unit "companion schemes preserve universal and equality constraints" =
  let contract = Support.native_contract () in
  let make implementation interface entry =
    Support.input
      contract
      ~interfaces:[ "catalog.chatml", interface, [] ]
      [ "entry.chatml", entry, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", implementation, []
      ]
  in
  ignore
    (Support.compiled
       contract
       (make
          "let identity x = x"
          "val identity : fun('a) -> 'a"
          "let a = Catalog.identity(1) let b = Catalog.identity(true)")
     : Linked.t);
  assert (
    Result.is_error
      (Linked.compile
         (make
            "let identity x = 1"
            "val identity : fun('a) -> 'a"
            "let a = Catalog.identity(1)")
         ~contract
         ~max_dependency_depth:16
         ~checkpoint:(fun () -> ())));
  List.iter [ false; true ] ~f:(fun declared ->
    let interface =
      "val same : fun('a, 'a) -> bool" ^ if declared then " where equality('a)" else ""
    in
    let checked =
      Linked.compile
        (make
           "let same left right = left == right"
           interface
           "let a = Catalog.same(1, 1)")
        ~contract
        ~max_dependency_depth:16
        ~checkpoint:(fun () -> ())
    in
    [%test_eq: bool] (Result.is_ok checked) declared)
;;

let%test_unit
    "every observed compiler checkpoint preserves interruption identity and publishes no \
     result"
  =
  let contract =
    C.for_static_surface
      (Chatml.Chatml_static_surface.of_runtime_surface
         Chatml.Chatml_builtin_surface.empty
         ~checkpoint:(fun () -> ()))
      ~required_bindings:[ "answer", B.TInt ]
  in
  let input =
    Support.input
      contract
      ~interfaces:[ "catalog.chatml", "val number : int", [] ]
      [ "entry.chatml", "let answer = Catalog.number", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let number = 7", []
      ]
  in
  let polls = ref 0 in
  ignore
    (Linked.compile input ~contract ~max_dependency_depth:16 ~checkpoint:(fun () ->
       incr polls)
     |> Result.ok
     |> Option.value_exn
     : Linked.t);
  assert (!polls > 0);
  let exception Interrupt of int in
  for target = 1 to !polls do
    let called = ref 0 in
    let sentinel = Interrupt target in
    match
      Linked.compile input ~contract ~max_dependency_depth:16 ~checkpoint:(fun () ->
        incr called;
        if !called = target then raise sentinel)
    with
    | _ -> failwith "Interrupted compiler returned a partial artifact/diagnostic"
    | exception actual -> assert (phys_equal actual sentinel)
  done;
  ignore (Support.compiled contract input : Linked.t)
;;

let%test_unit "cross-engine host checking rejects before mutating borrowed weak exports" =
  let module Check = Chatml_configured_check in
  let module Session = Chatml_dependency_session in
  let module Lookup = Chatml_configured_lookup in
  let module Parse = Chatml.Chatml_parse in
  let checkpoint () = () in
  let contract = Support.native_contract () in
  let input = Support.input contract [ "entry.chatml", "let weak = []", [] ] in
  let a = Check.create_static (C.static_surface contract) ~checkpoint in
  let b = Check.create_static (C.static_surface contract) ~checkpoint in
  let checked =
    Session.run input ~max_dependency_depth:16 ~checkpoint ~check:(fun node ~require ->
      let open Result.Let_syntax in
      let source = Session.Unit.implementation node in
      let%bind parsed = Parse.parse_captured (Lookup.source source) ~checkpoint in
      let require ~source reference =
        Result.map (require ~source reference) ~f:Check.Checked_unit.exports
      in
      let%bind checked = Check.check_unit a ~source ~require parsed in
      assert (
        Result.is_error
          (Check.check_entrypoints
             b
             checked
             ~source
             ~require
             ~required_bindings:[ "weak", B.TArray TInt ]));
      (* Rejected engine B must not specialize engine A's original shared cell. *)
      let%map _ =
        Check.check_entrypoints
          a
          checked
          ~source
          ~require
          ~required_bindings:[ "weak", B.TArray TBool ]
      in
      checked)
  in
  assert (Result.is_ok checked)
;;

let%test_unit
    "incompatible local host contract points to the original implementation binder"
  =
  let contract = Support.native_contract ~requirements:[ "main", B.TInt ] () in
  let input = Support.input contract [ "entry.chatml", "let main = true", [] ] in
  match
    Linked.compile input ~contract ~max_dependency_depth:16 ~checkpoint:(fun () -> ())
  with
  | Ok _ -> failwith "Incompatible host binding accepted"
  | Error failure ->
    let diagnostic = Linked.Failure.diagnostic failure in
    [%test_eq: string] (Support.owner diagnostic) "entry.chatml";
    let site = Option.value_exn diagnostic.Chatml_diagnostic.site in
    [%test_eq: string]
      (Chatml_source.read
         (Chatml_diagnostic.Site.source site)
         (Chatml_diagnostic.Site.location site)
       |> Or_error.ok_exn)
      "main"
;;
