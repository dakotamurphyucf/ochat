open Core
module A = Chatml_semantics
module H = Support

let failed contract input =
  match H.analyze contract input with
  | Ok (Failed partial) -> partial
  | _ -> failwith "expected syntax-only failed analysis"
;;

let paths partial =
  List.map (A.Partial.parsed partial) ~f:(fun row ->
    A.Captured.Id.relative_path (A.Captured.id (A.Partial.Syntax.source row)))
;;

let%test_unit "same-run partial syntax reflects the actual failing compiler phase" =
  let contract = H.contract () in
  let input = H.input contract [ "entry.chatml", "let =", [] ] in
  let partial = failed contract input in
  assert (List.is_empty (A.Partial.parsed partial));
  (match A.Partial.invalidity partial with
   | Parse { phase = Implementation; _ } -> ()
   | _ -> failwith "parse phase");
  let input =
    H.input
      contract
      [ "entry.chatml", "let result = Catalog.value", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let value = true + 1", []
      ]
  in
  let partial = failed contract input in
  [%test_eq: string list] (paths partial) [ "entry.chatml"; "catalog.chatml" ];
  (match A.Partial.invalidity partial with
   | Implementation source ->
     [%test_eq: string] (A.Captured.Id.relative_path source) "catalog.chatml"
   | _ -> failwith "actual nested implementation cause");
  let diagnostic = List.hd_exn (A.Partial.diagnostics partial) in
  let location = Chatml_diagnostic.Site.location (Option.value_exn diagnostic.site) in
  assert (
    A.Captured.Id.equal
      (A.Captured.Location.source location)
      (A.Captured.id (H.source input "catalog.chatml")));
  let input =
    H.input
      contract
      ~interfaces:[ "catalog.chatml", "val value : bool", [] ]
      [ "entry.chatml", "let result = Catalog.value", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let value = 1", []
      ]
  in
  let partial = failed contract input in
  [%test_eq: string list]
    (paths partial)
    [ "entry.chatml"; "catalog.chatml"; "catalog.chatmli" ];
  (match A.Partial.invalidity partial with
   | Companion source ->
     [%test_eq: string] (A.Captured.Id.relative_path source) "catalog.chatmli"
   | _ -> failwith "companion cause");
  let input =
    H.input
      contract
      [ "entry.chatml", "let result = Catalog.value", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let value = Entry.result", [ "Entry", "entry.chatml" ]
      ]
  in
  let partial = failed contract input in
  (match A.Partial.invalidity partial with
   | Dependency -> ()
   | _ -> failwith "cycle must not be relabeled implementation");
  let contract =
    H.contract ~requirements:[ "main", Chatml.Chatml_builtin_spec.TInt ] ()
  in
  let input = H.input contract [ "entry.chatml", "let main = true", [] ] in
  let partial = failed contract input in
  (match A.Partial.invalidity partial with
   | Host_requirements -> ()
   | _ -> failwith "host contract phase");
  [%test_eq: string list] (paths partial) [ "entry.chatml" ]
;;

let%test_unit "global limits reject publication and interrupted analyses retain no output"
  =
  let contract =
    Chatml.Chatml_compiler_contract.for_static_surface
      (Chatml.Chatml_static_surface.of_runtime_surface
         Chatml.Chatml_builtin_surface.empty
         ~checkpoint:H.checkpoint)
      ~required_bindings:[]
  in
  let input =
    H.input
      contract
      [ "entry.chatml", "let identity x = x;; let value = identity(1)", [] ]
  in
  List.iter
    [ H.limits ~facts:1 ()
    ; H.limits ~edges:1 ()
    ; H.limits ~labels:1 ()
    ; H.limits ~work:1 ()
    ; H.limits ~input_bytes:1 ()
    ; H.limits ~nodes:1 ()
    ]
    ~f:(fun limits -> assert (Result.is_error (H.analyze ~limits contract input)));
  let actual = A.Prepared.Accounting.input_bytes (A.Prepared.accounting input) in
  assert (
    Result.is_ok (H.analyze ~limits:(H.limits ~input_bytes:actual ()) contract input));
  (match H.analyze ~limits:(H.limits ~input_bytes:(actual - 1) ()) contract input with
   | Error Input_limit -> ()
   | _ -> failwith "exact input boundary");
  let polls = ref 0 in
  assert (Result.is_ok (H.analyze ~checkpoint:(fun () -> incr polls) contract input));
  assert (!polls > 0);
  assert (Result.is_ok (H.analyze ~limits:(H.limits ~work:!polls ()) contract input));
  (match H.analyze ~limits:(H.limits ~work:(!polls - 1) ()) contract input with
   | Error Work_limit -> ()
   | _ -> failwith "final work boundary");
  let expected = Chatml_typechecker.Type_error "analysis callback origin" in
  for stop = 1 to !polls do
    let current = ref 0 in
    match
      H.analyze
        ~checkpoint:(fun () ->
          incr current;
          if Int.equal stop !current then raise expected)
        contract
        input
    with
    | _ -> failwith "cancelled analysis cannot publish"
    | exception actual -> assert (phys_equal actual expected)
  done;
  ignore (H.complete contract input : A.Snapshot.t)
;;
