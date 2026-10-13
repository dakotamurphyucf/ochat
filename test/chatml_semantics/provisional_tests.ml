open Core
module A = Chatml_semantics
module H = Support
module S = Chatml_source
module P = Chatml_prepared_program
module Parse = Chatml.Chatml_parse
module C = Chatml_compilation
module Contract = Chatml.Chatml_compiler_contract

let id path =
  S.Id.create ~package_namespace:"semantic-query-test" ~relative_path:path
  |> Or_error.ok_exn
;;

let capture path contents =
  S.create
    (id path)
    ~contents
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:H.checkpoint
  |> Or_error.ok_exn
;;

let witness text cursor =
  let source = capture "prefix.chatml" text in
  match
    Parse.Top_level_prefix.find source ~cursor_byte:cursor ~checkpoint:H.checkpoint
  with
  | Error Invalid_cursor -> failwith "valid cursor rejected"
  | Ok None -> None
  | Ok (Some witness) -> Some (Parse.Top_level_prefix.end_byte witness)
;;

let%test_unit "only actual completed outer statements and separators witness a prefix" =
  let complete = "let x = 1;;" in
  [%test_eq: int option]
    (witness (complete ^ " let broken =") (String.length complete))
    (Some (String.length complete));
  [%test_eq: int option] (witness "let x = 1 +" (String.length "let x = 1 +")) None;
  [%test_eq: int option]
    (witness "module M = struct let x = 1" (String.length "module M = struct let x = 1"))
    None;
  [%test_eq: int option]
    (witness
       "let x = 1;; module M = struct let y ="
       (String.length "let x = 1;; module M = struct let y ="))
    (Some (String.length complete));
  [%test_eq: int option]
    (witness (complete ^ " (* unterminated") (String.length complete))
    (Some (String.length "let x = 1"));
  [%test_eq: int option]
    (witness complete (String.length complete))
    (Some (String.length complete));
  [%test_eq: int option]
    (witness "let x = 1\r\nlet y =" (String.length "let x = 1\r\nlet y ="))
    (Some (String.length "let x = 1"));
  let type_prefix = "type item = int;;" in
  [%test_eq: int option]
    (witness (type_prefix ^ " let y =") (String.length type_prefix))
    (Some (String.length type_prefix));
  let source = capture "prefix.chatml" complete in
  match
    Parse.Top_level_prefix.find
      source
      ~cursor_byte:(String.length complete + 1)
      ~checkpoint:H.checkpoint
  with
  | Error Invalid_cursor -> ()
  | _ -> failwith "out-of-range cursor accepted"
;;

let%test_unit "prefix parser preserves every checkpoint exception" =
  let source = capture "prefix.chatml" "let x = 1;; let unfinished =" in
  let polls = ref 0 in
  assert (
    Result.is_ok
      (Parse.Top_level_prefix.find
         source
         ~cursor_byte:(S.byte_length source)
         ~checkpoint:(fun () -> incr polls)));
  assert (!polls > 0);
  let expected = Failure "prefix callback identity" in
  for stop = 1 to !polls do
    let seen = ref 0 in
    match
      Parse.Top_level_prefix.find
        source
        ~cursor_byte:(S.byte_length source)
        ~checkpoint:(fun () ->
          incr seen;
          if Int.equal !seen stop then raise expected)
    with
    | _ -> failwith "callback exception became a parse result"
    | exception actual -> assert (phys_equal actual expected)
  done
;;

let provisional contract input path cursor =
  A.analyze_prefix
    input
    ~source:(id path)
    ~cursor_byte:cursor
    ~contract
    ~max_dependency_depth:16
    ~limits:(H.limits ())
    ~checkpoint:H.checkpoint
;;

let%test_unit
    "provisional prefix skips only final host requirement and stays source-bound"
  =
  let contract =
    H.contract ~requirements:[ "main", Chatml.Chatml_builtin_spec.TInt ] ()
  in
  let full = "let x = 1;; let broken =" in
  let prefix = "let x = 1;;" in
  let input = H.input contract [ "entry.chatml", full, [] ] in
  (match H.analyze contract input with
   | Ok (Failed partial) ->
     (match A.Partial.invalidity partial with
      | Parse { phase = Implementation; _ } -> ()
      | _ -> failwith "full parse did not own diagnostics")
   | _ -> failwith "invalid full document accepted");
  let result =
    match provisional contract input "entry.chatml" (String.length full) with
    | Ok (A.Prefix.Provisional result) -> result
    | _ -> failwith "completed prefix not provisional"
  in
  [%test_eq: int] (A.Provisional.end_byte result) (String.length prefix);
  assert (
    S.Digest.equal (A.Provisional.full_input_fingerprint result) (P.fingerprint input));
  assert (
    S.Digest.equal
      (A.Provisional.full_source_digest result)
      (S.digest (H.source input "entry.chatml")));
  let snapshot = A.Provisional.snapshot result in
  assert (
    not
      (S.Digest.equal
         (A.Identity.input_fingerprint (A.Snapshot.identity snapshot))
         (P.fingerprint input)));
  let captured =
    List.find_exn (A.Snapshot.sources snapshot) ~f:(fun source ->
      S.Id.equal (S.id source) (id "entry.chatml"))
  in
  [%test_eq: string] (S.contents captured) prefix;
  assert (S.Digest.equal (A.Provisional.prefix_source_digest result) (S.digest captured));
  let original_point =
    S.locate
      (H.source input "entry.chatml")
      ~start_byte:0
      ~end_byte:0
      ~checkpoint:H.checkpoint
    |> Or_error.ok_exn
  in
  (match A.Snapshot.point snapshot original_point with
   | Error Unknown_source -> ()
   | Error _ -> ()
   | Ok _ -> failwith "full revision point entered provisional snapshot");
  let prefix_point =
    S.locate captured ~start_byte:0 ~end_byte:0 ~checkpoint:H.checkpoint
    |> Or_error.ok_exn
  in
  assert (Result.is_ok (A.Snapshot.point snapshot prefix_point));
  let exact_prefix = H.input contract [ "entry.chatml", prefix, [] ] in
  match H.analyze contract exact_prefix with
  | Ok (Failed partial) ->
    (match A.Partial.invalidity partial with
     | Host_requirements -> ()
     | _ -> failwith "ordinary analysis skipped required main")
  | _ -> failwith "ordinary analysis admitted missing main"
;;

let%test_unit "failed provisional type, dependency and companion checks publish no facts" =
  let contract = H.contract () in
  let full = "let x = true + 1;; let broken =" in
  let input = H.input contract [ "entry.chatml", full, [] ] in
  (match provisional contract input "entry.chatml" (String.length full) with
   | Ok (Unavailable (Prefix_rejected (Implementation _))) -> ()
   | _ -> failwith "invalid prefix type exposed semantic facts");
  let full = "let result = Catalog.value;; let broken =" in
  let input =
    H.input
      contract
      [ "entry.chatml", full, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let value = true + 1", []
      ]
  in
  (match provisional contract input "entry.chatml" (String.length full) with
   | Ok (Unavailable (Prefix_rejected (Implementation source))) ->
     assert (S.Id.equal source (id "catalog.chatml"))
   | _ -> failwith "failed dependency exposed facts");
  let input =
    H.input
      contract
      ~interfaces:[ "catalog.chatml", "val value : bool", [] ]
      [ "entry.chatml", full, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let value = 1", []
      ]
  in
  (match provisional contract input "entry.chatml" (String.length full) with
   | Ok (Unavailable (Prefix_rejected (Companion source))) ->
     assert (S.Id.equal source (id "catalog.chatmli"))
   | _ -> failwith "failed companion exposed facts");
  (match
     provisional contract input "catalog.chatmli" (String.length "val value : bool")
   with
   | Ok (Unavailable Unsupported_source_phase) -> ()
   | _ -> failwith "incomplete companion certified a prefix");
  let unused = "let value = 1;; let unfinished =" in
  let input =
    H.input contract [ "entry.chatml", "let main = 1", []; "catalog.chatml", unused, [] ]
  in
  match provisional contract input "catalog.chatml" (String.length unused) with
  | Ok (Unavailable Unchecked_source) -> ()
  | _ -> failwith "unreached catalog source certified checked facts"
;;

let%test_unit "one work ledger and cancellation protect provisional publication" =
  let contract = H.contract () in
  let full = "let x = 1;; let broken =" in
  let input = H.input contract [ "entry.chatml", full, [] ] in
  let run ?(limits = H.limits ()) ~checkpoint () =
    A.analyze_prefix
      input
      ~source:(id "entry.chatml")
      ~cursor_byte:(String.length full)
      ~contract
      ~max_dependency_depth:16
      ~limits
      ~checkpoint
  in
  let polls = ref 0 in
  (match run ~checkpoint:(fun () -> incr polls) () with
   | Ok (Provisional _) -> ()
   | _ -> failwith "baseline prefix failed");
  assert (!polls > 0);
  assert (
    Result.is_ok (run ~limits:(H.limits ~work:!polls ()) ~checkpoint:H.checkpoint ()));
  (match run ~limits:(H.limits ~work:(!polls - 1) ()) ~checkpoint:H.checkpoint () with
   | Error Work_limit -> ()
   | _ -> failwith "work budget reset across prefix phases");
  let expected = Failure "provisional cancellation identity" in
  let seen = ref 0 in
  match
    run
      ~checkpoint:(fun () ->
        incr seen;
        if Int.equal !seen (!polls / 2) then raise expected)
      ()
  with
  | _ -> failwith "cancelled prefix published"
  | exception actual -> assert (phys_equal actual expected)
;;

let%test_unit
    "joined compiler adapter returns provisional facts without runtime admission"
  =
  Eio_main.run (fun env ->
    let contract = Contract.for_target One_off_v1 ~checkpoint:H.checkpoint in
    let full = "let x = 1;; let main input =" in
    let input = H.input contract [ "entry.chatml", full, [] ] in
    (match
       C.analyze_prepared_prefix
         input
         ~source:(id "entry.chatml")
         ~cursor_byte:(String.length full)
         ~semantic_limits:(H.limits ())
         ~max_dependency_depth:16
         ~env
         ~target:One_off_v1
         ()
     with
     | Ok (Provisional facts) ->
       [%test_eq: int] (A.Provisional.end_byte facts) (String.length "let x = 1;;");
       assert (List.is_empty (A.Snapshot.host_requirements (A.Provisional.snapshot facts)))
     | _ -> failwith "joined prefix unavailable");
    match
      C.analyze_prepared
        input
        ~semantic_limits:(H.limits ())
        ~max_dependency_depth:16
        ~env
        ~target:One_off_v1
        ()
    with
    | Ok (Failed _) -> ()
    | _ -> failwith "ordinary joined analysis accepted incomplete source")
;;

let%test_unit "foreign-source metadata rows consume bounded work and cancellation" =
  let contract = H.contract () in
  let full = "let x = 1;; let unfinished =" in
  let many =
    List.init 96 ~f:(fun index ->
      Printf.sprintf "catalog%03d.chatml" index, "let value = 1", [])
  in
  let input = H.input contract (("entry.chatml", full, []) :: many) in
  let run ?(limits = H.limits ()) ~checkpoint () =
    A.analyze_prefix
      input
      ~source:(id "entry.chatml")
      ~cursor_byte:(String.length full)
      ~contract
      ~max_dependency_depth:16
      ~limits
      ~checkpoint
  in
  let polls = ref 0 in
  (match run ~checkpoint:(fun () -> incr polls) () with
   | Ok (Provisional _) -> ()
   | _ -> failwith "many-row provisional baseline failed");
  assert (!polls > 96);
  (match run ~limits:(H.limits ~work:96 ()) ~checkpoint:H.checkpoint () with
   | Error Work_limit -> ()
   | _ -> failwith "metadata visit escaped shared work bound");
  let expected = Failure "metadata cancellation identity" in
  let seen = ref 0 in
  match
    run
      ~checkpoint:(fun () ->
        incr seen;
        if Int.equal !seen 96 then raise expected)
      ()
  with
  | _ -> failwith "metadata cancellation published facts"
  | exception actual -> assert (phys_equal actual expected)
;;
