open Core
module C = Chatml_authoring_context
module S = Chatml_source
module P = Chatml_prepared_program
module B = Chatml_project_build

let ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (C.Error.sexp_of_t error)
;;

let identity nonce =
  C.Identity.create
    ~principal:"principal"
    ~host_incarnation:"host"
    ~connection_lease:"lease"
    ~nonce
  |> Or_error.ok_exn
;;

let revision number = C.Editor_revision.create number |> Or_error.ok_exn

let base () =
  let fixture = Fixtures.make () in
  fixture, Fixtures.compose fixture |> Support.ok
;;

let limits base ~contexts ~extra_bytes ~open_documents =
  let accounting = B.prepared base |> P.accounting in
  let source_bytes = P.Accounting.source_bytes accounting in
  let input_bytes = P.Accounting.input_bytes accounting in
  C.Limits.create
    ~max_contexts:contexts
    ~max_sources:(P.Accounting.source_count accounting)
    ~max_open_documents:open_documents
    ~max_document_bytes:1024
    ~max_effective_input_bytes:(input_bytes + extra_bytes)
    ~max_context_source_bytes:(source_bytes + extra_bytes)
    ~max_total_source_bytes:((source_bytes * contexts) + extra_bytes)
  |> Or_error.ok_exn
;;

let admit store ~identity ~base ~editable =
  C.admit store ~identity ~base ~editable_sources:editable ~checkpoint:ignore |> ok
;;

let version store identity source = C.version store ~identity ~source |> Option.value_exn

let contents store identity source =
  let inputs = C.inputs store ~identity ~checkpoint:ignore |> ok in
  C.Inputs.sources inputs
  |> List.find_exn ~f:(fun current -> S.Id.equal (S.id current) source)
  |> S.contents
;;

let expect_error actual ~is_expected =
  match actual with
  | Error error when is_expected error -> ()
  | Error error -> Error.raise_s [%sexp "unexpected", (error : C.Error.t)]
  | Ok _ -> failwith "invalid authoring operation admitted"
;;

let%test_unit "whole-context CAS invalidates another document's old token" =
  let fixture, base = base () in
  let id = identity "one" in
  let store =
    C.create ~limits:(limits base ~contexts:1 ~extra_bytes:128 ~open_documents:2)
    |> admit ~identity:id ~base ~editable:[ S.id fixture.main; S.id fixture.helper ]
  in
  let old_helper = version store id (S.id fixture.helper) in
  let main = version store id (S.id fixture.main) in
  let store =
    C.replace
      store
      ~expected:main
      ~next_revision:(revision 1)
      ~contents:"alpha"
      ~checkpoint:ignore
    |> ok
  in
  expect_error
    (C.replace
       store
       ~expected:old_helper
       ~next_revision:(revision 1)
       ~contents:"beta"
       ~checkpoint:ignore)
    ~is_expected:(function
      | C.Error.Stale_generation -> true
      | _ -> false);
  let helper = version store id (S.id fixture.helper) in
  let store =
    C.replace
      store
      ~expected:helper
      ~next_revision:(revision 1)
      ~contents:"beta"
      ~checkpoint:ignore
    |> ok
  in
  [%test_eq: string] (contents store id (S.id fixture.main)) "alpha";
  [%test_eq: string] (contents store id (S.id fixture.helper)) "beta";
  [%test_eq: int]
    (C.View.generation (C.find store ~identity:id |> Option.value_exn)
     |> C.Generation.to_int)
    2
;;

type command =
  | Edit of int * int * string
  | Close of int
[@@deriving sexp_of]

let%test_unit "generated edits follow independent revision and quota model" =
  let fixture, base = base () in
  let id = identity "model" in
  let sources = [| S.id fixture.main; S.id fixture.helper |] in
  let base_contents = [| S.contents fixture.main; S.contents fixture.helper |] in
  let accounting = B.prepared base |> P.accounting in
  let base_bytes = P.Accounting.source_bytes accounting in
  let base_input_bytes = P.Accounting.input_bytes accounting in
  let extra_bytes = 8 in
  let limits = limits base ~contexts:1 ~extra_bytes ~open_documents:2 in
  let command_generator =
    let open Quickcheck.Generator.Let_syntax in
    let edit =
      let%bind index = Int.gen_incl 0 1 in
      let%bind next = Int.gen_incl 1 6 in
      let%map text =
        Quickcheck.Generator.of_list [ ""; "a"; "bb"; "12345678"; "123456789" ]
      in
      Edit (index, next, text)
    in
    let close =
      Int.gen_incl 0 1 |> Quickcheck.Generator.map ~f:(fun index -> Close index)
    in
    Quickcheck.Generator.weighted_union [ 4., edit; 1., close ]
  in
  let commands = Quickcheck.Generator.list_with_length 32 command_generator in
  Quickcheck.test ~trials:100 commands ~sexp_of:[%sexp_of: command list] ~f:(fun steps ->
    let store =
      ref (C.create ~limits |> admit ~identity:id ~base ~editable:(Array.to_list sources))
    in
    let selected = Array.copy base_contents in
    let overlay = Array.create ~len:2 false in
    let last_revision = Array.create ~len:2 0 in
    let generation = ref 0 in
    let model_charge () =
      base_bytes
      + Array.foldi overlay ~init:0 ~f:(fun index added is_open ->
        added + if is_open then String.length selected.(index) else 0)
    in
    let model_effective () =
      base_input_bytes
      + Array.foldi overlay ~init:0 ~f:(fun index added is_open ->
        added
        +
        if is_open
        then String.length selected.(index) - String.length base_contents.(index)
        else 0)
    in
    List.iter steps ~f:(fun step ->
      let index =
        match step with
        | Edit (index, _, _) | Close index -> index
      in
      let before = !store in
      let token = version before id sources.(index) in
      let result =
        match step with
        | Edit (_, next, text) ->
          C.replace
            before
            ~expected:token
            ~next_revision:(revision next)
            ~contents:text
            ~checkpoint:ignore
        | Close _ -> C.close_document before ~expected:token
      in
      let rejection =
        match step with
        | Edit (_, next, text) ->
          if next <= last_revision.(index)
          then Some `Revision
          else (
            let new_charge =
              model_charge ()
              - (if overlay.(index) then String.length selected.(index) else 0)
              + String.length text
            in
            let new_effective =
              model_effective () - String.length selected.(index) + String.length text
            in
            if
              new_charge > base_bytes + extra_bytes
              || new_effective > base_input_bytes + extra_bytes
            then Some `Limit
            else None)
        | Close _ -> if overlay.(index) then None else Some `Closed
      in
      if Option.is_none rejection
      then (
        store := ok result;
        incr generation;
        match step with
        | Edit (_, next, text) ->
          selected.(index) <- text;
          overlay.(index) <- true;
          last_revision.(index) <- next
        | Close _ ->
          selected.(index) <- base_contents.(index);
          overlay.(index) <- false)
      else (
        (match rejection with
         | Some `Revision ->
           expect_error result ~is_expected:(function
             | C.Error.Revision_not_increasing -> true
             | _ -> false)
         | Some `Limit ->
           expect_error result ~is_expected:(function
             | C.Error.Limit_exceeded -> true
             | _ -> false)
         | Some `Closed ->
           expect_error result ~is_expected:(function
             | C.Error.Document_not_open -> true
             | _ -> false)
         | None -> assert false);
        assert (phys_equal before !store));
      let view = C.find !store ~identity:id |> Option.value_exn in
      [%test_eq: int] (C.View.generation view |> C.Generation.to_int) !generation;
      [%test_eq: int] (C.charged_source_bytes !store) (model_charge ());
      [%test_eq: int] (C.View.charged_source_bytes view) (model_charge ());
      [%test_eq: int] (C.View.open_document_count view) (Array.count overlay ~f:Fn.id);
      let inputs = C.inputs !store ~identity:id ~checkpoint:ignore |> ok in
      [%test_eq: int] (C.Inputs.effective_input_bytes inputs) (model_effective ());
      [%test_eq: int]
        (List.length (C.Inputs.changed_sources inputs))
        (Array.count overlay ~f:Fn.id);
      Array.iteri sources ~f:(fun index source ->
        [%test_eq: string] (contents !store id source) selected.(index);
        [%test_eq: bool]
          (C.Version.overlay_open (version !store id source))
          overlay.(index);
        [%test_eq: int option]
          (C.Version.editor_revision (version !store id source)
           |> Option.map ~f:C.Editor_revision.to_int)
          (if last_revision.(index) = 0 then None else Some last_revision.(index)))))
;;

let%test_unit "limits, capture polling and rejected updates preserve current store" =
  let fixture, base = base () in
  let id = identity "limits" in
  let accounting = B.prepared base |> P.accounting in
  let base_bytes = P.Accounting.source_bytes accounting in
  let limit = limits base ~contexts:1 ~extra_bytes:3 ~open_documents:1 in
  let empty = C.create ~limits:limit in
  let polls = ref 0 in
  let too_many =
    List.init (P.Accounting.source_count accounting + 1) ~f:(fun _ -> S.id fixture.main)
  in
  expect_error
    (C.admit empty ~identity:id ~base ~editable_sources:too_many ~checkpoint:(fun () ->
       incr polls))
    ~is_expected:(function
      | C.Error.Limit_exceeded -> true
      | _ -> false);
  [%test_eq: int] !polls (P.Accounting.source_count accounting);
  [%test_eq: int] (C.context_count empty) 0;
  [%test_eq: int] (C.charged_source_bytes empty) 0;
  let store = admit empty ~identity:id ~base ~editable:[ S.id fixture.main ] in
  let token = version store id (S.id fixture.main) in
  expect_error
    (C.replace
       store
       ~expected:token
       ~next_revision:(revision 1)
       ~contents:"abcd"
       ~checkpoint:ignore)
    ~is_expected:(function
      | C.Error.Limit_exceeded -> true
      | _ -> false);
  [%test_eq: int] (C.charged_source_bytes store) base_bytes;
  let exception Stop of unit ref in
  let marker = ref () in
  (match
     C.replace
       store
       ~expected:token
       ~next_revision:(revision 1)
       ~contents:"abc"
       ~checkpoint:(fun () -> raise (Stop marker))
   with
   | exception Stop actual -> assert (phys_equal actual marker)
   | _ -> failwith "checkpoint exception lost");
  [%test_eq: int]
    (C.View.generation (C.find store ~identity:id |> Option.value_exn)
     |> C.Generation.to_int)
    0;
  [%test_eq: int] (C.charged_source_bytes store) base_bytes
;;

let%test_unit "closing an overlay rejects an over-limit restored input atomically" =
  let fixture, base = base () in
  let id = identity "restore-limit" in
  let accounting = B.prepared base |> P.accounting in
  let source_bytes = P.Accounting.source_bytes accounting in
  let input_bytes = P.Accounting.input_bytes accounting in
  let limits =
    C.Limits.create
      ~max_contexts:1
      ~max_sources:(P.Accounting.source_count accounting)
      ~max_open_documents:2
      ~max_document_bytes:1024
      ~max_effective_input_bytes:(input_bytes + 1)
      ~max_context_source_bytes:(source_bytes + 100)
      ~max_total_source_bytes:(source_bytes + 100)
    |> Or_error.ok_exn
  in
  let store =
    C.create ~limits
    |> admit ~identity:id ~base ~editable:[ S.id fixture.main; S.id fixture.helper ]
  in
  let store =
    C.replace
      store
      ~expected:(version store id (S.id fixture.main))
      ~next_revision:(revision 1)
      ~contents:""
      ~checkpoint:ignore
    |> ok
  in
  let helper_contents = String.make (String.length (S.contents fixture.helper) + 5) 'x' in
  let store =
    C.replace
      store
      ~expected:(version store id (S.id fixture.helper))
      ~next_revision:(revision 1)
      ~contents:helper_contents
      ~checkpoint:ignore
    |> ok
  in
  let before = C.find store ~identity:id |> Option.value_exn in
  expect_error
    (C.close_document store ~expected:(version store id (S.id fixture.main)))
    ~is_expected:(function
      | C.Error.Limit_exceeded -> true
      | _ -> false);
  let after = C.find store ~identity:id |> Option.value_exn in
  [%test_eq: int]
    (C.View.generation after |> C.Generation.to_int)
    (C.View.generation before |> C.Generation.to_int);
  [%test_eq: int] (C.View.open_document_count after) 2;
  [%test_eq: string] (contents store id (S.id fixture.main)) "";
  [%test_eq: string] (contents store id (S.id fixture.helper)) helper_contents
;;

let%test_unit "admission rejects lock, unknown and duplicate editable IDs" =
  let fixture, base = base () in
  let id = identity "editable" in
  let empty =
    C.create ~limits:(limits base ~contexts:1 ~extra_bytes:32 ~open_documents:1)
  in
  let admit_ids ids =
    C.admit empty ~identity:id ~base ~editable_sources:ids ~checkpoint:ignore
  in
  (match admit_ids [ S.id (B.lock_source base) ] with
   | Error (C.Error.Read_only_source source) ->
     assert (S.Id.equal source (S.id (B.lock_source base)))
   | _ -> failwith "generated lock edit admitted");
  let missing = Support.id ~namespace:"root" "missing.chatml" in
  (match admit_ids [ missing ] with
   | Error (C.Error.Unknown_source source) -> assert (S.Id.equal source missing)
   | _ -> failwith "unknown editable ID admitted");
  (match admit_ids [ S.id fixture.main; S.id fixture.main ] with
   | Error (C.Error.Duplicate_editable_source source) ->
     assert (S.Id.equal source (S.id fixture.main))
   | _ -> failwith "duplicate editable ID admitted");
  [%test_eq: int] (C.context_count empty) 0
;;

let%test_unit "aggregate ledger conserves source charge across contexts and close" =
  let fixture, base = base () in
  let first = identity "ledger-first" in
  let second = identity "ledger-second" in
  let base_bytes = B.prepared base |> P.accounting |> P.Accounting.source_bytes in
  let store =
    C.create ~limits:(limits base ~contexts:2 ~extra_bytes:3 ~open_documents:1)
    |> admit ~identity:first ~base ~editable:[ S.id fixture.main ]
    |> admit ~identity:second ~base ~editable:[ S.id fixture.main ]
  in
  [%test_eq: int] (C.context_count store) 2;
  [%test_eq: int] (C.charged_source_bytes store) (base_bytes * 2);
  let first_token = version store first (S.id fixture.main) in
  let second_token = version store second (S.id fixture.main) in
  let store =
    C.replace
      store
      ~expected:first_token
      ~next_revision:(revision 1)
      ~contents:"abc"
      ~checkpoint:ignore
    |> ok
  in
  [%test_eq: int] (C.charged_source_bytes store) ((base_bytes * 2) + 3);
  expect_error
    (C.replace
       store
       ~expected:second_token
       ~next_revision:(revision 1)
       ~contents:"x"
       ~checkpoint:ignore)
    ~is_expected:(function
      | C.Error.Limit_exceeded -> true
      | _ -> false);
  [%test_eq: int] (C.charged_source_bytes store) ((base_bytes * 2) + 3);
  let store = C.close_context store ~identity:first |> ok in
  [%test_eq: int] (C.context_count store) 1;
  [%test_eq: int] (C.charged_source_bytes store) base_bytes;
  expect_error
    (C.replace
       store
       ~expected:first_token
       ~next_revision:(revision 2)
       ~contents:"x"
       ~checkpoint:ignore)
    ~is_expected:(function
      | C.Error.Unknown_context -> true
      | _ -> false);
  let store =
    C.replace
      store
      ~expected:second_token
      ~next_revision:(revision 1)
      ~contents:"x"
      ~checkpoint:ignore
    |> ok
  in
  [%test_eq: int] (C.charged_source_bytes store) (base_bytes + 1)
;;

let%test_unit "refresh replaces identity and analysis key only after explicit discard" =
  let fixture, base = base () in
  let id = identity "first" in
  let store =
    C.create ~limits:(limits base ~contexts:1 ~extra_bytes:128 ~open_documents:1)
    |> admit ~identity:id ~base ~editable:[ S.id fixture.main ]
  in
  let graph = B.prepared base |> P.fingerprint in
  let store =
    C.note_analysis
      store
      ~identity:id
      ~expected:(C.View.generation (C.find store ~identity:id |> Option.value_exn))
      ~validated_graph:graph
    |> ok
  in
  assert (Option.is_some (C.analysis_key store ~identity:id));
  let token = version store id (S.id fixture.main) in
  let store =
    C.replace
      store
      ~expected:token
      ~next_revision:(revision 1)
      ~contents:"edited"
      ~checkpoint:ignore
    |> ok
  in
  assert (Option.is_none (C.analysis_key store ~identity:id));
  let current = C.View.generation (C.find store ~identity:id |> Option.value_exn) in
  expect_error
    (C.refresh
       store
       ~identity:id
       ~expected:current
       ~new_identity:(identity "second")
       ~base
       ~editable_sources:[ S.id fixture.main ]
       ~policy:Reject_open_documents
       ~checkpoint:ignore)
    ~is_expected:(function
      | C.Error.Open_documents -> true
      | _ -> false);
  let refreshed =
    C.refresh
      store
      ~identity:id
      ~expected:current
      ~new_identity:(identity "second")
      ~base
      ~editable_sources:[ S.id fixture.main ]
      ~policy:Discard_open_documents
      ~checkpoint:ignore
    |> ok
  in
  assert (Option.is_none (C.find refreshed ~identity:id));
  let view = C.find refreshed ~identity:(identity "second") |> Option.value_exn in
  [%test_eq: int] (C.View.generation view |> C.Generation.to_int) 0;
  [%test_eq: string]
    (contents refreshed (identity "second") (S.id fixture.main))
    (S.contents fixture.main);
  assert (Option.is_none (C.analysis_key refreshed ~identity:(identity "second")))
;;

let%expect_test "one overlay lifecycle" =
  let fixture, base = base () in
  let id = identity "expect" in
  let store =
    C.create ~limits:(limits base ~contexts:1 ~extra_bytes:128 ~open_documents:1)
    |> admit ~identity:id ~base ~editable:[ S.id fixture.main ]
  in
  let show store =
    let view = C.find store ~identity:id |> Option.value_exn in
    print_s
      [%sexp
        { generation = (C.View.generation view |> C.Generation.to_int : int)
        ; open_documents = (C.View.open_document_count view : int)
        ; contents = (contents store id (S.id fixture.main) : string)
        }]
  in
  show store;
  let store =
    C.replace
      store
      ~expected:(version store id (S.id fixture.main))
      ~next_revision:(revision 7)
      ~contents:"scratch"
      ~checkpoint:ignore
    |> ok
  in
  show store;
  let store =
    C.close_document store ~expected:(version store id (S.id fixture.main)) |> ok
  in
  show store;
  [%expect
    {|
    ((generation 0) (open_documents 0) (contents "let main = 0"))
    ((generation 1) (open_documents 1) (contents scratch))
    ((generation 2) (open_documents 0) (contents "let main = 0"))
    |}]
;;
