open! Core
module CM = Prompt.Chat_markdown
module D = CM.Document_occurrence
module P = CM.Document_projection
module I = Chatmd_parser_input

let with_parsed sources f =
  Eio_main.run (fun env ->
    let dir = Eio.Path.(env#fs / "tmp" / "chatmd-documents-captured-only") in
    let limits =
      CM.Limits.create
        ~max_input_bytes:65536
        ~max_parse_calls:64
        ~max_tokens:10000
        ~max_depth:32
      |> Or_error.ok_exn
    in
    let parse sources =
      CM.prepare_chat_inputs
        ~source:"root.chatmd"
        ~source_loader:(Source_loader.captured_filesystem ~root:dir ~sources)
        ~dir
        ~preprocess:Fn.id
        ~limits
        ~checkpoint:ignore
        (List.Assoc.find_exn sources "root.chatmd" ~equal:String.equal)
    in
    f (parse sources) parse)
;;

let materialize
      ?(checkpoint = ignore)
      ?(occurrences = 64)
      ?(bytes = 65536)
      ?(work = 100000)
      parsed
      rendered
  =
  CM.materialize_documents
    parsed
    ~rendered
    ~max_occurrences:occurrences
    ~max_output_bytes:bytes
    ~max_work_steps:work
    ~checkpoint
;;

let fixture =
  [ ( "root.chatmd"
    , {|<user ochat-source-context="pretend/external.chatmd">before<doc src="./guide.md" local capture/><agent src="child.chatmd" local><doc src="builtin:chatml"/></agent><doc src="https://example.invalid/dynamic"/></user><import src="nested/part.chatmd"/><import src="nested/part.chatmd"/><reasoning id="r"><doc src="builtin:chatml"/></reasoning>|}
    )
  ; ( "nested/part.chatmd"
    , {|<system><doc src="../other.md" local capture markdown/></system>|} )
  ]
;;

let rendered parsed =
  List.filter_map (CM.Parsed.document_occurrences parsed) ~f:(fun occurrence ->
    match D.selection occurrence with
    | Dynamic -> None
    | Local_capture | Builtin _ ->
      Some (occurrence, "literal:" ^ Int.to_string (D.ordinal occurrence)))
;;

let%expect_test "owned slots retain import bases and literal materialization" =
  with_parsed fixture (fun parsed _ ->
    List.iter (CM.Parsed.document_occurrences parsed) ~f:(fun occurrence ->
      let input = I.Range.input (D.range occurrence) in
      print_s
        [%sexp
          (D.ordinal occurrence : int)
        , (I.path input : string)
        , (D.selection occurrence : D.Selection.t)
        , (D.src occurrence : string)]);
    let projection = materialize parsed (rendered parsed) |> Or_error.ok_exn in
    (match
       List.hd_exn (P.elements projection), List.hd_exn (CM.Parsed.elements parsed)
     with
     | User current, User original ->
       assert (Option.equal String.equal current.source_context original.source_context);
       assert (
         Option.equal String.equal current.source_context (Some "pretend/external.chatmd"))
     | _ -> assert false);
    let rec print_items (values : CM.content_item list) =
      List.iter values ~f:(function
        | Basic value ->
          print_s
            [%sexp
              (value.text : string option)
            , (value.document_url : string option)
            , (value.is_local : bool)]
        | Agent value -> print_items value.items)
    in
    List.iter (P.elements projection) ~f:(function
      | User value | System value ->
        (match value.content with
         | Some (Items values) -> print_items values
         | Some (Text _) | None -> ())
      | _ -> ());
    assert (phys_equal (P.parsed projection) parsed));
  [%expect
    {|
    (0 root.chatmd Local_capture ./guide.md)
    (1 root.chatmd (Builtin chatml) builtin:chatml)
    (2 root.chatmd Dynamic https://example.invalid/dynamic)
    (3 nested/part.chatmd Local_capture ../other.md)
    (4 nested/part.chatmd Local_capture ../other.md)
    ((before) () false)
    ((literal:0) () false)
    ((literal:1) () false)
    (() (https://example.invalid/dynamic) false)
    ((literal:3) () false)
    ((literal:4) () false)
    |}]
;;

let%test_unit "projection exact limits, ownership, completeness and callback identity" =
  with_parsed fixture (fun parsed parse ->
    let outputs = rendered parsed in
    let projection = materialize parsed outputs |> Or_error.ok_exn in
    let bytes =
      List.sum (module Int) outputs ~f:(fun (_, value) -> String.length value)
    in
    let count = List.length (CM.Parsed.document_occurrences parsed) in
    let work = P.work_steps projection in
    assert (Result.is_ok (materialize ~bytes ~occurrences:count ~work parsed outputs));
    assert (Result.is_error (materialize ~bytes:(bytes - 1) parsed outputs));
    assert (Result.is_error (materialize ~occurrences:(count - 1) parsed outputs));
    assert (Result.is_error (materialize ~work:(work - 1) parsed outputs));
    assert (Result.is_error (materialize parsed (List.tl_exn outputs)));
    assert (Result.is_error (materialize parsed (List.hd_exn outputs :: outputs)));
    let foreign = parse fixture in
    assert (Result.is_error (materialize parsed (rendered foreign)));
    let dynamic =
      List.find_exn (CM.Parsed.document_occurrences parsed) ~f:(fun d ->
        D.Selection.equal (D.selection d) Dynamic)
    in
    assert (Result.is_error (materialize parsed ((dynamic, "bad") :: outputs)));
    let polls = ref 0 in
    ignore
      (materialize ~checkpoint:(fun () -> incr polls) parsed outputs |> Or_error.ok_exn
       : P.t);
    let exception Stop of int ref in
    for stop = 1 to !polls do
      let identity = ref stop
      and reached = ref 0 in
      match
        materialize
          ~checkpoint:(fun () ->
            incr reached;
            if !reached = stop then raise (Stop identity))
          parsed
          outputs
      with
      | exception Stop actual -> assert (phys_equal identity actual)
      | Ok _ | Error _ -> assert false
    done)
;;

let%test_unit "plain message items consume the same bounded projection work" =
  with_parsed
    [ "root.chatmd", "<user><doc src=\"builtin:chatml\"/></user>" ]
    (fun small parse ->
       let small_work =
         P.work_steps (materialize small (rendered small) |> Or_error.ok_exn)
       in
       let many =
         parse
           [ ( "root.chatmd"
             , "<user><doc src=\"builtin:chatml\"/>"
               ^ String.concat (List.init 100 ~f:(fun _ -> "<img src=\"x\"/>"))
               ^ "</user>" )
           ]
       in
       assert (Result.is_error (materialize ~work:small_work many (rendered many)));
       let large = materialize many (rendered many) |> Or_error.ok_exn in
       assert (P.work_steps large > small_work + 100))
;;

let%test_unit "portable doc member resolution uses actual declaring member" =
  let base = Source.Portable_path.create "nested/part.chatmd" |> Or_error.ok_exn in
  List.iter
    [ "./guide.md", "nested/guide.md"
    ; "../guide.md", "guide.md"
    ; "deep/../guide.md", "nested/guide.md"
    ]
    ~f:(fun (reference, expected) ->
      let actual =
        Source_loader.resolve_member
          base
          ~reference
          ~max_reference_bytes:1024
          ~checkpoint:ignore
        |> Or_error.ok_exn
        |> Source.Portable_path.to_string
      in
      assert (String.equal actual expected));
  List.iter
    [ "../../escape.md"
    ; "/absolute.md"
    ; "https://example.invalid/guide"
    ; "back\\slash.md"
    ; "nul\000.md"
    ; "line\n.md"
    ; ""
    ]
    ~f:(fun reference ->
      assert (
        Result.is_error
          (Source_loader.resolve_member
             base
             ~reference
             ~max_reference_bytes:1024
             ~checkpoint:ignore)));
  assert (
    Result.is_error
      (Source_loader.resolve_member
         base
         ~reference:"abc"
         ~max_reference_bytes:2
         ~checkpoint:ignore));
  let exception Stop of int ref in
  let identity = ref 0 in
  match
    Source_loader.resolve_member
      base
      ~reference:"guide.md"
      ~max_reference_bytes:1024
      ~checkpoint:(fun () -> raise (Stop identity))
  with
  | exception Stop actual -> assert (phys_equal actual identity)
  | Ok _ | Error _ -> assert false
;;

let%test_unit "parser-input v1 identity preserves independently encoded fixture" =
  let input =
    I.create
      ~role:I.Role.Root
      ~path:"root.chatmd"
      ~authored:"alpha\n"
      ~effective:"beta\n"
      ~max_input_bytes:64
      ~checkpoint:ignore
    |> Result.map_error ~f:(fun _ -> "invalid fixture")
    |> Result.ok_or_failwith
  in
  assert (
    String.equal
      (I.identity_digest input ~checkpoint:ignore)
      "fb49b15e97a46bd687baabfbea4510337285ea92ee3939889ed1a757e73a1b9f")
;;
