open! Core
module CM = Prompt.Chat_markdown
module E = CM.Declaration.Extraction
module Input = Chatmd_parser_input
module Source = Chatml_source

let parse
      ?(bytes = 1024 * 1024)
      ?(tokens = 100_000)
      ?(checkpoint = ignore)
      ?(preprocess = Fn.id)
      sources
  =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let root = List.Assoc.find_exn sources "root.chatmd" ~equal:String.equal in
    let loader = Source_loader.captured_filesystem ~root:dir ~sources in
    let limits =
      CM.Limits.create
        ~max_input_bytes:bytes
        ~max_parse_calls:1024
        ~max_tokens:tokens
        ~max_depth:128
      |> Or_error.ok_exn
    in
    CM.prepare_chat_inputs
      ~source:"root.chatmd"
      ~source_loader:loader
      ~dir
      ~preprocess
      ~limits
      ~checkpoint
      root)
;;

let declaration parsed id =
  List.find_exn (CM.Parsed.declarations parsed) ~f:(fun item ->
    String.equal (CM.Declaration.script_id item) id)
;;

let compiled ?(namespace = "test") ?(path = "script.chatml") body =
  let id =
    Source.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
  in
  Source.create id ~contents:body ~expected_digest:None ~max_bytes:None ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let bound declaration =
  let body = CM.Declaration.script_body declaration in
  match E.bind declaration ~compiled_source:(compiled body) ~checkpoint:ignore with
  | Ok bound -> bound
  | Error _ -> assert false
;;

let project declaration bound ~start_byte ~end_byte =
  let source = compiled (CM.Declaration.script_body declaration) in
  let location =
    Source.locate source ~start_byte ~end_byte ~checkpoint:ignore |> Or_error.ok_exn
  in
  match E.project_raw bound location ~checkpoint:ignore with
  | Ok projection -> projection
  | Error _ -> assert false
;;

let assert_exact projection input expected_start expected_end =
  match projection with
  | E.Exact_inline span ->
    assert (phys_equal (E.input span) input);
    assert (Int.equal (E.start_byte span) expected_start);
    assert (Int.equal (E.end_byte span) expected_end)
  | E.External_origin _ | E.Approximate_declaration _ -> assert false
;;

let assert_approximate projection declaration =
  match projection with
  | E.Approximate_declaration range ->
    assert (
      phys_equal
        (Input.Range.input range)
        (Input.Range.input (CM.Declaration.range declaration)));
    assert (
      Int.equal
        (Input.Range.start_byte range)
        (Input.Range.start_byte (CM.Declaration.range declaration)))
  | E.Exact_inline _ | E.External_origin _ -> assert false
;;

let inverse bound input authored_byte =
  match E.authored_point bound ~input ~authored_byte ~checkpoint:ignore with
  | Ok point -> point
  | Error _ -> assert false
;;

let assert_inverse_exact bound input authored_byte expected_compiled =
  match inverse bound input authored_byte with
  | E.Authored_point.Exact_compiled_byte actual ->
    assert (Int.equal actual expected_compiled)
  | Unavailable _ -> assert false
;;

let assert_inverse_unavailable bound input authored_byte expected =
  match inverse bound input authored_byte with
  | E.Authored_point.Unavailable actual ->
    assert (
      Sexp.equal
        (E.Authored_point.sexp_of_unavailable actual)
        (E.Authored_point.sexp_of_unavailable expected))
  | Exact_compiled_byte actual ->
    failwithf "expected unavailable authored point, got compiled %d" actual ()
;;

let%test_unit "inline identity, Unicode bytes, CRLF and ambiguous RAW gap" =
  let body = "é\r\nleft RAW|right|RAW" in
  let root = "<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ body ^ "</script>" in
  let parsed = parse [ "root.chatmd", root ] in
  let declaration = declaration parsed "s" in
  let input = CM.Parsed.root_input parsed in
  assert (String.equal (CM.Declaration.script_body declaration) "é\r\nleft right");
  let bound = bound declaration in
  let content_start = String.substr_index_exn root ~pattern:body in
  assert_exact
    (project declaration bound ~start_byte:0 ~end_byte:2)
    input
    content_start
    (content_start + 2);
  assert_exact
    (project declaration bound ~start_byte:2 ~end_byte:4)
    input
    (content_start + 2)
    (content_start + 4);
  assert_exact
    (project declaration bound ~start_byte:4 ~end_byte:8)
    input
    (content_start + 4)
    (content_start + 8);
  assert_exact
    (project declaration bound ~start_byte:9 ~end_byte:14)
    input
    (content_start + 13)
    (content_start + 18);
  assert_approximate (project declaration bound ~start_byte:8 ~end_byte:10) declaration;
  assert_approximate (project declaration bound ~start_byte:9 ~end_byte:9) declaration;
  assert_exact
    (project declaration bound ~start_byte:14 ~end_byte:14)
    input
    (content_start + 18)
    (content_start + 18);
  assert_inverse_exact bound input (content_start + 1) 1;
  assert_inverse_exact bound input (content_start + 5) 5;
  assert_inverse_exact bound input (content_start + 8) 8;
  assert_inverse_unavailable
    bound
    input
    (content_start + 9)
    E.Authored_point.Unmapped_or_ambiguous;
  assert_inverse_unavailable
    bound
    input
    (content_start + 13)
    E.Authored_point.Unmapped_or_ambiguous;
  assert_inverse_exact bound input (content_start + 14) 10;
  assert_inverse_exact bound input (content_start + 18) 14
;;

let%test_unit "small generated plain/RAW sequences agree with authored-byte oracle" =
  let generator =
    Quickcheck.Generator.list_with_length
      4
      (Quickcheck.Generator.both
         Quickcheck.Generator.bool
         (Quickcheck.Generator.of_list [ "a"; "bb"; "é" ]))
  in
  Quickcheck.test ~trials:100 generator ~f:(fun parts ->
    let authored_rev, emitted_rev, origins_rev, _ =
      List.fold
        parts
        ~init:([], [], [], 0)
        ~f:(fun (authored_rev, emitted_rev, origins_rev, authored_cursor) (raw, text) ->
          let authored = if raw then "RAW|" ^ text ^ "|RAW" else text in
          let text_start = authored_cursor + if raw then 4 else 0 in
          let origins =
            List.init (String.length text) ~f:(fun index -> text_start + index)
          in
          ( authored :: authored_rev
          , text :: emitted_rev
          , List.rev_append origins origins_rev
          , authored_cursor + String.length authored ))
    in
    let authored_body = String.concat (List.rev authored_rev) in
    let expected_body = String.concat (List.rev emitted_rev) in
    let origins = Array.of_list (List.rev origins_rev) in
    let prefix = "<script id=\"s\" language=\"chatml\" kind=\"tool\">" in
    let root = prefix ^ authored_body ^ "</script>" in
    let parsed = parse [ "root.chatmd", root ] in
    let declaration = declaration parsed "s" in
    assert (String.equal (CM.Declaration.script_body declaration) expected_body);
    let input = CM.Parsed.root_input parsed in
    let source = compiled expected_body in
    let bound =
      match E.bind declaration ~compiled_source:source ~checkpoint:ignore with
      | Ok bound -> bound
      | Error _ -> assert false
    in
    let count = Array.length origins in
    for first = 0 to count do
      for last = first to count do
        let expected_origin =
          if first < last
          then (
            let start = origins.(first) in
            if
              List.for_all
                (List.init (last - first) ~f:Fn.id)
                ~f:(fun delta -> Int.equal origins.(first + delta) (start + delta))
            then Some (start, start + last - first)
            else None)
          else if Int.equal first 0
          then Some (origins.(0), origins.(0))
          else if Int.equal first count
          then Some (origins.(count - 1) + 1, origins.(count - 1) + 1)
          else if Int.equal (origins.(first - 1) + 1) origins.(first)
          then Some (origins.(first), origins.(first))
          else None
        in
        let location =
          Source.locate source ~start_byte:first ~end_byte:last ~checkpoint:ignore
          |> Or_error.ok_exn
        in
        let actual = E.project_raw bound location ~checkpoint:ignore in
        match expected_origin, actual with
        | Some (start_byte, end_byte), Ok (E.Exact_inline span) ->
          let start_byte = String.length prefix + start_byte in
          let end_byte = String.length prefix + end_byte in
          assert_exact (E.Exact_inline span) input start_byte end_byte;
          assert (
            String.equal
              (String.sub
                 (Input.authored input)
                 ~pos:start_byte
                 ~len:(end_byte - start_byte))
              (String.sub expected_body ~pos:first ~len:(last - first)))
        | None, Ok (E.Approximate_declaration _) -> ()
        | _ -> assert false
      done
    done;
    for authored = 0 to String.length authored_body do
      let matching_compiled =
        List.filter
          (List.init (count + 1) ~f:Fn.id)
          ~f:(fun compiled ->
            (Int.equal compiled 0 || Int.equal (origins.(compiled - 1) + 1) authored)
            && (Int.equal compiled count || Int.equal origins.(compiled) authored))
      in
      let actual = inverse bound input (String.length prefix + authored) in
      match matching_compiled, actual with
      | [ expected ], E.Authored_point.Exact_compiled_byte compiled ->
        assert (Int.equal expected compiled)
      | [], E.Authored_point.Unavailable Unmapped_or_ambiguous -> ()
      | _ -> assert false
    done)
;;

let%test_unit "repeated text and imported owner use parser identity, not substring search"
  =
  let body = "same same" in
  let imported =
    "<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ body ^ "</script>"
  in
  let parsed =
    parse
      [ ( "root.chatmd"
        , "<user>same</user><import src=\"part.chatmd\" \
           namespace=\"lib\"/><user>same</user>" )
      ; "part.chatmd", imported
      ]
  in
  let declaration = declaration parsed "lib:s" in
  let input = Input.Range.input (CM.Declaration.range declaration) in
  assert (String.equal (Input.path input) "part.chatmd");
  let content_start = String.substr_index_exn imported ~pattern:body in
  assert_exact
    (project declaration (bound declaration) ~start_byte:5 ~end_byte:9)
    input
    (content_start + 5)
    (content_start + 9)
;;

let%test_unit "generated child serialization and external src are not forged inline sites"
  =
  let nested =
    "<script id=\"n\" language=\"chatml\" kind=\"tool\">a<user>b</user>c</script>"
  in
  let external_script =
    "<script id=\"e\" language=\"chatml\" kind=\"tool\" src=\"code.chatml\"/>"
  in
  let parsed = parse [ "root.chatmd", nested ^ external_script; "code.chatml", "same" ] in
  let n = declaration parsed "n" in
  let n_bound = bound n in
  assert_exact
    (project n n_bound ~start_byte:0 ~end_byte:1)
    (CM.Parsed.root_input parsed)
    (String.substr_index_exn nested ~pattern:">a<" + 1)
    (String.substr_index_exn nested ~pattern:">a<" + 2);
  assert_approximate (project n n_bound ~start_byte:1 ~end_byte:2) n;
  let e = declaration parsed "e" in
  let e_bound = bound e in
  assert_inverse_unavailable
    e_bound
    (CM.Parsed.root_input parsed)
    0
    E.Authored_point.External_body;
  match project e e_bound ~start_byte:0 ~end_byte:4 with
  | E.External_origin { member; body_digest; start_byte; end_byte } ->
    assert (String.equal member "code.chatml");
    assert (Source.Digest.equal body_digest (Source.digest (compiled "same")));
    assert (Int.equal start_byte 0 && Int.equal end_byte 4)
  | E.Exact_inline _ | E.Approximate_declaration _ -> assert false
;;

let%test_unit "stale binding and foreign compiler location fail closed" =
  let parsed =
    parse
      [ "root.chatmd", "<script id=\"s\" language=\"chatml\" kind=\"tool\">x</script>" ]
  in
  let declaration = declaration parsed "s" in
  assert (
    Result.is_error
      (E.bind declaration ~compiled_source:(compiled "y") ~checkpoint:ignore));
  let bound = bound declaration in
  let foreign =
    Source.locate
      (compiled ~namespace:"foreign" "x")
      ~start_byte:0
      ~end_byte:1
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  assert (Result.is_error (E.project_raw bound foreign ~checkpoint:ignore))
;;

let%test_unit "transformed effective root cannot claim an authored exact site" =
  let authored = "<script id=\"s\" language=\"chatml\" kind=\"tool\">x</script>" in
  let effective = "<script id=\"s\" language=\"chatml\" kind=\"tool\">y</script>" in
  let parsed =
    parse
      ~preprocess:(fun bytes ->
        assert (String.equal bytes authored);
        effective)
      [ "root.chatmd", authored ]
  in
  let declaration = declaration parsed "s" in
  assert (String.equal (CM.Declaration.script_body declaration) "y");
  assert_approximate
    (project declaration (bound declaration) ~start_byte:0 ~end_byte:1)
    declaration;
  let input = CM.Parsed.root_input parsed in
  let content_start = String.substr_index_exn authored ~pattern:">x<" + 1 in
  assert_inverse_unavailable
    (bound declaration)
    input
    content_start
    E.Authored_point.Effective_only
;;

let%test_unit "one shared map byte cap degrades later declarations without changing body" =
  let body = String.make 120 'x' in
  let script =
    "<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ body ^ "</script>"
  in
  let parsed =
    parse
      ~bytes:180
      [ ( "root.chatmd"
        , "<import src=\"a.chatmd\" namespace=\"a\"/><import src=\"b.chatmd\" \
           namespace=\"b\"/>" )
      ; "a.chatmd", script
      ; "b.chatmd", script
      ]
  in
  let first = declaration parsed "a:s" in
  let second = declaration parsed "b:s" in
  assert (String.equal (CM.Declaration.script_body first) body);
  assert (String.equal (CM.Declaration.script_body second) body);
  let first_owner = Input.Range.input (CM.Declaration.range first) in
  let first_start = String.substr_index_exn script ~pattern:body in
  assert_exact
    (project first (bound first) ~start_byte:0 ~end_byte:1)
    first_owner
    first_start
    (first_start + 1);
  assert_approximate (project second (bound second) ~start_byte:0 ~end_byte:1) second
;;

exception Cancelled of int

let%test_unit "binding, projection and long RAW scans preserve cancellation identity" =
  let parsed =
    parse
      [ "root.chatmd", "<script id=\"s\" language=\"chatml\" kind=\"tool\">x</script>" ]
  in
  let declaration = declaration parsed "s" in
  let exception_value = Cancelled 42 in
  (match
     E.bind declaration ~compiled_source:(compiled "x") ~checkpoint:(fun () ->
       raise exception_value)
   with
   | _ -> assert false
   | exception actual -> assert (phys_equal actual exception_value));
  let source = compiled "x" in
  let location =
    Source.locate source ~start_byte:0 ~end_byte:1 ~checkpoint:ignore |> Or_error.ok_exn
  in
  (match
     E.project_raw (bound declaration) location ~checkpoint:(fun () ->
       raise exception_value)
   with
   | _ -> assert false
   | exception actual -> assert (phys_equal actual exception_value));
  let input = CM.Parsed.root_input parsed in
  let body_start = String.substr_index_exn (Input.authored input) ~pattern:">x<" + 1 in
  (match
     E.authored_point
       (bound declaration)
       ~input
       ~authored_byte:body_start
       ~checkpoint:(fun () -> raise exception_value)
   with
   | _ -> assert false
   | exception actual -> assert (phys_equal actual exception_value));
  let raw count =
    let n = ref 0 in
    let root =
      "<script id=\"s\" language=\"chatml\" kind=\"tool\">RAW|"
      ^ String.make count 'x'
      ^ "|RAW</script>"
    in
    ignore (parse ~checkpoint:(fun () -> incr n) [ "root.chatmd", root ] : CM.Parsed.t);
    !n
  in
  assert (raw 65_536 > raw 128 + 10)
;;
