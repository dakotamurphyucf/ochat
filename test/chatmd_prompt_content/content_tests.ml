open! Core

let%test_module "bounded rendered document custody" =
  (module struct
    module C = Chatmd_prompt_content
    module CM = Prompt.Chat_markdown
    module O = CM.Document_occurrence
    module I = Chatmd_parser_input
    module A = C.Accounting
    module D = Document_schema
    module Storage = Agent_store.Prompt_program_bundle

    let ok result =
      match result with
      | Ok value -> value
      | Error error -> failwith (Sexp.to_string_hum (C.Error.sexp_of_t error))
    ;;

    let limits
          ?(occurrences = 32)
          ?(inputs = 32)
          ?(input = 100000)
          ?(output = 100000)
          ?(work = 10000000)
          ?(nodes = 100000)
          ?(payloads = 32)
          ?(physical = 100000)
          ?(document = D.Limits.default)
          ()
      =
      C.Limits.create
        ~max_occurrences:occurrences
        ~max_parser_inputs:inputs
        ~max_input_bytes:input
        ~max_output_bytes:output
        ~max_work_steps:work
        ~max_nodes:nodes
        ~max_depth:64
        ~max_payloads:payloads
        ~max_payload_bytes:physical
        ~document
      |> Or_error.ok_exn
    ;;

    let with_parser f =
      Eio_main.run (fun env ->
        let dir = Eio.Path.(env#fs / "tmp" / "captured-docs-pure") in
        let parser_limits =
          CM.Limits.create
            ~max_input_bytes:65536
            ~max_parse_calls:32
            ~max_tokens:10000
            ~max_depth:32
          |> Or_error.ok_exn
        in
        let parse ?(root = "root.chatmd") body =
          CM.prepare_chat_inputs
            ~source:root
            ~source_loader:
              (Source_loader.captured_filesystem ~root:dir ~sources:[ root, body ])
            ~dir
            ~preprocess:Fn.id
            ~limits:parser_limits
            ~checkpoint:ignore
            body
        in
        f parse)
    ;;

    let root_text =
      {|<user><doc src="./guide.md" local capture/><doc src="./same.md" local capture/><doc src="https://example.invalid/live"/><doc src="./live.md" local/></user>|}
    ;;

    let render ?(body = "frozen") parsed =
      List.filter_map (CM.Parsed.document_occurrences parsed) ~f:(fun occurrence ->
        let source =
          match O.selection occurrence with
          | Dynamic -> None
          | Builtin id ->
            let id = Builtin_instructions.Id.of_string id |> Or_error.ok_exn in
            Some
              (C.Rendered.Source.Builtin
                 (Builtin_instructions.find id
                  |> Result.map_error ~f:(fun _ -> "unknown fixture asset")
                  |> Result.ok_or_failwith))
          | Local_capture ->
            let base =
              Source.Portable_path.create (I.path (I.Range.input (O.range occurrence)))
              |> Or_error.ok_exn
            in
            let member =
              Source_loader.resolve_member
                base
                ~reference:(O.src occurrence)
                ~max_reference_bytes:1024
                ~checkpoint:ignore
              |> Or_error.ok_exn
            in
            Some (C.Rendered.Source.Local { member; contents = body })
        in
        Option.map source ~f:(fun source ->
          C.Rendered.create
            parsed
            ~occurrence
            ~source
            ~limits:(limits ())
            ~checkpoint:ignore
          |> ok))
    ;;

    let capture ?(bounds = limits ()) ?(checkpoint = ignore) contexts rendered =
      C.capture ~parsed_contexts:contexts ~rendered ~limits:bounds ~checkpoint
    ;;

    let materialize custody parsed =
      C.materialize custody ~parsed ~limits:(limits ()) ~checkpoint:ignore
      |> ok
      |> C.Materialized.projection
    ;;

    let inputs contexts = List.concat_map contexts ~f:CM.Parsed.inputs
    let storage custody = C.to_storage custody

    let metadata custody =
      storage custody |> Storage.manifest |> Storage.Manifest.metadata
    ;;

    let equal_output (left_text, left_url) (right_text, right_url) =
      Option.equal String.equal left_text right_text
      && Option.equal String.equal left_url right_url
    ;;

    let outputs projection =
      List.concat_map (CM.Document_projection.elements projection) ~f:(function
        | CM.User value | System value ->
          (match value.content with
           | Some (Items values) ->
             List.map values ~f:(function
               | Basic item -> item.text, item.document_url
               | Agent _ -> None, None)
           | Some (Text text) -> [ Some text, None ]
           | None -> [])
        | _ -> [])
    ;;

    let%expect_test "shared output freezes as literals while dynamic doc stays live" =
      with_parser (fun parse ->
        let parsed = parse root_text in
        let custody = capture [ parsed ] (render parsed) |> ok in
        assert (
          List.equal
            Bool.equal
            (List.map (CM.Parsed.document_occurrences parsed) ~f:O.local)
            [ true; true; false; true ]);
        print_s
          [%sexp
            (A.occurrences (C.accounting custody) : int)
          , (A.input_bytes (C.accounting custody) : int)
          , (A.output_bytes (C.accounting custody) : int)
          , (List.length (Storage.payloads (storage custody)) : int)];
        print_s
          [%sexp
            (outputs (materialize custody parsed) : (string option * string option) list)]);
      [%expect
        {|
        (4 12 12 1)
        (((frozen) ()) ((frozen) ()) (() (https://example.invalid/live))
         (() (./live.md)))
        |}]
    ;;

    let%test_unit "aggregate input/output/physical/work bounds count repeated rendering" =
      with_parser (fun parse ->
        List.iter
          [ root_text, "frozen"
          ; ( {|<user><doc src="a.html" local capture strip/><doc src="b.html" local capture strip/></user>|}
            , "<p>hello</p>" )
          ; ( {|<user><doc src="a.html" local capture markdown/><doc src="b.html" local capture markdown/></user>|}
            , "<p>hello</p>" )
          ]
          ~f:(fun (text, body) ->
            let parsed = parse text in
            let rendered = render ~body parsed in
            let custody = capture [ parsed ] rendered |> ok in
            let a = C.accounting custody in
            let expected_input =
              List.sum
                (module Int)
                rendered
                ~f:(fun r -> A.input_bytes (C.Rendered.accounting r))
            in
            let expected_output =
              List.sum
                (module Int)
                rendered
                ~f:(fun r -> String.length (C.Rendered.contents r))
            in
            assert (
              A.input_bytes a = expected_input && expected_input = 2 * String.length body);
            assert (A.output_bytes a = expected_output);
            assert (List.length (Storage.payloads (storage custody)) = 1);
            let physical = String.length (C.Rendered.contents (List.hd_exn rendered)) in
            let exact =
              limits
                ~input:expected_input
                ~output:expected_output
                ~work:(A.work_steps a)
                ~nodes:(A.nodes a)
                ~physical
                ~payloads:1
                ()
            in
            ignore (capture ~bounds:exact [ parsed ] rendered |> ok : C.t);
            List.iter
              [ limits ~input:(expected_input - 1) ()
              ; limits ~output:(expected_output - 1) ()
              ; limits ~work:(A.work_steps a - 1) ()
              ; limits ~physical:(physical - 1) ()
              ; limits ~payloads:0 ()
              ]
              ~f:(fun bounds ->
                assert (Result.is_error (capture ~bounds [ parsed ] rendered)));
            if A.nodes a > 0
            then
              assert (
                Result.is_error
                  (capture ~bounds:(limits ~nodes:(A.nodes a - 1) ()) [ parsed ] rendered))))
    ;;

    let replace json name value =
      match json with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (key, old) ->
             key, if String.equal key name then value else old))
      | _ -> assert false
    ;;

    let get json name =
      match D.Json.field json ~name with
      | Value value -> value
      | Absent | Null -> assert false
    ;;

    let modified custody f =
      let doc = metadata custody in
      let json = f (D.Document.json doc) in
      let metadata =
        D.Document.inspect ~limits:D.Limits.default json
        |> Result.map_error ~f:(fun _ -> "malformed fixture")
        |> Result.ok_or_failwith
      in
      Storage.create ~metadata ~payloads:(Storage.payloads (storage custody))
      |> Result.map_error ~f:(fun _ -> "invalid fixture storage")
      |> Result.ok_or_failwith
    ;;

    let change_doc custody f =
      modified custody (fun json ->
        let payload = get json "payload" in
        let docs =
          match get payload "documents" with
          | `Array docs -> docs
          | _ -> assert false
        in
        let docs =
          match docs with
          | first :: rest -> f first :: rest
          | [] -> assert false
        in
        replace json "payload" (replace payload "documents" (`Array docs)))
    ;;

    let%test_unit "complete inactive metadata and exact payloads reject tampering" =
      with_parser (fun parse ->
        let parsed = parse root_text in
        let child =
          parse ~root:"child.chatmd" {|<system><doc src="builtin:chatml"/></system>|}
        in
        let contexts = [ parsed; child ] in
        let custody = capture contexts (render parsed @ render child) |> ok in
        let restore store =
          C.of_storage
            store
            ~inputs:(inputs contexts)
            ~limits:(limits ())
            ~checkpoint:ignore
        in
        let restored = restore (storage custody) |> ok in
        assert (
          List.equal
            equal_output
            (outputs (materialize restored parsed))
            (outputs (materialize custody parsed)));
        List.iter
          [ (fun doc -> replace doc "ordinal" (`Number "999"))
          ; (fun doc -> replace doc "input" (`String (String.make 64 '0')))
          ; (fun doc -> replace doc "end" (`Number "999999"))
          ; (fun doc -> replace doc "local" `False)
          ]
          ~f:(fun f -> assert (Result.is_error (restore (change_doc custody f))));
        let source_member =
          change_doc custody (fun doc ->
            replace
              doc
              "source"
              (replace (get doc "source") "member" (`String "wrong.md")))
        in
        let forged = restore source_member |> ok in
        assert (
          Result.is_error
            (C.materialize forged ~parsed ~limits:(limits ()) ~checkpoint:ignore));
        let no_payloads =
          Storage.create ~metadata:(metadata custody) ~payloads:[]
          |> Result.map_error ~f:(fun _ -> "invalid empty payload fixture")
          |> Result.ok_or_failwith
        in
        assert (Result.is_error (restore no_payloads));
        let extra =
          Storage.Payload.create "unreferenced"
          |> Result.map_error ~f:(fun _ -> "fixture")
          |> Result.ok_or_failwith
        in
        let surplus =
          Storage.create
            ~metadata:(metadata custody)
            ~payloads:(extra :: Storage.payloads (storage custody))
          |> Result.map_error ~f:(fun _ -> "fixture")
          |> Result.ok_or_failwith
        in
        assert (Result.is_error (restore surplus)))
    ;;

    let%test_unit
        "unknown metadata survives extension and retained child literal selection"
      =
      with_parser (fun parse ->
        let parent = parse root_text in
        let original = capture [ parent ] (render parent) |> ok in
        let unknown =
          modified original (function
            | `Object fields -> `Object (("vendor", `Object [ "retain", `Null ]) :: fields)
            | _ -> assert false)
        in
        let previous =
          C.of_storage
            unknown
            ~inputs:(inputs [ parent ])
            ~limits:(limits ())
            ~checkpoint:ignore
          |> ok
        in
        let child =
          parse ~root:"child.chatmd" {|<system><doc src="builtin:chatml"/></system>|}
        in
        let incoming = capture [ parent; child ] (render parent @ render child) |> ok in
        let extended =
          C.adopt incoming ~previous ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        let complete_work = A.work_steps (C.accounting extended) in
        ignore
          (C.adopt
             incoming
             ~previous
             ~limits:(limits ~work:complete_work ())
             ~checkpoint:ignore
           |> ok
           : C.t);
        assert (
          Result.is_error
            (C.adopt
               incoming
               ~previous
               ~limits:(limits ~work:(complete_work - 1) ())
               ~checkpoint:ignore));
        assert (
          D.Json.equal
            (get (D.Document.json (metadata extended)) "vendor")
            (`Object [ "retain", `Null ]));
        let child_output = outputs (materialize extended child) in
        let asset =
          Builtin_instructions.Id.of_string "chatml"
          |> Or_error.ok_exn
          |> Builtin_instructions.find
          |> Result.map_error ~f:(fun _ -> "fixture")
          |> Result.ok_or_failwith
        in
        assert (
          List.equal
            equal_output
            child_output
            [ Some (Builtin_instructions.Asset.contents asset), None ]);
        assert (
          Result.is_error
            (C.adopt original ~previous:extended ~limits:(limits ()) ~checkpoint:ignore));
        let empty = parse ~root:"empty.chatmd" "<user>literal only</user>" in
        let empty_bounds =
          limits ~occurrences:0 ~payloads:0 ~physical:0 ~input:0 ~output:0 ()
        in
        let zero = capture ~bounds:empty_bounds [ empty ] [] |> ok in
        let zero =
          C.of_storage
            (storage zero)
            ~inputs:(inputs [ empty ])
            ~limits:empty_bounds
            ~checkpoint:ignore
          |> ok
        in
        ignore
          (C.materialize zero ~parsed:empty ~limits:empty_bounds ~checkpoint:ignore |> ok
           : C.Materialized.t))
    ;;

    let%test_unit "materialization receipt includes validation before tree projection" =
      with_parser (fun parse ->
        let parsed = parse root_text in
        let custody = capture [ parsed ] (render parsed) |> ok in
        let replayed = parse root_text in
        let result =
          C.materialize custody ~parsed:replayed ~limits:(limits ()) ~checkpoint:ignore
          |> ok
        in
        let projection = C.Materialized.projection result in
        assert (phys_equal (CM.Document_projection.parsed projection) replayed);
        let receipt = C.Materialized.accounting result in
        let complete_work = A.work_steps receipt in
        (* Independent fixture: two captured six-byte outputs, two Dynamic slots;
           one physical payload must not reduce selected logical output to six. *)
        assert (List.length (Storage.payloads (storage custody)) = 1);
        assert (A.occurrences receipt = 4);
        assert (A.output_bytes receipt = 12);
        assert (A.input_bytes receipt = 0);
        assert (A.nodes receipt = 0);
        assert (complete_work > CM.Document_projection.work_steps projection);
        let exact =
          C.materialize
            custody
            ~parsed:replayed
            ~limits:(limits ~occurrences:4 ~output:12 ~work:complete_work ())
            ~checkpoint:ignore
          |> ok
        in
        assert (A.work_steps (C.Materialized.accounting exact) = complete_work);
        List.iter
          [ limits ~occurrences:3 (); limits ~output:11 () ]
          ~f:(fun limits ->
            assert (
              Result.is_error
                (C.materialize custody ~parsed:replayed ~limits ~checkpoint:ignore)));
        assert (
          Result.is_error
            (C.materialize
               custody
               ~parsed:replayed
               ~limits:(limits ~work:(complete_work - 1) ())
               ~checkpoint:ignore)))
    ;;

    let%test_unit
        "content checkpoints preserve original exception at every successful stage poll"
      =
      with_parser (fun parse ->
        let parsed = parse root_text in
        let rendered = render parsed in
        let polls = ref 0 in
        ignore
          (capture ~checkpoint:(fun () -> incr polls) [ parsed ] rendered |> ok : C.t);
        let exception Stop of int ref in
        for stop = 1 to !polls do
          let identity = ref stop
          and count = ref 0 in
          match
            capture
              ~checkpoint:(fun () ->
                incr count;
                if !count = stop then raise (Stop identity))
              [ parsed ]
              rendered
          with
          | exception Stop actual -> assert (phys_equal identity actual)
          | Ok _ | Error _ -> assert false
        done)
    ;;
  end)
;;
