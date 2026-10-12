open! Core

let%test_module "borrowed local document capture" =
  (module struct
    module Native = Chatmd_content_capture
    module C = Chatmd_prompt_content
    module CM = Prompt.Chat_markdown
    module Local = Chatml_local_source

    let ok = function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Native.Error.t)]
    ;;

    let limits ?(input = 10000) ?(output = 10000) ?(work = 1000000) ?(occurrences = 16) ()
      =
      C.Limits.create
        ~max_occurrences:occurrences
        ~max_parser_inputs:16
        ~max_input_bytes:input
        ~max_output_bytes:output
        ~max_work_steps:work
        ~max_nodes:10000
        ~max_depth:32
        ~max_payloads:16
        ~max_payload_bytes:10000
        ~document:Document_schema.Limits.default
      |> Or_error.ok_exn
    ;;

    let with_fixture f =
      Eio_main.run (fun env ->
        let bytes = Cstruct.create 16 in
        Eio.Flow.read_exact env#secure_random bytes;
        let suffix =
          Cstruct.to_string bytes
          |> String.to_list
          |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
          |> String.concat
        in
        let directory = Eio.Path.(env#fs / "/tmp" / ("och221-docs-" ^ suffix)) in
        Eio.Path.mkdir ~perm:0o700 directory;
        Fun.protect
          ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
          (fun () ->
             Eio.Path.with_open_dir directory (fun root ->
               let root_text =
                 {|<user source="forged/elsewhere.chatmd"><doc src="./guide.md" local capture/><doc src="https://example.invalid/live"/></user><import src="nested/import.chatmd"/>|}
               in
               let imported = {|<user><doc src="../guide.md" local capture/></user>|} in
               Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "nested");
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(root / "guide.md")
                 "frozen";
               let sources =
                 [ "root.chatmd", root_text; "nested/import.chatmd", imported ]
               in
               let parsed =
                 CM.prepare_chat_inputs
                   ~source:"root.chatmd"
                   ~source_loader:
                     (Source_loader.captured_filesystem
                        ~root:(root :> Eio.Fs.dir_ty Eio.Path.t)
                        ~sources)
                   ~dir:(root :> Eio.Fs.dir_ty Eio.Path.t)
                   ~preprocess:Fn.id
                   ~limits:
                     (CM.Limits.create
                        ~max_input_bytes:10000
                        ~max_parse_calls:16
                        ~max_tokens:10000
                        ~max_depth:32
                      |> Or_error.ok_exn)
                   ~checkpoint:ignore
                   root_text
               in
               Eio.Switch.run (fun sw ->
                 let owner =
                   Local.open_root root ~sw ~max_path_components:16 ~checkpoint:ignore
                   |> Result.map_error ~f:(fun _ -> "fixture local owner")
                   |> Result.ok_or_failwith
                 in
                 f root owner parsed))))
    ;;

    let run ?(bounds = limits ()) ?(checkpoint = ignore) owner parsed =
      Native.run owner ~parsed_contexts:[ parsed ] ~limits:bounds ~checkpoint
    ;;

    let%expect_test
        "actual imported bases freeze literal output and leave dynamic slots live"
      =
      with_fixture (fun root owner parsed ->
        let result = run owner parsed |> ok in
        let content = Native.content result in
        let receipt = Native.accounting result in
        assert (Native.Accounting.occurrences receipt = 3);
        assert (Native.Accounting.input_bytes receipt = 12);
        assert (Native.Accounting.output_bytes receipt = 12);
        assert (
          List.length (Agent_store.Prompt_program_bundle.payloads (C.to_storage content))
          = 1);
        Eio.Path.unlink Eio.Path.(root / "guide.md");
        let restored =
          C.of_storage
            (C.to_storage content)
            ~inputs:(CM.Parsed.inputs parsed)
            ~limits:(limits ())
            ~checkpoint:ignore
          |> Result.map_error ~f:(fun _ -> "restore")
          |> Result.ok_or_failwith
        in
        let materialized =
          C.materialize restored ~parsed ~limits:(limits ()) ~checkpoint:ignore
          |> Result.map_error ~f:(fun _ -> "materialize")
          |> Result.ok_or_failwith
        in
        assert (
          phys_equal
            parsed
            (CM.Document_projection.parsed (C.Materialized.projection materialized)));
        print_s
          [%sexp
            { actual_slots = (Native.Accounting.occurrences receipt : int)
            ; observed_bytes = (Native.Accounting.input_bytes receipt : int)
            ; literal_bytes = (Native.Accounting.output_bytes receipt : int)
            ; physical_payloads = 1
            ; replay_without_document = true
            }]);
      [%expect
        {|
        ((actual_slots 3) (observed_bytes 12) (literal_bytes 12)
         (physical_payloads 1) (replay_without_document true))
        |}]
    ;;

    let%test_unit "logical bytes and stage work use one complete allowance" =
      with_fixture (fun _ owner parsed ->
        let actual = run owner parsed |> ok in
        let work = Native.Accounting.work_steps (Native.accounting actual) in
        assert (
          Result.is_ok
            (run
               ~bounds:(limits ~input:12 ~output:12 ~work ~occurrences:3 ())
               owner
               parsed));
        List.iter
          [ limits ~input:11 ()
          ; limits ~output:11 ()
          ; limits ~work:(work - 1) ()
          ; limits ~occurrences:2 ()
          ]
          ~f:(fun bounds -> assert (Result.is_error (run ~bounds owner parsed)));
        assert (Result.is_error (run ~bounds:(limits ~work:0 ()) owner parsed)))
    ;;

    let%test_unit "every borrowed checkpoint failure preserves identity and owner reuse" =
      with_fixture (fun _ owner parsed ->
        let total = ref 0 in
        ignore (run ~checkpoint:(fun () -> incr total) owner parsed |> ok : Native.t);
        for stop = 1 to !total do
          let marker = Failure "doc capture checkpoint" in
          let count = ref 0 in
          match
            run owner parsed ~checkpoint:(fun () ->
              incr count;
              if !count = stop then raise marker)
          with
          | exception exn -> assert (phys_equal exn marker)
          | _ -> failwith "borrowed checkpoint did not escape"
        done;
        assert (Result.is_ok (run owner parsed)))
    ;;
  end)
;;
