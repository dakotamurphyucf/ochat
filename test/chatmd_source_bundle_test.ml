open Core
module B = Chatmd_source_bundle
module CM = Prompt.Chat_markdown

let bundle ?limits ?(root_file = "root.chatmd") sources =
  B.create ?limits ~root_file ~sources () |> Result.ok_or_failwith
;;

let fails message f =
  match f () with
  | _ -> failwith ("expected failure containing " ^ message)
  | exception exn -> assert (String.is_substring (Exn.to_string exn) ~substring:message)
;;

let with_dir f =
  Eio_main.run (fun env ->
    let path = Core_unix.mkdtemp "/tmp/ochat-bundle.XXXXXX" in
    let dir = Eio.Path.(Eio.Stdenv.fs env / path) in
    Exn.protect
      ~f:(fun () -> f dir)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir))
;;

let%test_unit "generated bundles validate canonical paths and source budgets" =
  let valid = bundle [ "root.chatmd", "text" ] in
  assert (String.equal (B.root_file valid) "root.chatmd");
  List.iter
    [ "/absolute"; "../escape"; "./root"; "a//b"; "C:\\file"; "a\\b"; "a\000b"; "a\nb" ]
    ~f:(fun path ->
      assert (Result.is_error (B.create ~root_file:path ~sources:[ path, "" ] ())));
  List.iter
    [ [ "root.chatmd", "a"; "root.chatmd", "b" ]
    ; [ "root.chatmd", "a"; "ROOT.chatmd", "b" ]
    ; [ "root.chatmd", ""; "file", ""; "file/child", "" ]
    ; [ "root.chatmd", ""; "file", ""; "FILE/child", "" ]
    ]
    ~f:(fun sources ->
      assert (Result.is_error (B.create ~root_file:"root.chatmd" ~sources ())));
  assert (Result.is_error (B.create ~root_file:"missing" ~sources:[] ()));
  let limits = B.{ max_source_bytes = 4; max_bundle_bytes = 6; max_files = 2 } in
  ignore (bundle ~limits [ "root.chatmd", "1234"; "other", "12" ] : B.t);
  List.iter
    [ [ "root.chatmd", "12345" ]
    ; [ "root.chatmd", "1234"; "other", "123" ]
    ; [ "root.chatmd", ""; "a", ""; "b", "" ]
    ]
    ~f:(fun sources ->
      assert (Result.is_error (B.create ~limits ~root_file:"root.chatmd" ~sources ())));
  let same = bundle [ "other", "x"; "root.chatmd", "text" ] in
  let reordered = bundle [ "root.chatmd", "text"; "other", "x" ] in
  assert (String.equal (B.fingerprint same) (B.fingerprint reordered));
  assert (not (String.equal (B.fingerprint same) (B.fingerprint valid)))
;;

let%test_unit
    "generated parsing bypasses ambient preprocessing and rejects explicit directives"
  =
  with_dir (fun dir ->
    let previous = Sys.getenv "OCHAT_META_REFINE" in
    Exn.protect
      ~f:(fun () ->
        Core_unix.putenv ~key:"OCHAT_META_REFINE" ~data:"true";
        let result =
          CM.parse_source_bundle
            ~dir
            (bundle [ "root.chatmd", "<developer>unchanged bytes</developer>" ])
        in
        assert (List.length result.root = 1);
        (match List.hd_exn result.root with
         | CM.Developer { content = Some (Text text); _ } ->
           assert (String.equal text "unchanged bytes")
         | _ -> assert false);
        List.iter
          [ [ "root.chatmd", "<!-- META_REFINE --><developer>x</developer>" ]
          ; [ "root.chatmd", "<import src=\"import.chatmd\"/>"
            ; "import.chatmd", "<!-- META_REFINE -->"
            ]
          ; [ "root.chatmd", "<tool name=\"child\" agent=\"child.chatmd\" local/>"
            ; "child.chatmd", "<!-- META_REFINE -->"
            ]
          ]
          ~f:(fun sources ->
            fails "preprocessing" (fun () -> CM.parse_source_bundle ~dir (bundle sources))))
      ~finally:(fun () ->
        match previous with
        | Some value -> Core_unix.putenv ~key:"OCHAT_META_REFINE" ~data:value
        | None -> Core_unix.unsetenv "OCHAT_META_REFINE"))
;;

let%test_unit "bundles use supplied bytes and traverse the unique local-agent closure" =
  with_dir (fun dir ->
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(dir / "parts");
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(dir / "parts" / "worker.chatml")
      "live bytes must not be used";
    let sources =
      [ ( "root.chatmd"
        , {|<import src="parts/definitions.chatmd" namespace="lab"/><tool name="child" agent="agents/child.chatmd" local/>|}
        )
      ; ( "parts/definitions.chatmd"
        , {|<script id="worker" language="chatml" kind="tool" src="worker.chatml"/><tool name="work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
        )
      ; "parts/worker.chatml", "let run = fun ctx input -> Task.pure(`Complete(input))"
      ; "parts/schema.json", "true"
      ; ( "agents/child.chatmd"
        , {|<script id="child_script" language="chatml" kind="tool">let run = fun ctx input -> Task.pure(`Complete(input))</script><tool name="back" agent="../root.chatmd" local/>|}
        )
      ]
    in
    let result = CM.parse_source_bundle ~dir (bundle sources) in
    assert (List.length result.agents = 1);
    let path, child = List.hd_exn result.agents in
    assert (String.equal path "agents/child.chatmd");
    let script =
      List.find_map_exn child ~f:(function
        | CM.Extension_script script -> Some script
        | _ -> None)
    in
    assert (String.equal script.source_ref.file "agents/child.chatmd");
    assert (
      String.equal
        script.source_ref.source_dir
        (Eio.Path.native_exn Eio.Path.(dir / "agents")));
    let imported =
      List.find_map_exn result.root ~f:(function
        | CM.Extension_script script -> Some script
        | _ -> None)
    in
    assert (String.equal imported.id "lab:worker");
    assert (String.equal imported.source_ref.file "parts/definitions.chatmd");
    assert (
      String.is_substring
        (Chatmd_shell_spec.Extension_spec.script_text imported)
        ~substring:"Task.pure");
    fails "chatmd.extension_source_unavailable" (fun () ->
      CM.parse_source_bundle
        ~dir
        (bundle
           (List.filter sources ~f:(fun (path, _) ->
              not (String.equal path "parts/worker.chatml"))))))
;;

let%test_unit
    "generated source references cannot escape or use uncaptured agent definitions"
  =
  with_dir (fun dir ->
    List.iter
      [ "/tmp/outside.chatmd"
      ; "../outside.chatmd"
      ; "C:\\outside.chatmd"
      ; "missing.chatmd"
      ]
      ~f:(fun path ->
        fails "source" (fun () ->
          CM.parse_source_bundle
            ~dir
            (bundle
               [ "root.chatmd", sprintf "<tool name=\"child\" agent=\"%s\" local/>" path ])));
    fails "bundled local" (fun () ->
      CM.parse_source_bundle
        ~dir
        (bundle
           [ ( "root.chatmd"
             , {|<tool name="remote" agent="https://example.invalid/agent.chatmd"/>|} )
           ]));
    List.iter
      [ "/tmp/outside.chatmd"
      ; "../outside.chatmd"
      ; "C:\\outside.chatmd"
      ; "https://example.invalid/source"
      ]
      ~f:(fun path ->
        fails "source" (fun () ->
          CM.parse_source_bundle
            ~dir
            (bundle [ "root.chatmd", sprintf "<import src=\"%s\"/>" path ]))))
;;

let%test_unit "generated parsing bounds repeated imports bytes tokens and depth" =
  with_dir (fun dir ->
    let files =
      List.init 12 ~f:(fun i ->
        let body =
          if i = 0
          then "<developer>x</developer>"
          else
            sprintf
              "<import src=\"%d.chatmd\"/><import src=\"%d.chatmd\"/>"
              (i - 1)
              (i - 1)
        in
        sprintf "%d.chatmd" i, body)
    in
    fails "expansion limit" (fun () ->
      CM.parse_source_bundle ~dir (bundle ~root_file:"11.chatmd" files));
    fails "expansion limit" (fun () ->
      CM.parse_source_bundle
        ~dir
        (bundle
           [ ( "root.chatmd"
             , String.concat (List.init 70 ~f:(fun _ -> "<import src=\"large.chatmd\"/>"))
             )
           ; "large.chatmd", "<developer>" ^ String.make (128 * 1024) 'x' ^ "</developer>"
           ]));
    fails "nesting limit" (fun () ->
      CM.parse_source_bundle
        ~dir
        (bundle
           [ ( "root.chatmd"
             , String.concat (List.init 129 ~f:(fun _ -> "<user>"))
               ^ String.concat (List.init 129 ~f:(fun _ -> "</user>")) )
           ]));
    let limits = { B.default_limits with max_source_bytes = 1024 * 1024 } in
    fails "token limit" (fun () ->
      CM.parse_source_bundle
        ~dir
        (bundle
           ~limits
           [ "root.chatmd", String.concat (List.init 100_001 ~f:(fun _ -> "<config/>")) ]));
    fails "UTF-8" (fun () ->
      CM.parse_source_bundle ~dir (bundle [ "root.chatmd", "<user>\255</user>" ])))
;;

let%test_unit "generated inline imports retain their local-agent source directory" =
  with_dir (fun dir ->
    let result =
      CM.parse_source_bundle
        ~dir
        (bundle
           [ "root.chatmd", {|<developer><import src="parts/inline.chatmd"/></developer>|}
           ; ( "parts/inline.chatmd"
             , {|<agent src="child.chatmd" local><user>hello</user></agent>|} )
           ; "parts/child.chatmd", "<developer>nested agent</developer>"
           ])
    in
    assert (
      List.equal String.equal (List.map result.agents ~f:fst) [ "parts/child.chatmd" ]))
;;

let%test_unit "large generated documents preserve sibling order and fragmented text" =
  with_dir (fun dir ->
    let text =
      List.init 6000 ~f:(fun i -> sprintf "<user>%d</user>" i) |> String.concat
    in
    let parsed = CM.parse_source_bundle ~dir (bundle [ "root.chatmd", text ]) in
    assert (List.length parsed.root = 6000);
    List.iteri parsed.root ~f:(fun i -> function
      | CM.User { content = Some (Text text); _ } ->
        assert (String.equal text (Int.to_string i))
      | _ -> assert false);
    let text = List.init 4000 ~f:(fun i -> sprintf "a%d < b%d | " i i) |> String.concat in
    let parsed =
      CM.parse_source_bundle
        ~dir
        (bundle [ "root.chatmd", "<developer>" ^ text ^ "</developer>" ])
    in
    match parsed.root with
    | [ CM.Developer { content = Some (Text actual); _ } ] ->
      assert (String.equal actual text)
    | _ -> assert false)
;;

let%test_unit "generated markup depth includes imported ancestor chains" =
  with_dir (fun dir ->
    let wrap source =
      String.concat (List.init 65 ~f:(fun _ -> "<developer>"))
      ^ source
      ^ String.concat (List.init 65 ~f:(fun _ -> "</developer>"))
    in
    fails "expanded markup nesting limit" (fun () ->
      CM.parse_source_bundle
        ~dir
        (bundle
           [ "root.chatmd", wrap "<import src=\"nested.chatmd\"/>"
           ; "nested.chatmd", wrap "text"
           ])))
;;
