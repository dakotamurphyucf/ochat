open Core
module C = Chatml_prompt_programs
module CM = Prompt.Chat_markdown
module Contract = Chatml.Chatml_compiler_contract
module B = Chatml_project_build
module S = Chatml_source

let ok = function
  | Ok value -> value
  | Error _ -> failwith "valid retained fixture rejected"
;;

let limits =
  C.Limits.create
    ~max_programs:8
    ~max_parser_inputs:16
    ~max_logical_sources:64
    ~max_total_input_bytes:(8 * 1024 * 1024)
    ~project_work_steps:1_000_000
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let parser_limits =
  CM.Limits.create
    ~max_input_bytes:1_000_000
    ~max_parse_calls:32
    ~max_tokens:10_000
    ~max_depth:32
  |> Or_error.ok_exn
;;

let parse dir root sources =
  CM.prepare_chat_inputs
    ~source:root
    ~source_loader:(Source_loader.captured_filesystem ~root:dir ~sources)
    ~dir
    ~preprocess:Fn.id
    ~limits:parser_limits
    ~checkpoint:ignore
    (List.Assoc.find_exn sources root ~equal:String.equal)
;;

let binding parsed id =
  CM.Parsed.declarations parsed
  |> List.find_exn ~f:(fun declaration ->
    String.equal (CM.Declaration.script_id declaration) id)
  |> C.Binding.of_declaration parsed ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let fixture dir =
  let body = "let main = 0" in
  let raw =
    sprintf
      {|<script id="main" language="chatml" kind="tool">%s</script>
<script id="provided" language="chatml" kind="tool">%s</script>|}
      body
      body
  in
  let sources = [ "root.chatmd", raw ] in
  let authored = Chatmd_source_bundle.create ~root_file:"root.chatmd" ~sources () |> ok in
  let parsed = parse dir "root.chatmd" sources in
  let first = binding parsed "main" in
  let provided = binding parsed "provided" in
  let original = Fixtures.make () in
  let build = Fixtures.compose original |> Support.ok in
  let lock = B.lock_source build in
  let original_bytes = "\n " ^ S.contents lock ^ " \n" in
  let formatted =
    S.create
      (S.id lock)
      ~contents:original_bytes
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let build =
    B.restore
      formatted
      ~sources:(Support.without_lock build)
      ~contract:(Fixtures.contract original)
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  let native =
    Contract.for_static_surface
      (Chatml.Chatml_static_surface.of_runtime_surface
         Chatml.Chatml_builtin_surface.empty
         ~checkpoint:ignore)
      ~required_bindings:[]
  in
  let native_build = Fixtures.compose (Fixtures.make ~contract:native ()) |> Support.ok in
  let digest = Contract.fingerprint (Fixtures.contract original) ~checkpoint:ignore in
  let native_digest = Contract.fingerprint native ~checkpoint:ignore in
  let custody =
    C.capture
      authored
      ~parsed_contexts:[ parsed ]
      ~root:(CM.Parsed.root_input parsed)
      ~programs:
        [ first, Selected build
        ; first, Selected native_build
        ; provided, Provided (B.prepared build)
        ]
      ~active_programs:[ first, digest ]
      ~limits
      ~checkpoint:ignore
    |> ok
    |> fun custody ->
    C.of_storage (C.to_storage custody) ~authored ~limits ~checkpoint:ignore |> ok
  in
  custody, first, digest, native_digest, formatted
;;

let project ?(max_work_steps = 100_000) ?(checkpoint = ignore) custody =
  C.retained custody ~max_work_steps ~checkpoint
;;

let key projection binding digest =
  C.Retained.programs projection
  |> List.find_exn ~f:(fun program ->
    C.Binding.equal binding (C.Retained.Program.binding program)
    && S.Digest.equal digest (C.Retained.Program.compiler_contract program))
  |> C.Retained.Program.key
;;

let lookup ?(max_work_steps = 100_000) ?(checkpoint = ignore) projection key =
  C.Retained.selected_lock projection ~key ~max_work_steps ~checkpoint
;;

let%expect_test "edited declaration keeps its slot while original lock stays exact" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let custody, old, digest, native_digest, original_lock = fixture dir in
    let projection = project custody |> ok in
    let edited =
      parse
        dir
        "root.chatmd"
        [ ( "root.chatmd"
          , "<!-- moved declaration -->\n\
             <script id=\"main\" language=\"chatml\" kind=\"tool\">let main = 99</script>"
          )
        ]
    in
    let current = binding edited "main" in
    assert (not (C.Binding.equal old current));
    assert (C.Binding.Slot.equal (C.Binding.slot old) (C.Binding.slot current));
    assert (
      C.Binding.Slot.matches
        (C.Binding.slot old)
        (C.Binding.slot current)
        ~checkpoint:ignore);
    assert (not (S.Digest.equal digest native_digest));
    [%test_eq: int] 3 (List.length (C.Retained.programs projection));
    let actual = lookup projection (key projection old digest) |> ok in
    Support.assert_same_source original_lock actual;
    print_endline "body, offset and compiler revisions are not the slot";
    print_endline
      "inactive contract variant remains distinct; selected lock keeps original \
       whitespace");
  [%expect
    {| 
    body, offset and compiler revisions are not the slot
    inactive contract variant remains distinct; selected lock keeps original whitespace |}]
;;

let%test_unit "missing foreign malformed and Provided keys never select a sibling" =
  Eio_main.run (fun env ->
    let custody, first, digest, _, _ = fixture (Eio.Stdenv.cwd env) in
    let projection = project custody |> ok in
    (match lookup projection "bad" with
     | Error Invalid_key -> ()
     | _ -> assert false);
    let foreign = String.make 64 '0' ^ ":" ^ String.make 64 'f' in
    (match lookup projection foreign with
     | Error (Missing_key actual) -> assert (String.equal actual foreign)
     | _ -> assert false);
    let provided =
      C.Retained.programs projection
      |> List.find_exn ~f:(fun program ->
        not (C.Binding.equal first (C.Retained.Program.binding program)))
    in
    (match lookup projection (C.Retained.Program.key provided) with
     | Error (Provided_key _) -> ()
     | _ -> assert false);
    assert (Option.is_some (Result.ok (lookup projection (key projection first digest)))))
;;

let assert_budget_and_identity f =
  let observed = ref 0 in
  f ~max_work_steps:100_000 ~checkpoint:(fun () -> incr observed) |> ok |> ignore;
  assert (!observed > 1);
  (match f ~max_work_steps:(!observed - 1) ~checkpoint:ignore with
   | Error C.Retained.Error.Work_limit_exceeded -> ()
   | _ -> assert false);
  f ~max_work_steps:!observed ~checkpoint:ignore |> ok |> ignore;
  let cancellation =
    match
      Eio.Cancel.sub (fun context ->
        Eio.Cancel.cancel context Exit;
        Eio.Fiber.check ())
    with
    | exception (Eio.Cancel.Cancelled _ as original) -> original
    | (exception _) | () -> assert false
  in
  List.iter [ Failure "retained callback origin"; cancellation ] ~f:(fun marker ->
    for stop = 1 to !observed do
      let count = ref 0 in
      match
        f ~max_work_steps:100_000 ~checkpoint:(fun () ->
          incr count;
          if !count = stop then raise marker)
      with
      | exception actual -> assert (phys_equal actual marker)
      | Ok _ | Error _ -> assert false
    done)
;;

let%test_unit "each complete projection and lookup obeys work and interruption" =
  Eio_main.run (fun env ->
    let custody, first, digest, _, _ = fixture (Eio.Stdenv.cwd env) in
    assert_budget_and_identity (fun ~max_work_steps ~checkpoint ->
      project custody ~max_work_steps ~checkpoint);
    let projection = project custody |> ok in
    let old_key = key projection first digest in
    assert_budget_and_identity (fun ~max_work_steps ~checkpoint ->
      lookup projection old_key ~max_work_steps ~checkpoint);
    project custody |> ok |> ignore)
;;

let%test_unit "qualified aliases and different registration Roots have separate slots" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let imported =
      {|<script id="handler" language="chatml" kind="tool">let main = 0</script>|}
    in
    let sources =
      [ ( "root.chatmd"
        , {|<import src="shared.chatmd" namespace="left"/><import src="shared.chatmd" namespace="right"/>|}
        )
      ; "shared.chatmd", imported
      ]
    in
    let parsed = parse dir "root.chatmd" sources in
    let left = binding parsed "left:handler"
    and right = binding parsed "right:handler" in
    assert (not (C.Binding.Slot.equal (C.Binding.slot left) (C.Binding.slot right)));
    let root_sources = [ "other.chatmd", imported ] in
    let other = binding (parse dir "other.chatmd" root_sources) "handler" in
    let same_decl =
      binding (parse dir "shared.chatmd" [ "shared.chatmd", imported ]) "handler"
    in
    assert (not (C.Binding.Slot.equal (C.Binding.slot other) (C.Binding.slot same_decl))))
;;
