open Core
module P = Chat_response.Authoring_policy
module C = Chat_response.Tool_capability
module M = Chatmd_shell_spec.Authoring_metadata
module S = Chatmd_shell_spec.Extension_spec

let digest = Chatmd_shell_spec.Source_ref.digest

let get = function
  | Ok x -> x
  | Error e -> raise_s [%sexp (e : P.error)]
;;

let cap_get = function
  | Ok x -> x
  | Error e -> raise_s [%sexp (e : C.error)]
;;

let expect code = function
  | Error (e : P.error) -> assert (String.equal e.code code)
  | Ok _ -> failwith ("expected " ^ code)
;;

let help =
  M.
    { version = 1
    ; package = "one-off"
    ; tasks = [ One_off_script ]
    ; topics = [ "chatml/basics" ]
    ; required_helpers = []
    }
;;

let catalog ?(identity = "corpus-v1") () =
  P.catalog
    ~identity:(digest identity)
    ~packages:[ help ]
    ~topics:
      [ "chatml/basics", [ M.One_off_script ]; "chatmd/children", [ M.Child_agent ] ]
  |> get
;;

let native name calls =
  let module Definition = struct
    type input = string

    let name = name
    let description = None
    let type_ = "function"
    let parameters = `True
    let input_of_string input = input
  end
  in
  Ochat_function.create_function
    (module Definition)
    (fun input ->
       incr calls;
       Openai.Responses.Tool_output.Output.Text input)
;;

let fixture ?(author = help) ?(authentic = true) f =
  Mirage_crypto_rng_unix.use_default ();
  let calls = ref 0 in
  let implementations =
    List.map
      [ "author"
      ; "second"
      ; "ordinary_script"
      ; M.helper_name Reference
      ; M.helper_name Validation
      ]
      ~f:(fun name -> native name calls)
  in
  let metadata =
    [ ("author", M.{ authoring = Some author; helper = None })
    ; ("second", M.{ authoring = Some help; helper = None })
    ]
    @
    if authentic
    then
      List.map [ M.Reference; Validation ] ~f:(fun helper ->
        M.helper_name helper, M.{ authoring = None; helper = Some helper })
    else []
  in
  let ceiling =
    C.create
      ~metadata
      ~owner:"parent"
      ~resource_fingerprint:(digest "resources")
      (List.map implementations ~f:(fun fn -> digest "implementation", fn))
    |> cap_get
  in
  f ceiling;
  assert (!calls = 0)
;;

let names plan = C.references (P.capabilities plan) |> List.map ~f:(fun r -> r.name)

let%test_unit "ordinary tools do not imply authoring from names or policy" =
  fixture (fun ceiling ->
    List.iter [ S.Auto; Manual ] ~f:(fun policy ->
      let plan =
        P.resolve ~policy ~ceiling ~selected_names:[ "ordinary_script" ] () |> get
      in
      assert (not (P.inject_primer plan));
      assert (List.is_empty (P.added_helpers plan));
      assert (List.is_empty (P.authoring_tools plan));
      assert (Option.is_none (P.corpus_identity plan));
      assert (List.equal String.equal (names plan) [ "ordinary_script" ]));
    expect
      "authoring.invalid_preload"
      (P.resolve
         ~policy:(S.Preload [ "chatml/basics" ])
         ~ceiling
         ~selected_names:[ "ordinary_script" ]
         ()))
;;

let%test_unit
    "auto adds authentic helpers once from the ceiling and retains implementations"
  =
  fixture (fun ceiling ->
    let plan =
      P.resolve ~catalog:(catalog ()) ~ceiling ~selected_names:[ "author"; "second" ] ()
      |> get
    in
    assert (P.inject_primer plan);
    assert (List.length (P.authoring_tools plan) = 2);
    assert (List.length (P.added_helpers plan) = 2);
    assert (List.length (P.helper_pointers plan) = 2);
    List.iter
      (C.references (P.capabilities plan))
      ~f:(fun reference ->
        let original = C.find ceiling ~name:reference.name |> cap_get in
        let selected = C.find (P.capabilities plan) ~name:reference.name |> cap_get in
        assert (phys_equal original selected);
        assert (M.equal (C.metadata original) (C.metadata selected)));
    let again =
      P.resolve ~catalog:(catalog ()) ~ceiling ~selected_names:[ "second"; "author" ] ()
      |> get
    in
    assert (String.equal (P.fingerprint plan) (P.fingerprint again));
    let explicit =
      P.resolve
        ~catalog:(catalog ())
        ~ceiling
        ~selected_names:
          [ "author"; "second"; M.helper_name Reference; M.helper_name Validation ]
        ()
      |> get
    in
    assert (List.is_empty (P.added_helpers explicit));
    assert (not (String.equal (P.fingerprint plan) (P.fingerprint explicit)));
    let changed =
      P.resolve
        ~catalog:(catalog ~identity:"corpus-v2" ())
        ~ceiling
        ~selected_names:[ "author"; "second" ]
        ()
      |> get
    in
    assert (not (String.equal (P.fingerprint plan) (P.fingerprint changed)));
    expect
      "authoring.catalog_unavailable"
      (P.resolve ~ceiling ~selected_names:[ "author" ] ()))
;;

let%test_unit "manual does not add helpers or require automatic reference packages" =
  fixture (fun ceiling ->
    let plan =
      P.resolve ~policy:S.Manual ~ceiling ~selected_names:[ "author" ] () |> get
    in
    assert (not (P.inject_primer plan));
    assert (List.is_empty (P.helper_pointers plan));
    assert (List.equal String.equal (names plan) [ "author" ]);
    let explicit =
      P.resolve
        ~policy:S.Manual
        ~ceiling
        ~selected_names:[ "author"; M.helper_name Reference ]
        ()
      |> get
    in
    assert (List.is_empty (P.added_helpers explicit));
    assert (
      List.equal
        (fun (a, b) (c, d) -> M.equal_helper a c && String.equal b d)
        (P.helper_pointers explicit)
        [ M.Reference, M.helper_name Reference ]));
  fixture ~author:{ help with required_helpers = [ M.Validation ] } (fun ceiling ->
    expect
      "authoring.helper_unavailable"
      (P.resolve ~policy:S.Manual ~ceiling ~selected_names:[ "author" ] ());
    ignore
      (P.resolve
         ~policy:S.Manual
         ~ceiling
         ~selected_names:[ "author"; M.helper_name Validation ]
         ()
       |> get
       : P.t))
;;

let%test_unit "child auto cannot restore helpers removed by its parent" =
  fixture (fun ceiling ->
    let child_ceiling = C.select ceiling ~names:[ "author" ] |> cap_get in
    expect
      "authoring.helper_unavailable"
      (P.resolve
         ~catalog:(catalog ())
         ~ceiling:child_ceiling
         ~selected_names:[ "author" ]
         ());
    ignore
      (P.resolve ~policy:S.Manual ~ceiling:child_ceiling ~selected_names:[ "author" ] ()
       |> get
       : P.t));
  fixture ~authentic:false (fun ceiling ->
    expect
      "authoring.helper_unavailable"
      (P.resolve ~catalog:(catalog ()) ~ceiling ~selected_names:[ "author" ] ());
    let plan =
      P.resolve
        ~policy:S.Manual
        ~ceiling
        ~selected_names:[ "author"; M.helper_name Reference ]
        ()
      |> get
    in
    assert (List.is_empty (P.helper_pointers plan)))
;;

let%test_unit "preload validates packages topics tasks and explicit ordering" =
  fixture (fun ceiling ->
    let plan =
      P.resolve
        ~policy:(S.Preload [ "chatml/basics" ])
        ~catalog:(catalog ())
        ~ceiling
        ~selected_names:[ "author" ]
        ()
      |> get
    in
    assert (List.equal String.equal (P.preload_topics plan) [ "chatml/basics" ]);
    List.iter [ "missing"; "chatmd/children" ] ~f:(fun topic ->
      expect
        "authoring.incompatible_help"
        (P.resolve
           ~policy:(S.Preload [ topic ])
           ~catalog:(catalog ())
           ~ceiling
           ~selected_names:[ "author" ]
           ()));
    List.iter
      [ []; [ "chatml/basics"; "chatml/basics" ] ]
      ~f:(fun topics ->
        expect
          "authoring.invalid_preload"
          (P.resolve
             ~policy:(S.Preload topics)
             ~catalog:(catalog ())
             ~ceiling
             ~selected_names:[ "author" ]
             ())));
  fixture ~author:{ help with package = "uninstalled" } (fun ceiling ->
    expect
      "authoring.incompatible_help"
      (P.resolve ~catalog:(catalog ()) ~ceiling ~selected_names:[ "author" ] ()))
;;

let%test_unit
    "metadata and catalog validation reject unbound conflicting or malformed entries"
  =
  Mirage_crypto_rng_unix.use_default ();
  let calls = ref 0 in
  let create metadata =
    C.create
      ~metadata
      ~owner:"host"
      ~resource_fingerprint:(digest "resources")
      [ digest "implementation", native "tool" calls ]
  in
  List.iter
    [ [ "missing", M.empty ]
    ; [ "tool", M.empty; "tool", M.empty ]
    ; [ ("tool", M.{ authoring = None; helper = Some Reference }) ]
    ; [ ("tool", M.{ authoring = Some { help with version = 2 }; helper = None }) ]
    ; [ ("tool", M.{ authoring = Some { help with topics = [] }; helper = None }) ]
    ]
    ~f:(fun metadata -> assert (Result.is_error (create metadata)));
  expect
    "authoring.invalid_catalog"
    (P.catalog ~identity:(digest "corpus") ~packages:[ help ] ~topics:[]);
  expect
    "authoring.invalid_catalog"
    (P.catalog
       ~identity:(digest "corpus")
       ~packages:[ help; help ]
       ~topics:[ "chatml/basics", [ M.One_off_script ] ]);
  assert (!calls = 0)
;;

let%test_unit "parsed ChatMD policy keeps provenance and checks retained versions" =
  Eio_main.run (fun env ->
    let source = {|<authoring_context policy="manual"/>|} in
    let bundle =
      Chatmd_source_bundle.create
        ~root_file:"root.chatmd"
        ~sources:[ "root.chatmd", source ]
        ()
      |> Result.ok_or_failwith
    in
    let parsed =
      Prompt.Chat_markdown.parse_source_bundle ~dir:(Eio.Stdenv.cwd env) bundle
    in
    let context =
      List.find_map_exn parsed.root ~f:(function
        | Prompt.Chat_markdown.Authoring_context context -> Some context
        | _ -> None)
    in
    fixture (fun ceiling ->
      let plan =
        P.resolve_context ~context ~ceiling ~selected_names:[ "author" ] () |> get
      in
      assert (S.equal_policy (P.policy plan) S.Manual);
      assert (not (P.inject_primer plan));
      assert (String.equal (Option.value_exn (P.policy_source plan)).file "root.chatmd");
      expect
        "authoring.invalid_policy_version"
        (P.resolve_context
           ~context:{ context with version = 2 }
           ~ceiling
           ~selected_names:[ "author" ]
           ())))
;;

let%test_unit "authoring metadata uses frozen task and helper identifiers" =
  List.iter
    [ M.One_off_script
    ; Standalone_tool
    ; Moderator_tool
    ; Child_agent
    ; Background_workflow
    ]
    ~f:(fun task ->
      let json = M.jsonaf_of_task task in
      assert (M.equal_task (M.task_of_jsonaf json) task);
      assert (Poly.equal json (`Array [ `String (M.task_id task) ])));
  List.iter [ M.Reference; Validation ] ~f:(fun helper ->
    assert (
      Poly.equal (M.jsonaf_of_helper helper) (`Array [ `String (M.helper_name helper) ])))
;;
