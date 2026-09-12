open Core
module P = Chat_response.Authoring_policy
module C = Chat_response.Tool_capability
module M = Chatmd_shell_spec.Authoring_metadata
module S = Chatmd_shell_spec.Extension_spec
module G = Agent_protocol.Authoring_guidance
module H = Agent_protocol.History
module Presence = Chat_response.Authoring_presence

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

let%expect_test "guidance presence honors policy, provenance and exact effective content" =
  fixture (fun ceiling ->
    let plan policy =
      P.resolve ~policy ~catalog:(catalog ()) ~ceiling ~selected_names:[ "author" ] ()
      |> get
    in
    let auto = plan Auto in
    let manual = plan Manual in
    let preload = plan (Preload [ "chatml/basics" ]) in
    let ok = function
      | Ok x -> x
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
    in
    let context_identity = digest "installed-runtime-and-target" in
    let make ?(corpus = "corpus-v1") policy purpose sequence ~complete ~authored =
      let payload =
        `Object [ "role", `String "user"; "content", `String "Exact guidance" ]
      in
      let guidance =
        G.create
          ~context_identity
          ~policy_fingerprint:(P.fingerprint policy)
          ~purpose
          ~payload
          ~topics:
            [ { id = "chatml/basics"
              ; document_sha256 = digest "topic-v1"
              ; source =
                  (if authored
                   then Authored (digest "custom")
                   else Installed (digest corpus))
              ; complete
              }
            ]
        |> ok
      in
      H.
        { id =
            History_entry.Id.create ~namespace:"guidance" ~sequence
            |> Result.ok_or_failwith
        ; role = User
        ; kind = Message
        ; payload
        ; provenance = Runtime_authoring guidance
        ; redacted = false
        }
    in
    let show label policy entry effective ~context_identity =
      let known = Presence.remember ~previous:[] ~history:[ entry ] |> ok in
      let result = Presence.inspect ~policy ~context_identity ~known ~effective |> ok in
      print_s
        [%sexp
          (label : string)
        , (List.map result.observations ~f:(fun item -> item.presence)
           : Presence.presence list)
        , (result.refresh_primer : bool)
        , (result.missing_preload : string list)]
    in
    let entry = make auto Primer 0 ~complete:true ~authored:false in
    show "present" auto entry [ entry ] ~context_identity;
    show "compacted" auto entry [] ~context_identity;
    show
      "changed payload at same ID"
      auto
      entry
      [ { entry with payload = `String "summary" } ]
      ~context_identity;
    show
      "same text without host provenance"
      auto
      entry
      [ { entry with provenance = Canonical } ]
      ~context_identity;
    show
      "redacted"
      auto
      entry
      [ { entry with payload = `Object []; redacted = true } ]
      ~context_identity;
    show "new target" auto entry [ entry ] ~context_identity:(digest "other-target");
    show "manual after auto" manual entry [ entry ] ~context_identity;
    show "manual after compaction" manual entry [] ~context_identity;
    let loaded = make preload Preload 1 ~complete:true ~authored:false in
    show "preloaded topic" preload loaded [ loaded ] ~context_identity;
    let wrong_corpus =
      make ~corpus:"other-corpus" preload Preload 5 ~complete:true ~authored:false
    in
    show "other installed corpus" preload wrong_corpus [ wrong_corpus ] ~context_identity;
    let partial = make preload Reference 2 ~complete:false ~authored:false in
    show "partial topic" preload partial [ partial ] ~context_identity;
    let authored = make preload Preload 3 ~complete:true ~authored:true in
    show "custom prose" preload authored [ authored ] ~context_identity;
    let pointer = make preload Rediscovery 4 ~complete:false ~authored:false in
    show "rediscovery pointer" preload pointer [ pointer ] ~context_identity;
    let known = Presence.remember ~previous:[] ~history:[ entry ] |> ok in
    let repeated = Presence.remember ~previous:known ~history:[ entry ] |> ok in
    assert (List.equal Presence.equal_receipt known repeated);
    let rebound = make preload Primer 0 ~complete:true ~authored:false in
    assert (Result.is_error (Presence.remember ~previous:known ~history:[ rebound ]));
    assert (
      Result.is_error
        (Presence.inspect
           ~policy:auto
           ~context_identity
           ~known
           ~effective:[ entry; entry ]));
    assert (Result.is_error (Presence.remember ~previous:[] ~history:[ entry; entry ])));
  [%expect
    {|
    (present (Present) false ())
    (compacted (Absent) true ())
    ("changed payload at same ID" (Modified) true ())
    ("same text without host provenance" (Modified) true ())
    (redacted (Redacted) true ())
    ("new target" (Stale_context) true ())
    ("manual after auto" (Stale_policy) false ())
    ("manual after compaction" (Absent) false ())
    ("preloaded topic" (Present) true ())
    ("other installed corpus" (Present) true (chatml/basics))
    ("partial topic" (Present) true (chatml/basics))
    ("custom prose" (Present) true (chatml/basics))
    ("rediscovery pointer" (Present) true (chatml/basics))
    |}]
;;

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
    assert (List.is_empty (P.helper_pointers plan));
    let description =
      Chat_response.Authoring_tool_description.describe
        ~capabilities:(P.capabilities plan)
        ~name:"author"
        ~description:(Some "Custom authoring entrypoint.")
      |> Option.value_exn
    in
    assert (String.is_prefix description ~prefix:"Custom authoring entrypoint.");
    assert (String.is_substring description ~substring:"chatml/basics");
    assert (not (String.is_substring description ~substring:(M.helper_name Reference))))
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

let%test_unit "authored help binds actual registrations without overriding trusted roles" =
  let module R = Chat_response.Authoring_registration in
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let calls = ref 0 in
    let implementation = native "author" calls in
    let source =
      {|<authoring_help tool="author" package="one-off" tasks="one_off_script" topics="chatml/basics"/>|}
    in
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
    let declaration =
      List.find_map_exn parsed.root ~f:(function
        | Prompt.Chat_markdown.Authoring_help help -> Some help
        | _ -> None)
    in
    let registrations = [ digest "implementation", implementation ] in
    let create ?host_metadata ?(registrations = registrations) declarations =
      R.create
        ?host_metadata
        ~declarations
        ~owner:"owner"
        ~resource_fingerprint:(digest "roots")
        registrations
    in
    let admitted = create [ declaration ] |> cap_get in
    let registry = R.capabilities admitted in
    let binding = C.find registry ~name:"author" |> cap_get in
    assert (
      phys_equal (C.native_implementation binding |> Option.value_exn) implementation);
    assert (Option.equal M.equal_help (C.metadata binding).authoring (Some help));
    assert (Option.is_none (C.metadata binding).helper);
    assert (
      String.equal
        (List.Assoc.find_exn (R.sources admitted) ~equal:String.equal "author").file
        "root.chatmd");
    assert (!calls = 0);
    let plan =
      P.resolve ~policy:Manual ~ceiling:registry ~selected_names:[ "author" ] () |> get
    in
    assert (not (P.inject_primer plan));
    assert (List.length (P.authoring_tools plan) = 1);
    expect
      "authoring.helper_unavailable"
      (P.resolve ~catalog:(catalog ()) ~ceiling:registry ~selected_names:[ "author" ] ());
    let reject code result =
      match result with
      | Ok _ -> failwith ("expected " ^ code)
      | Error (error : C.error) -> assert (String.equal code error.code)
    in
    reject "authoring.unknown_tool" (create [ { declaration with tool = "missing" } ]);
    reject
      "authoring.invalid_metadata"
      (create [ { declaration with tool = "bad tool" } ]);
    reject "authoring.duplicate_metadata" (create [ declaration; declaration ]);
    reject
      "authoring.invalid_metadata"
      (create [ { declaration with help = { help with version = 2 } } ]);
    reject
      "authoring.metadata_override"
      (create
         ~host_metadata:[ ("author", M.{ authoring = Some help; helper = None }) ]
         [ declaration ]);
    reject
      "authoring.helper_metadata"
      (create
         ~registrations:[ digest "helper", native "ochat_validate" calls ]
         [ { declaration with tool = "ochat_validate" } ]);
    reject
      "capability.invalid_registration"
      (create ~registrations:[ "bad", implementation ] [ declaration ]);
    let helpers = [ M.Reference; Validation ] in
    let helper_registrations =
      List.map helpers ~f:(fun helper ->
        digest "helper", native (M.helper_name helper) calls)
    in
    let host_metadata =
      List.map helpers ~f:(fun helper ->
        M.helper_name helper, M.{ authoring = None; helper = Some helper })
    in
    let resolve ?context ?(registrations = registrations @ helper_registrations) () =
      R.resolve
        ~host_metadata
        ?context
        ~catalog:(catalog ())
        ~declarations:[ declaration ]
        ~owner:"owner"
        ~resource_fingerprint:(digest "roots")
        ~registrations
        ~selected_names:[ "author" ]
        ()
    in
    let registered, automatic = resolve () |> cap_get in
    assert (P.inject_primer automatic);
    assert (List.length (P.added_helpers automatic) = 2);
    let author = C.find (P.capabilities automatic) ~name:"author" |> cap_get in
    assert (phys_equal (C.native_implementation author |> Option.value_exn) implementation);
    assert (List.length (R.sources registered) = 1);
    let context : S.authoring_context =
      { version = 1; policy = Manual; source_ref = declaration.source_ref }
    in
    let _, manual = resolve ~context () |> cap_get in
    assert (not (P.inject_primer manual));
    assert (List.is_empty (P.added_helpers manual));
    let _, preload =
      resolve ~context:{ context with policy = Preload [ "chatml/basics" ] } () |> cap_get
    in
    assert (List.equal String.equal (P.preload_topics preload) [ "chatml/basics" ]);
    let changed =
      { declaration with
        source_ref = { declaration.source_ref with file = "other.chatmd" }
      }
    in
    let other = create [ changed ] |> cap_get |> R.capabilities in
    let other = C.find other ~name:"author" |> cap_get |> C.reference in
    assert (
      not
        (String.equal
           (C.reference binding).implementation_revision
           other.implementation_revision));
    assert (!calls = 0))
;;
