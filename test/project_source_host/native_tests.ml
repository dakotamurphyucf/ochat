open Core
open Support
module Fixture = Git_fixture_support
module Git = Chatml_git_host
module Sources = Agent_session.Prompt_script_preparation
module Owner = Agent_session.Prompt_source_owner
module C = Chatml_prompt_programs

let save root file body =
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / file) body
;;

let helpers root =
  let helper = Eio.Path.(root / "helpers") in
  Eio.Path.mkdir ~perm:0o700 helper;
  List.iter
    [ "git", "git"; "git-upload-pack", "git"; "git-remote-https", "git-remote-https" ]
    ~f:(fun (name, target) ->
      Eio.Path.symlink
        ~link_to:(Filename.concat (Fixture.deployment_directory ()) target)
        Eio.Path.(helper / name));
  Eio.Path.native_exn helper
;;

let project_repo env root format =
  let source, _, _, _ = Fixture.fixture env root format in
  let git arguments stdin =
    Fixture.run_git env source ~arguments ~stdin |> String.strip
  in
  let manifest =
    {|{"schema_version":1,"namespace":"dependency","modules":{"Code":{"implementation":"code.chatml"}}}|}
  in
  let declaration = git [ "hash-object"; "-w"; "--stdin" ] manifest in
  let body = "let meaning = 42\n" in
  let blob = git [ "hash-object"; "-w"; "--stdin" ] body in
  let tree =
    git
      [ "mktree" ]
      ("100644 blob "
       ^ declaration
       ^ "\tchatml.project.json\n100644 blob "
       ^ blob
       ^ "\tcode.chatml\n")
  in
  let commit = git [ "commit-tree"; tree ] "project\n" in
  ignore (git [ "update-ref"; "refs/heads/main"; commit ] "");
  ignore (git [ "symbolic-ref"; "HEAD"; "refs/heads/main" ] "");
  ( source
  , Git.Pin.create ~object_format:format ~commit |> Or_error.ok_exn
  , Git.Object_id.create ~object_format:format blob |> Or_error.ok_exn
  , body )
;;

let captured env root host dependencies =
  save
    root
    "lookup.chatml"
    "let run = fun ctx input -> Task.pure(`Complete(`String(\"captured\")))";
  save
    root
    "root.chatmd"
    {|<script id="lookup" language="chatml" kind="tool" src="lookup.chatml" project="chatml.project.json"/>|};
  save
    root
    "chatml.project.json"
    ("{\"schema_version\":1,\"namespace\":\"example\",\"modules\":{\"Lookup\":{\"implementation\":\"lookup.chatml\"}}"
     ^ dependencies
     ^ "}");
  let definition =
    Agent_session.Prompt_definition.create
      ~id:(Agent_protocol.Id.Prompt_definition.of_string "prd_original" |> ok)
      ~config_name:"original"
      ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
      ~allowed_workspaces:[]
      ~permission_profile:"interactive"
      ~runtime_policy:None
      ~enabled:true
      ~description:None
    |> ok
  in
  let receipt =
    Agent_store.Prompt_source_receipt.create
      ~definition_id:definition.id
      ~root_file:definition.root_file
      ~max_path_bytes:4_096
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let source_limits =
    Owner.Limits.create ~max_path_components:64 ~max_runtime_leases:4 |> Or_error.ok_exn
  in
  let parser_limits =
    Prompt.Chat_markdown.Limits.create
      ~max_input_bytes:100_000
      ~max_parse_calls:100
      ~max_tokens:10_000
      ~max_depth:64
    |> Or_error.ok_exn
  in
  Eio.Path.with_open_dir root (fun directory ->
    Eio.Switch.run (fun sw ->
      let owner =
        Owner.open_owned
          directory
          ~sw
          ~policy:(Owner.Policy.create receipt ~limits:source_limits)
          ~checkpoint:ignore
        |> ok
      in
      Owner.with_scope owner ~f:(fun scope ->
        let capture =
          Owner.Scope.capture
            scope
            ~env
            ~definition
            ~preprocess:Fn.id
            ~limits:parser_limits
            ~checkpoint:ignore
          |> ok
        in
        let prepared =
          Host.with_host host ~f:(fun policy host ->
            match
              Sources.capture
                policy
                ~root_role:Headless
                ~host
                ~scope
                ~capture
                ~operation:(fun _ -> Chatml_project_build.Operation.Initial_prepare)
                ~provided:[]
                ~env
                ~checkpoint:ignore
                ()
            with
            | Ok result -> result
            | Error diagnostics ->
              raise_s
                [%sexp
                  (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)])
          |> ok
        in
        Ok (Sources.Prepared.programs prepared))
      |> ok))
;;

let%test_unit "local-only host prepares real configured local projects" =
  with_workspace (fun env root data ->
    Eio.Switch.run (fun sw ->
      let host = open_host env data sw in
      let programs = captured env root host "" in
      assert (List.length (C.bindings programs) = 1)))
;;

let%test_unit "explicit Git deployment memory is enforced at probe without widening" =
  with_workspace (fun env root data ->
    let configuration =
      Configuration.
        { external_roots = []
        ; git =
            Some
              { helper_directory = helpers root
              ; ca_bundle = None
              ; memory_bytes_per_process = 1
              ; repositories = []
              }
        }
    in
    Eio.Switch.run (fun sw ->
      match
        Host.open_owned
          configuration
          ~sw
          ~fs:env#fs
          ~data_root:data
          ~clock:env#mono_clock
          ~checkpoint:ignore
          ()
      with
      | Error (Host.Error.Git _) ->
        assert (
          match Eio.Path.kind ~follow:false Eio.Path.(root / "data" / "project-git") with
          | `Not_found -> true
          | _ -> false)
      | Ok _ | Error _ -> assert false))
;;

let%expect_test
    "explicit local and authorized HTTPS grants capture full SHA1 and SHA256 projects"
  =
  List.iter [ Git.Pin.Sha1; Sha256 ] ~f:(fun format ->
    with_workspace (fun env root data ->
      let helper_directory = helpers root in
      let source, pin, oid, body = project_repo env root format in
      save root "ca.pem" Tls_fixture.certificate;
      save root "authorization" "Authorization: Bearer fixture-private-header";
      Eio.Switch.run (fun sw ->
        let requests = ref 0 in
        let https =
          Https_fixture.start env#net ~sw ~handle:(fun request ->
            incr requests;
            assert (
              Option.equal
                String.equal
                (Https_fixture.Request.header request "authorization")
                (Some "Bearer fixture-private-header"));
            Fixture.smart_git_response env source request)
          ^ "/repo"
        in
        List.iter
          [ Configuration.Repository.Local (Eio.Path.native_exn source)
          ; Https
              { repository = https
              ; authorization_header_file =
                  Some (Eio.Path.native_exn Eio.Path.(root / "authorization"))
              }
          ]
          ~f:(fun route ->
            let repository =
              match route with
              | Configuration.Repository.Local path -> path
              | Https { repository; _ } -> repository
            in
            let configuration =
              Configuration.
                { external_roots = []
                ; git =
                    Some
                      { helper_directory
                      ; ca_bundle = Some (Eio.Path.native_exn Eio.Path.(root / "ca.pem"))
                      ; memory_bytes_per_process = Fixture.memory_cap ()
                      ; repositories = [ route ]
                      }
                }
            in
            let host = open_host ~configuration env data sw in
            let object_format =
              match format with
              | Git.Pin.Sha1 -> "sha1"
              | Sha256 -> "sha256"
            in
            let dependencies =
              sprintf
                {|,"environments":{"default":{"modules":{"Own":"self.Lookup","Lib":"lib.Code"}}},"dependencies":{"lib":{"source":{"kind":"git","repository":%s,"object_format":%s,"commit":%s},"project":"chatml.project.json"}}|}
                (Jsonaf.to_string (`String repository))
                (Jsonaf.to_string (`String object_format))
                (Jsonaf.to_string (`String (Git.Pin.commit pin)))
            in
            let programs = captured env root host dependencies in
            assert (List.length (C.bindings programs) = 1);
            Host.with_host host ~f:(fun _ sources ->
              let store = Option.value_exn sources.git_store in
              let result =
                Git.with_cached_repository
                  store
                  ~repository
                  ~pin
                  ~limits:(Fixture.limits ())
                  ~checkpoint:ignore
                  ~f:(fun repository ->
                    ignore (Fixture.expect_raw repository oid body : string);
                    Ok ())
              in
              let _, receipt = Fixture.admitted result in
              assert (
                Git.Receipt.equal_disposition (Git.Receipt.disposition receipt) Cached))
            |> ok;
            Host.close_and_wait host;
            print_s
              [%sexp
                (object_format : string)
              , ((match route with
                  | Local _ -> "local"
                  | Https _ -> "https")
                 : string)]);
        assert (!requests >= 2))));
  [%expect
    {|
    (sha1 local)
    (sha1 https)
    (sha256 local)
    (sha256 https)
    |}]
;;

let%test_unit "explicit external anchor prepares a local dependency outside original root"
  =
  with_workspace (fun env root data ->
    let prompt = Eio.Path.(root / "prompt") in
    let shared = Eio.Path.(root / "shared") in
    Eio.Path.mkdir ~perm:0o700 prompt;
    Eio.Path.mkdir ~perm:0o700 shared;
    save shared "code.chatml" "let meaning = 42";
    save
      shared
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"shared","modules":{"Code":{"implementation":"code.chatml"}}}|};
    let path = Eio.Path.native_exn shared in
    let configuration = Configuration.{ external_roots = [ path ]; git = None } in
    Eio.Switch.run (fun sw ->
      let host = open_host ~configuration env data sw in
      let dependencies =
        sprintf
          {|,"environments":{"default":{"modules":{"Own":"self.Lookup","Lib":"lib.Code"}}},"dependencies":{"lib":{"source":{"kind":"local","path":%s},"project":"chatml.project.json"}}|}
          (Jsonaf.to_string (`String path))
      in
      let programs = captured env prompt host dependencies in
      assert (List.length (C.bindings programs) = 1);
      Host.with_host host ~f:(fun _ sources ->
        assert (List.length sources.additional_anchors = 1);
        assert (Option.is_none sources.git_store))
      |> ok))
;;
