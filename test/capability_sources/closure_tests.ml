open! Core
open Support
module Daemon = Agent_server.Daemon
module Catalog = Agent_session.Prompt_catalog
module Revision = Agent_session.Prompt_revision
module Programs = Chatml_prompt_programs
module Plan = Chat_response.Script_contract_plan
module Capture = Chatml_source

let protocol_ok = Agent_server_test_support.protocol_ok

let with_revision ?project_sources env root bundle entry f =
  let home = Eio.Path.(root / Agent_protocol.Id.Transaction.(create () |> to_string)) in
  Eio.Path.mkdir ~perm:0o700 home;
  Eio.Switch.run (fun sw ->
    let home_name = Eio.Path.native_exn home in
    let bundle_name = Eio.Path.native_exn bundle in
    let config =
      Agent_server_test_support.config
        home_name
        bundle_name
        (Eio.Path.native_exn Eio.Path.(bundle / Source.Portable_path.to_string entry))
    in
    let config = { config with server = { config.server with project_sources } } in
    let daemon =
      Daemon.start
        ~sw
        ~env
        ~config
        ~tool_dir:bundle_name
        ~home:home_name
        ~process_start_identity:None
        ~options:{ Daemon.default_options with startup_mode = On_demand }
        ()
      |> protocol_ok
    in
    Exn.protect
      ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
      ~f:(fun () ->
        match Catalog.entries (Daemon.prompts daemon) with
        | [ { availability = Ready revision; _ } ] ->
          let artifact_root =
            Agent_store.Data_root.prompt_artifacts_path
              (Agent_store.Session_store.data_root (Daemon.store daemon))
          in
          f (Ok (revision, artifact_root))
        | [ { availability = Unavailable diagnostics; _ } ] -> f (Error diagnostics)
        | [] | _ :: _ -> failwith "expected exactly one exported prompt"))
;;

let project_sources bundle =
  Agent_server.Project_source_configuration.
    { external_roots = [ Eio.Path.native_exn Eio.Path.(bundle / "examples/libraries") ]
    ; git = None
    }
;;

let require_ready = function
  | Ok value -> value
  | Error diagnostics ->
    raise_s
      [%message
        "exported project is unavailable"
          (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
;;

let restoration_limits () =
  (* Artifact admission deliberately has no semantic project-work allowance.
     This fixture is the restoring host and supplies its own finite policy. *)
  Programs.Limits.create
    ~max_programs:16
    ~max_parser_inputs:32
    ~max_logical_sources:128
    ~max_total_input_bytes:(8 * 1024 * 1024)
    ~project_work_steps:1_000_000
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let restored revision =
  let custody = Revision.programs revision |> Option.value_exn in
  let parsed = Revision.parser_context revision |> Option.value_exn in
  let definition = Revision.definition revision in
  let moderator_invocation = Agent_session.Prompt_definition.root_invocation definition in
  let moderator_surface =
    match moderator_invocation with
    | Ui -> Chatml.Chatml_builtin_surface.ui_moderator_surface
    | Headless -> Chatml.Chatml_builtin_surface.moderator_surface
  in
  (* Original custody includes inactive delegated variants. Only the actual root
     invocation's authenticated plan supplies contracts for current selection. *)
  let plan =
    Plan.create
      ~tool_contract:(Agent_session.Prompt_definition.tool_contract definition)
      parsed
      ~moderator_invocation
      ~moderator_surface
      ~delegated_moderator:false
      ~shell:None
      ~limits:
        (Plan.Limits.create
           ~max_programs:16
           ~max_metadata_bytes:1_000_000
           ~max_work_steps:1_000_000
         |> Or_error.ok_exn)
      ~checkpoint:Eio.Fiber.check
    |> function
    | Ok value -> value
    | Error diagnostics ->
      raise_s [%sexp (diagnostics : Chatmd_shell_spec.Diagnostic.t list)]
  in
  Programs.restore
    custody
    ~parsed
    ~contracts:(Plan.contracts plan)
    ~limits:(restoration_limits ())
    ~checkpoint:Eio.Fiber.check
  |> function
  | Ok value -> value
  | Error error ->
    raise_s
      (Sexp.List
         [ Sexp.Atom "captured exported project restore failed"
         ; Restore_diagnostics.sexp_of_error error
         ])
;;

let source_identity ?(normalize_entry = false) revision =
  restored revision
  |> Programs.Restored.programs
  |> List.concat_map ~f:(fun program ->
    let prepared = Programs.Restored.Program.prepared program in
    let entry = Chatml_prepared_program.entry prepared in
    Chatml_prepared_program.sources prepared
    |> List.filter_map ~f:(fun source ->
      let id = Capture.id source in
      let name = Capture.Id.relative_path id in
      if
        String.is_suffix name ~suffix:".chatml"
        || String.is_suffix name ~suffix:".chatmli"
      then
        Some
          ( (if normalize_entry && Capture.Id.equal id entry
             then "<registered-entry>"
             else Capture.Id.package_namespace id)
          , name
          , Capture.Digest.to_hex (Capture.digest source) )
      else None))
  |> List.dedup_and_sort ~compare:[%compare: string * string * string]
;;

let%expect_test
    "real exported local closure prepares and restores without original source tree"
  =
  List.iter [ "lifecycle"; "instruction-selection" ] ~f:(fun id ->
    with_workspace (fun env root parent ->
      let selected = resource ~id () in
      let left = export selected parent "left" |> export_ok in
      let right = export selected parent "right" |> export_ok in
      assert (
        Capture.Digest.equal (Export.Receipt.digest left) (Export.Receipt.digest right));
      let entry = Sources.Resource.entry selected in
      let left_tree = Eio.Path.(root / "left") in
      let right_tree = Eio.Path.(root / "right") in
      with_revision env root left_tree entry (function
        | Ok _ -> failwith "sibling libraries prepared without an explicit external root"
        | Error diagnostics ->
          assert (
            List.exists diagnostics ~f:(fun diagnostic ->
              String.equal
                diagnostic.Agent_session.Prompt_revision_builder.Diagnostic.code
                "prompt.original_source_unavailable")));
      let old, artifact_root =
        with_revision
          ~project_sources:(project_sources left_tree)
          env
          root
          left_tree
          entry
          require_ready
      in
      let expected = source_identity old in
      assert (not (List.is_empty expected));
      with_revision
        ~project_sources:(project_sources right_tree)
        env
        root
        right_tree
        entry
        (fun outcome ->
           let revision, _ = require_ready outcome in
           (* Independent prompt registration owns a distinct entry namespace.
            Compare entry bytes/path and full package IDs across export locations;
            exact original IDs are still required by the deletion/restore check. *)
           [%test_eq: (string * string * string) list]
             (source_identity ~normalize_entry:true revision)
             (source_identity ~normalize_entry:true old));
      let required =
        Sources.Resource.members selected
        |> List.filter_map ~f:(fun member ->
          let name = Source.Portable_path.to_string (Sources.Member.path member) in
          if
            String.is_suffix name ~suffix:".chatml"
            || String.is_suffix name ~suffix:".chatmli"
            || String.equal (Filename.basename name) "chatml.project.json"
          then Some (Sources.Member.path member)
          else None)
      in
      Docs_project_bundle.check_with_project_sources
        ~project_sources:(project_sources right_tree)
        ~env
        ~home_parent:root
        ~bundle:right_tree
        ~entry
        ~required;
      Eio.Path.rmtree left_tree;
      Eio.Path.rmtree right_tree;
      let artifacts =
        Agent_store.Prompt_artifact_store.open_existing ~env ~root:artifact_root
        |> function
        | Ok value -> value
        | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
      in
      let reloaded =
        Agent_session.Prompt_revision_builder.restore
          ~artifact_store:artifacts
          (Revision.definition old)
          (Revision.id old)
        |> function
        | Ok value -> value
        | Error errors ->
          raise_s
            [%sexp (errors : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
      in
      [%test_eq: (string * string * string) list] (source_identity reloaded) expected;
      print_endline
        "two fresh exports: same package identities and entry bytes; every required \
         member enforced; captured restore survives deletion"));
  [%expect
    {|
    two fresh exports: same package identities and entry bytes; every required member enforced; captured restore survives deletion
    two fresh exports: same package identities and entry bytes; every required member enforced; captured restore survives deletion
    |}]
;;
