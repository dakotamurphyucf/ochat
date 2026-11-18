open! Core
open Support
module A = Agent_session
module S = Agent_server
module Port = A.Instruction_source_port
module Request = A.Instruction_source_request

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let configured_port
      project
      source
      ~(project_sources : S.Project_source_configuration.t)
      ~actor
      ~installed
  =
  let principal = Operator_authorization.principal actor in
  let configuration : S.Authoring_project_access_configuration.t =
    { read_grants =
        List.map project_sources.external_roots ~f:(fun root ->
          { S.Authoring_project_access_configuration.Read_grant.root
          ; principal_id = principal.id
          ; authentication_kind = principal.authentication_kind
          })
    ; selections = []
    }
  in
  S.Authoring_project_access_configuration.validate configuration ~project_sources
  |> Or_error.ok_exn;
  let bound =
    S.Authoring_configured_project_policy.bind
      project
      ~configuration
      ~project_sources
      ~contracts:None
      ~max_contract_identity_bytes:1048576
      ~max_work_steps:1048576
      ~checkpoint:ignore
    |> function
    | Ok value -> value
    | Error error -> raise_s (S.Authoring_configured_project_policy.Error.sexp_of_t error)
  in
  S.Authoring_instruction_sources.create source ~configured:bound ~is_current:(fun () ->
    Atomic.get installed)
;;

let with_port ?actor f =
  let project = ref None in
  with_roots
    ?allowed_actor:actor
    ~on_project:(fun value -> project := Some value)
    (fun env home left right source _ _ _ actor _ admitted ->
       let project_sources : S.Project_source_configuration.t =
         { external_roots = [ Eio.Path.native_exn left; Eio.Path.native_exn right ]
         ; git = None
         }
       in
       let installed = Atomic.make true in
       let port =
         configured_port
           (Option.value_exn !project)
           source
           ~project_sources
           ~actor
           ~installed
       in
       f env home left right port actor admitted installed)
;;

let descriptor selection discovery =
  Request.create
    ~selection
    ~members:
      (List.map (Request.Discovery.observations discovery) ~f:Request.Member.of_observed)
  |> protocol_ok
;;

let%expect_test
    "real read-only source facade captures exact bytes, refresh is inert, and Uses expire"
  =
  with_port (fun _ _ left right port actor admitted installed ->
    let original = skill "alpha" in
    save left "alpha/SKILL.md" original;
    save right "beta/SKILL.md" (skill "beta");
    let retained_use = ref None in
    let capture, roots =
      Port.with_use port ~actor ~checkpoint:ignore (fun use ->
        retained_use := Some use;
        let roots = Port.Use.roots use in
        [%test_eq: int] 2 (List.length roots);
        assert (not (C.Root_id.equal (List.nth_exn roots 0) (List.nth_exn roots 1)));
        let selection = Request.Selection.Skills [ List.hd_exn roots ] in
        let discovery = Port.refresh use ~selection ~checkpoint:ignore |> protocol_ok in
        let request = descriptor selection discovery in
        let capture = Port.capture use ~request ~checkpoint:ignore |> protocol_ok in
        [%test_eq: string list]
          [ original ]
          (List.map
             (A.Instruction_capture.loaded (Port.Captured.contents capture))
             ~f:C.Loaded.contents);
        Ok (Port.Captured.contents capture, roots))
      |> protocol_ok
    in
    assert (Result.is_error (Port.Use.check_now (Option.value_exn !retained_use) ~roots));
    let replacement = skill ~description:"Explicitly refreshed metadata" "alpha" in
    save left "alpha/SKILL.md" replacement;
    Port.with_use port ~actor ~checkpoint:ignore (fun use ->
      let selection = Request.Selection.Skills [ List.hd_exn roots ] in
      let discovery = Port.refresh use ~selection ~checkpoint:ignore |> protocol_ok in
      [%test_eq: string option]
        (Some "Explicitly refreshed metadata")
        (C.Metadata.description
           (C.Observed.metadata (List.hd_exn (Request.Discovery.observations discovery))));
      [%test_eq: string list]
        [ original ]
        (List.map (A.Instruction_capture.loaded capture) ~f:C.Loaded.contents);
      Ok ())
    |> protocol_ok;
    admitted := false;
    assert (
      Result.is_error
        (Port.with_use port ~actor ~checkpoint:ignore (fun use ->
           Port.Use.check_now use ~roots)));
    admitted := true;
    Atomic.set installed false;
    assert (
      Result.is_error (Port.with_use port ~actor ~checkpoint:ignore (fun _ -> Ok ())));
    print_endline
      "read-only roots; exact capture; refresh leaves captured bytes; \
       expired/revoked/closed uses deny");
  [%expect
    {| read-only roots; exact capture; refresh leaves captured bytes; expired/revoked/closed uses deny |}]
;;

let%expect_test
    "source completion is truthful while a later effect and stale capture disclosure deny"
  =
  with_port (fun _ _ left _ port actor admitted _ ->
    save left "alpha/SKILL.md" (skill "alpha");
    let roots =
      Port.with_use port ~actor ~checkpoint:ignore (fun use -> Ok (Port.Use.roots use))
      |> protocol_ok
    in
    assert (not (List.is_empty roots));
    let completed =
      Port.with_use port ~actor ~checkpoint:ignore (fun _ ->
        (* Represents an already completed effect at this bracket's unit boundary.
         Actual provider qualification is separate; this asserts no post-return
         authority check can relabel a completion. *)
        admitted := false;
        Ok "completed")
      |> protocol_ok
    in
    [%test_eq: string] "completed" completed;
    (* An empty current root catalog is a valid discovery result. Reusing an
       earlier selected root is the denied effect, not entering an empty Use. *)
    assert (
      Result.is_error
        (Port.with_use port ~actor ~checkpoint:ignore (fun use ->
           Port.Use.check_now use ~roots)));
    admitted := true;
    Port.with_use port ~actor ~checkpoint:ignore (fun use ->
      let selection = Request.Selection.Skills [ List.hd_exn (Port.Use.roots use) ] in
      let discovery = Port.refresh use ~selection ~checkpoint:ignore |> protocol_ok in
      let request = descriptor selection discovery in
      save left "alpha/SKILL.md" (skill ~description:"changed" "alpha");
      admitted := false;
      (match Port.capture use ~request ~checkpoint:ignore with
       | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
       | Ok _ -> failwith "revoked source disclosed stale capture");
      Ok ())
    |> protocol_ok;
    print_endline
      "completed result retained; next effect and revoked stale disclosure denied");
  [%expect
    {| completed result retained; next effect and revoked stale disclosure denied |}]
;;

(* Reopen the actual configured source host, so changing an array position cannot
   stand in for changing the source identity behind a retained selection. *)
let with_configured_roots env data ~actor ~roots f =
  Eio.Switch.run (fun sw ->
    let project_sources : S.Project_source_configuration.t =
      { external_roots = List.map roots ~f:Eio.Path.native_exn; git = None }
    in
    let project = Project_support.open_host ~configuration:project_sources env data sw in
    let source =
      Sources.create
        project
        ~sw
        ~read_policy:
          (Sources.Read_policy.create ~authorize:(fun candidate ~root_index ->
             root_index >= 0
             && root_index < List.length roots
             && P.Id.Principal.equal
                  (Operator_authorization.principal candidate).id
                  (Operator_authorization.principal actor).id))
        ~document_policy:Sources.Document_policy.deny_all
        ~limits:
          (Sources.Limits.create
             ~max_contexts:4
             ~max_context_requests:16
             ~max_manifest_entries:32
             ~max_browse_entries:256
             ~max_read_bytes:1_048_576
             ~max_context_read_bytes:8_388_608
           |> Or_error.ok_exn)
    in
    Exn.protect
      ~finally:(fun () ->
        Sources.close_and_wait source;
        Project_support.Host.close_and_wait project)
      ~f:(fun () ->
        let port =
          configured_port
            project
            source
            ~project_sources
            ~actor
            ~installed:(Atomic.make true)
        in
        f port))
;;

let%expect_test
    "configured root identity survives reorder but never aliases a replacement"
  =
  Project_support.with_workspace (fun env home data ->
    let left = Eio.Path.(home / "left") in
    let right = Eio.Path.(home / "right") in
    List.iter [ left; right ] ~f:(Eio.Path.mkdir ~perm:0o700);
    (* Identical members and bytes deliberately remove content mismatch as a
       fallback defense: only the real configured root identity distinguishes them. *)
    let bytes = skill "same" in
    List.iter [ left; right ] ~f:(fun root -> save root "same/SKILL.md" bytes);
    let actor = actor "pri_instruction_reorder" in
    let request, original_roots =
      with_configured_roots env data ~actor ~roots:[ left; right ] (fun port ->
        Port.with_use port ~actor ~checkpoint:ignore (fun use ->
          let roots = Port.Use.roots use in
          let selection = Request.Selection.Skills [ List.hd_exn roots ] in
          let discovery = Port.refresh use ~selection ~checkpoint:ignore |> protocol_ok in
          Ok (descriptor selection discovery, roots))
        |> protocol_ok)
    in
    with_configured_roots env data ~actor ~roots:[ right; left ] (fun port ->
      Port.with_use port ~actor ~checkpoint:ignore (fun use ->
        assert (List.equal C.Root_id.equal (List.rev original_roots) (Port.Use.roots use));
        let capture = Port.capture use ~request ~checkpoint:ignore |> protocol_ok in
        [%test_eq: string list]
          [ bytes ]
          (List.map
             (A.Instruction_capture.loaded (Port.Captured.contents capture))
             ~f:C.Loaded.contents);
        Ok ())
      |> protocol_ok);
    with_configured_roots env data ~actor ~roots:[ right ] (fun port ->
      Port.with_use port ~actor ~checkpoint:ignore (fun use ->
        (match Port.capture use ~request ~checkpoint:ignore with
         | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
         | Ok _ -> failwith "removed root selection rebound to replacement root");
        Ok ())
      |> protocol_ok);
    print_endline "reordered root retains identity; same-byte replacement cannot capture");
  [%expect {| reordered root retains identity; same-byte replacement cannot capture |}]
;;
