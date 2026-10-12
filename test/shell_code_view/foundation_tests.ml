open Core
module B = Registry_code_budget
module F = Registry_resource_footprint
module E = Shell_access.Executor
module S = Chatmd_shell_spec.Shell_spec

let run maximum f = B.run ~max_work_steps:maximum ~checkpoint:ignore f

let%test_unit "chunk admission and lexical comparison follow independent byte oracle" =
  Eio_main.run (fun _ ->
    List.iter [ 0; 1; 1023; 1024; 1025; 8192 ] ~f:(fun length ->
      let prefix = String.make length 'a' in
      let expected = 1 + ((length + 1023) / 1024) in
      assert (
        match run expected (fun budget -> Ok (B.equal_string budget prefix prefix)) with
        | Ok (Ok (true, work)) -> Int.equal work expected
        | Ok (Ok (false, _)) | Ok (Error _) | Error _ -> false);
      assert (
        match
          run (expected - 1) (fun budget -> Ok (B.equal_string budget prefix prefix))
        with
        | Error B.Error.Work_limit -> true
        | Error Invalid_limit | Ok _ -> false);
      let right = prefix ^ "b" in
      let left = prefix ^ "a" in
      assert (
        match run 100 (fun budget -> Ok (B.compare_string budget left right)) with
        | Ok (Ok (comparison, _)) -> Int.equal comparison (String.compare left right)
        | Ok (Error _) | Error _ -> false)))
;;

let%test_unit "caught denial and checkpoint exception cannot authorize success" =
  Eio_main.run (fun _ ->
    assert (
      match
        run 0 (fun budget ->
          (try B.work budget with
           | _ -> ());
          Ok ())
      with
      | Error B.Error.Work_limit -> true
      | Error Invalid_limit | Ok _ -> false);
    let original = Failure "borrowed checkpoint" in
    let caught =
      try
        ignore
          (B.run
             ~max_work_steps:10
             ~checkpoint:(fun () -> raise original)
             (fun budget ->
                (try B.work budget with
                 | _ -> ());
                Ok ()));
        false
      with
      | actual -> phys_equal actual original
    in
    assert caught)
;;

let source =
  let position : Chatmd_shell_spec.Source_ref.position =
    { offset = 0; line = 1; column = 1 }
  in
  Chatmd_shell_spec.Source_ref.create
    ~file:"child.chatmd"
    ~source_dir:"/original"
    ~prompt_dir:"/original"
    ~namespace:None
    ~start_pos:position
    ~end_pos:position
    ~source:"original"
;;

let specification : S.t =
  { id =
      (match S.Runtime_id.of_string "child" with
       | Ok id -> id
       | Error _ -> failwith "invalid fixture runtime ID")
  ; extends = None
  ; requested_profile = None
  ; resolved_profile = None
  ; cwd = Inherit
  ; pipefail = Inherit
  ; capabilities = None
  ; resolver = None
  ; environment = None
  ; limits = None
  ; backends = None
  ; policy = None
  ; approvals = None
  ; reviewers = None
  ; interceptors = None
  ; effect_analysis = None
  ; secrets = None
  ; audit = None
  ; source
  }
;;

let%test_unit "effective field changes reject while diagnostic rebasing remains allowed" =
  Eio_main.run (fun _ ->
    let changes : (S.t -> S.t) list =
      [ (fun t -> { t with extends = Some "other" })
      ; (fun t -> { t with requested_profile = Some "development" })
      ; (fun t -> { t with resolved_profile = Some "development" })
      ; (fun t -> { t with cwd = Set (Absolute "/different") })
      ; (fun t -> { t with pipefail = Set true })
      ; (fun t ->
          { t with
            environment =
              Some { inherit_ = Set All_sanitized; operations = []; merge = Append }
          })
      ; (fun t ->
          { t with policy = Some { default = Set Allow; rules = []; merge = Append } })
      ; (fun t ->
          { t with
            secrets = Some { replacement = Set "hidden"; sources = []; merge = Append }
          })
      ]
    in
    List.iter changes ~f:(fun change ->
      assert (
        match
          run 1000 (fun budget ->
            F.check_runtime specification ~candidate:(change specification) ~budget)
        with
        | Ok (Error _) -> true
        | Ok (Ok _) | Error _ -> false));
    let diagnostic = { source with file = "rebased.chatmd"; source_dir = "/retained" } in
    assert (
      match
        run 1000 (fun budget ->
          F.check_runtime
            specification
            ~candidate:{ specification with source = diagnostic }
            ~budget)
      with
      | Ok (Ok _) -> true
      | Ok (Error _) | Error _ -> false);
    let material = String.make 8192 's' in
    let with_secret value =
      { specification with
        secrets =
          Some
            { replacement = Set "redacted"; sources = [ Literal value ]; merge = Append }
      }
    in
    let left = with_secret material in
    let right = with_secret (material ^ "changed") in
    assert (
      match run 1000 (fun budget -> F.check_runtime left ~candidate:right ~budget) with
      | Ok (Error error) -> String.equal error.field "secrets"
      | Ok (Ok _) | Error _ -> false))
;;

let%test_unit "deep policy traversal is stack safe and rejects before exceeding work" =
  Eio_main.run (fun _ ->
    let deep =
      List.fold (List.init 100_000 ~f:Fn.id) ~init:S.Any_command ~f:(fun matcher _ ->
        S.Not matcher)
    in
    let candidate =
      { specification with
        policy =
          Some
            { default = Inherit
            ; rules =
                [ { id = "deep"; action = Allow; matcher = deep; override = false } ]
            ; merge = Append
            }
      }
    in
    assert (
      match run 100 (fun budget -> F.check_runtime candidate ~candidate ~budget) with
      | Error B.Error.Work_limit -> true
      | Error Invalid_limit | Ok _ -> false);
    assert (
      match run 500_000 (fun budget -> F.check_runtime candidate ~candidate ~budget) with
      | Ok (Ok _) -> true
      | Ok (Error _) | Error _ -> false))
;;

let allowed config =
  let invocation : E.invocation =
    { request =
        Shell_access.Request.command
          (Shell_access.Command.create "/och236-absent-executable" [])
    ; input = Empty
    ; rationale = None
    ; origin = Host "code-view-fixture"
    }
  in
  match E.run config invocation with
  | Error (Denied _) -> false
  | Error (Resolution_error _) -> true
  | Ok _ | Error _ -> failwith "fixture reached an unexpected execution phase"
;;

let host env : Host.t =
  let path = Eio.Stdenv.cwd env in
  { env
  ; workspace = path
  ; tool_dir = path
  ; prompt_dir = path
  ; session_dir = path
  ; cache_dir = path
  ; home = path
  ; source_dirs = String.Map.singleton source.file path
  ; process_environment = [||]
  ; session_id = "construction"
  }
;;

let context : Shell_access.Context.t =
  let fingerprint : Shell_access.Executable.fingerprint =
    { device = 0
    ; inode = 0
    ; mode = 0
    ; uid = 0
    ; gid = 0
    ; size = 0L
    ; mtime = 0.
    ; sha256 = "fixture"
    }
  in
  { request_id = "fixture"
  ; runtime_id = "child"
  ; manifest_sha256 = "original"
  ; command = Shell_access.Command.create "fixture" []
  ; executable =
      { requested = "fixture"
      ; path = "/fixture"
      ; canonical_path = "/fixture"
      ; trusted = false
      ; fingerprint
      }
  ; cwd = "/fixture"
  ; environment = [||]
  ; request_kind = Structured
  ; stdin_kind = Empty
  ; stdin_sha256 = None
  ; stdin_bytes = 0
  ; script_sha256 = None
  ; script_preview = None
  ; origin = Host "matcher-fixture"
  ; effects = []
  ; capabilities = Shell_access.Capabilities.read_only ~roots:[]
  ; policy_action = None
  ; policy_matches = []
  ; session_id = None
  }
;;

let%test_unit "construction preserves callback order and executable short circuit" =
  Eio_main.run (fun env ->
    let compiled = ref [] in
    let executed = ref [] in
    let construction_complete = ref false in
    let checkpoint () =
      if !construction_complete then failwith "construction callback escaped"
    in
    let custom ~action:_ (matcher : S.chatml_matcher) =
      compiled := matcher.script :: !compiled;
      Shell_access.Matcher.custom (fun _ ->
        executed := matcher.script :: !executed;
        String.equal matcher.script "a" || String.equal matcher.script "c")
    in
    let leaf name =
      S.Chatml_match
        { script = name; function_ = "match_command"; failure = Deny_failure }
    in
    List.iter
      [ ( S.All [ leaf "a"; Any [ leaf "b"; leaf "c" ]; Not (leaf "d") ]
        , [ "a"; "b"; "c"; "d" ]
        , true )
      ; S.All [ leaf "b"; leaf "a" ], [ "b" ], false
      ; S.Any [ leaf "a"; leaf "b" ], [ "a" ], true
      ]
      ~f:(fun (specification, expected_execution, expected_result) ->
        construction_complete := false;
        compiled := [];
        executed := [];
        let matcher =
          Lowering.matcher
            ~checkpoint
            (host env)
            source
            ~chatml_matcher:custom
            ~action:Allow
            specification
        in
        let expected_construction =
          match specification with
          | All [ _; Any _; _ ] -> [ "a"; "b"; "c"; "d" ]
          | All _ -> [ "b"; "a" ]
          | Any _ -> [ "a"; "b" ]
          | _ -> failwith "unexpected fixture compound"
        in
        assert (List.equal String.equal (List.rev !compiled) expected_construction);
        construction_complete := true;
        assert (Bool.equal (Shell_access.Matcher.matches matcher context) expected_result);
        assert (List.equal String.equal (List.rev !executed) expected_execution)))
;;

let%test_unit "deep construction and borrowed lowering-error checkpoint identity" =
  Eio_main.run (fun env ->
    let host = host env in
    let custom ~action:_ _ = Shell_access.Matcher.any in
    let depth = 100_000 in
    let deep =
      List.fold (List.init depth ~f:Fn.id) ~init:S.Any_command ~f:(fun child _ ->
        S.Not child)
    in
    ignore (Lowering.matcher host source ~chatml_matcher:custom ~action:Allow deep);
    (* Construction alone is stack-safe; this intentionally does not execute
       arbitrary-depth nested matcher closures. *)
    let original =
      try
        ignore
          (Lowering.matcher
             host
             source
             ~chatml_matcher:custom
             ~action:Allow
             (S.Program_regex "["));
        None
      with
      | error -> Some error
    in
    let original =
      match original with
      | Some error -> error
      | None -> failwith "invalid regular expression fixture unexpectedly compiled"
    in
    let same =
      try
        ignore
          (Lowering.policy
             ~checkpoint:(fun () -> raise original)
             host
             ~source
             ~chatml_matcher:custom
             { default = Set Allow; rules = []; merge = Append });
        false
      with
      | error -> phys_equal error original
    in
    assert same)
;;

let%expect_test
    "new code replaces specialist lifetime but cannot revive an expired invocation"
  =
  Eio_main.run (fun env ->
    let owner () =
      E.config
        ~env
        ~runtime_id:"same-name"
        ~manifest_sha256:"original"
        ~policy:(Shell_access.Policy.create [])
        ~capabilities:(Shell_access.Capabilities.read_only ~roots:[])
        ()
    in
    let original = owner () in
    let replacement manifest check =
      E.Code.create
        ~manifest_sha256:manifest
        ~policy:(Shell_access.Policy.create [])
        ~reviewer:None
        ~reviewer_with_metadata:None
        ~analyzers:[]
        ~interceptors:[]
        ~audit:Shell_access.Audit.ignore
        ~check
    in
    let code_a = replacement "A" (fun () -> Error "old code owner closed") in
    let code_b = replacement "B" (fun () -> Ok ()) in
    let fresh = E.with_code (E.with_code original code_a) code_b in
    let scoped =
      E.with_execution_check original ~check:(fun () -> Error "invocation expired")
    in
    let escaped = E.with_code scoped code_b in
    print_s
      [%sexp
        { selected_manifest = (E.manifest_sha256 fresh : string)
        ; original_manifest = (E.manifest_sha256 original : string)
        ; original_owner_preserved = (E.same_resource_owner original fresh : bool)
        ; equal_name_is_same_owner = (E.same_resource_owner original (owner ()) : bool)
        ; fresh_from_original_allowed = (allowed fresh : bool)
        ; expired_invocation_allowed = (allowed escaped : bool)
        }]);
  [%expect
    {|
    ((selected_manifest B) (original_manifest original)
     (original_owner_preserved true) (equal_name_is_same_owner false)
     (fresh_from_original_allowed true) (expired_invocation_allowed false))
    |}]
;;
