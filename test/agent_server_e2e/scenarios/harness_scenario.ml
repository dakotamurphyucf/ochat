open Core
module Artifact_bundle = Support.Artifact_bundle
module Temporary_environment = Support.Temporary_environment

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let environment_value environment key =
  Array.find_map environment ~f:(fun entry ->
    match String.lsplit2 entry ~on:'=' with
    | Some (entry_key, value) when String.equal key entry_key -> Some value
    | Some _ | None -> None)
;;

let require_environment_value environment key expected =
  match environment_value environment key with
  | Some actual when String.equal actual expected -> ()
  | Some actual ->
    raise_s
      [%sexp
        "child environment value differs"
      , { key : string; expected : string; actual : string }]
  | None -> raise_s [%sexp "child environment value is missing", (key : string)]
;;

let root_is_private environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let stat =
    Eio.Path.stat ~follow:false (Temporary_environment.path environment roots.root)
  in
  Poly.equal stat.kind `Directory && Int.equal (stat.perm land 0o077) 0
;;

let load_e2e_dune_file env =
  let cwd = Eio.Stdenv.cwd env in
  [ Eio.Path.(cwd / "test/agent_server_e2e/dune"); Eio.Path.(cwd / "dune") ]
  |> List.find_map ~f:(fun path ->
    match Eio.Path.kind ~follow:true path with
    | `Regular_file -> Some (Eio.Path.load path)
    | `Not_found
    | `Unknown
    | `Fifo
    | `Character_special
    | `Directory
    | `Block_device
    | `Symbolic_link
    | `Socket -> None)
  |> Option.value_exn
;;

let test_dune_isolation env _environment =
  let contents = load_e2e_dune_file env in
  require
    (not (String.is_substring contents ~substring:"(alias runtest)"))
    "E2E Dune rules are attached to runtest";
  require
    (not (String.is_substring contents ~substring:"(test\n"))
    "E2E runner is declared as a Dune test"
;;

let test_concurrent_roots env environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  Temporary_environment.with_ ~env (fun nested ->
    let nested_roots : Temporary_environment.roots = Temporary_environment.roots nested in
    require
      (not (String.equal roots.root nested_roots.root))
      "concurrent temporary environments share one root")
;;

let test_cleanup_sentinel env environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let sentinel = Filename.concat roots.root "external-sentinel" in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temporary_environment.path environment sentinel)
    "outside nested environment";
  Temporary_environment.with_ ~env ignore;
  require
    (Poly.equal
       (Eio.Path.kind ~follow:false (Temporary_environment.path environment sentinel))
       `Regular_file)
    "nested cleanup removed an external sentinel"
;;

let test_child_environment _env environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let child =
    Temporary_environment.child_environment
      environment
      ~base:
        [| "PATH=/usr/bin:/bin"
         ; "HOME=/real/home"
         ; "OPENAI_API_KEY=ambient-secret"
         ; "ANTHROPIC_API_KEY=ambient-secret-two"
         ; "XDG_CACHE_HOME=/real/cache"
        |]
  in
  require_environment_value child "HOME" roots.home;
  require_environment_value child "OCHAT_E2E_ROOT" roots.root;
  require_environment_value child "OCHAT_E2E_SOCKET_ROOT" roots.sockets;
  require_environment_value child "OCHAT_AGENT_DATA_ROOT" roots.data;
  require_environment_value child "TMPDIR" roots.temporary;
  require_environment_value child "XDG_CACHE_HOME" roots.cache;
  require_environment_value child "XDG_CONFIG_HOME" roots.config;
  require (root_is_private environment) "temporary root is not private";
  require
    (Option.is_none (environment_value child "OPENAI_API_KEY"))
    "OpenAI credential survived environment sanitization";
  require
    (Option.is_none (environment_value child "ANTHROPIC_API_KEY"))
    "Anthropic credential survived environment sanitization"
;;

let artifact_bundle environment root secret =
  Artifact_bundle.create
    ~fs:(Temporary_environment.fs environment)
    ~root
    ~secrets:[ secret ]
;;

let test_bundle_redaction environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let secret = "e2e-secret-value" in
  let encoded = Base64.encode_exn secret in
  let bundle = artifact_bundle environment roots.artifacts secret in
  Artifact_bundle.write_text
    bundle
    ~name:"failure.txt"
    ~contents:("plain=" ^ secret ^ " encoded=" ^ encoded)
  |> Or_error.ok_exn;
  Artifact_bundle.validate_redaction bundle |> Or_error.ok_exn;
  let retained = Filename.concat roots.logs "retained-artifacts" in
  Artifact_bundle.preserve bundle ~destination:retained |> Or_error.ok_exn;
  require
    (Result.is_error
       (Artifact_bundle.write_text bundle ~name:"../escape.txt" ~contents:"unsafe"))
    "artifact bundle accepted a traversal name";
  let contents =
    Eio.Path.load
      (Temporary_environment.path
         environment
         (Filename.concat roots.artifacts "failure.txt"))
  in
  require
    (String.is_substring contents ~substring:"<redacted>")
    "artifact did not contain a redaction marker";
  let retained_contents =
    Eio.Path.load
      (Temporary_environment.path environment (Filename.concat retained "failure.txt"))
  in
  require
    (String.equal contents retained_contents)
    "retained artifact differs from its sanitized source"
;;

let test_automatic_failure_retention env environment =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let retention_root = Filename.concat roots.logs "automatic-failures" in
  let secret = "automatic-failure-secret" in
  let failure =
    Result.try_with (fun () ->
      Temporary_environment.with_
        ~scenario:"retention-probe"
        ~failure_artifact_root:retention_root
        ~env
        (fun nested ->
           Temporary_environment.register_secret nested secret;
           raise_s [%sexp "intentional retention probe", (secret : string)]))
  in
  require (Result.is_error failure) "failure-retention probe did not raise";
  let emitted = Exn.to_string (Result.error failure |> Option.value_exn) in
  require
    (not (String.is_substring emitted ~substring:secret))
    "outward failure disclosed its registered secret";
  let entries =
    Eio.Path.read_dir (Temporary_environment.path environment retention_root)
  in
  let retained = List.hd_exn entries |> Filename.concat retention_root in
  let report =
    Eio.Path.load
      (Temporary_environment.path environment (Filename.concat retained "failure.sexp"))
  in
  require
    (not (String.is_substring report ~substring:secret))
    "retained failure report disclosed its registered secret";
  require
    (String.is_substring report ~substring:"<redacted>")
    "retained failure report omitted its redaction marker"
;;

let test_artifact_redaction env environment =
  test_bundle_redaction environment;
  test_automatic_failure_retention env environment
;;

let require_removed env roots =
  List.iter [ roots.Temporary_environment.root; roots.sockets ] ~f:(fun root ->
    require
      (Poly.equal
         (Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / root))
         `Not_found)
      "owned temporary root survived cleanup")
;;

let test_cancelled_cleanup env _environment =
  let captured = ref None in
  let outcome =
    Result.try_with (fun () ->
      Eio.Cancel.sub (fun cancel ->
        Temporary_environment.with_ ~env (fun temporary ->
          captured := Some (Temporary_environment.roots temporary);
          Eio.Cancel.cancel cancel Exit;
          Eio.Fiber.yield ())))
  in
  require
    (match outcome with
     | Error (Eio.Cancel.Cancelled _) -> true
     | _ -> false)
    "temporary environment swallowed cancellation";
  require_removed env (Option.value_exn !captured)
;;

let test_partial_setup env _environment =
  List.iter [ 2; 3; 5 ] ~f:(fun fail_at ->
    let created = ref [] in
    let calls = ref 0 in
    let mkdir root =
      Int.incr calls;
      if !calls = fail_at then failwith "injected mkdir failure";
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
      created := root :: !created
    in
    let rmtree root =
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)
    in
    let outcome =
      Result.try_with (fun () ->
        Temporary_environment.For_testing.with_operations ~mkdir ~rmtree ~env ignore)
    in
    require (Result.is_error outcome) "partial allocation unexpectedly succeeded";
    List.iter !created ~f:(fun root ->
      require
        (Poly.equal
           (Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / root))
           `Not_found)
        "partial allocation leaked an owned path"))
;;

let test_independent_cleanup env _environment =
  let captured = ref None in
  let attempted = ref [] in
  let remove root =
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)
  in
  let rmtree root =
    attempted := root :: !attempted;
    if List.length !attempted = 1 then failwith "injected cleanup failure";
    remove root
  in
  Exn.protect
    ~f:(fun () ->
      let outcome =
        Result.try_with (fun () ->
          Temporary_environment.For_testing.with_operations
            ~mkdir:(fun root ->
              Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root))
            ~rmtree
            ~env
            (fun temporary -> captured := Some (Temporary_environment.roots temporary)))
      in
      require (Result.is_error outcome) "cleanup failure was hidden";
      let roots = Option.value_exn !captured in
      require (List.length !attempted = 2) "first cleanup failure skipped the other root";
      require
        (Poly.equal
           (Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / roots.sockets))
           `Not_found)
        "socket root was not removed after main-root cleanup failed")
    ~finally:(fun () ->
      Option.iter !captured ~f:(fun roots ->
        Eio.Cancel.protect (fun () -> List.iter [ roots.root; roots.sockets ] ~f:remove)))
;;

let cases =
  [ "dune.runtest-isolation", test_dune_isolation
  ; "paths.concurrent-roots", test_concurrent_roots
  ; "environment.no-ambient-home", test_child_environment
  ; "cleanup.external-sentinel", test_cleanup_sentinel
  ; "artifact.redaction", test_artifact_redaction
  ; "cleanup.cancelled-context", test_cancelled_cleanup
  ; "cleanup.partial-allocation", test_partial_setup
  ; "cleanup.independent-roots", test_independent_cleanup
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown harness-isolation case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"harness-isolation" ~env (fun environment ->
    List.iter (select case) ~f:(fun (_name, test) -> test env environment);
    let roots : Temporary_environment.roots = Temporary_environment.roots environment in
    print_s
      [%sexp
        { scenario = ("harness-isolation" : string)
        ; selected_case = (case : string option)
        ; root_private = (root_is_private environment : bool)
        ; child_home_isolated = (not (String.equal roots.home "/real/home") : bool)
        ; artifact_redaction = (true : bool)
        ; cleanup_scope = ("owned-root" : string)
        }])
;;
