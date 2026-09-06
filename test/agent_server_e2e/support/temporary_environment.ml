open Core

type roots =
  { root : string
  ; home : string
  ; data : string
  ; cache : string
  ; config : string
  ; workspaces : string
  ; logs : string
  ; artifacts : string
  ; sockets : string
  ; temporary : string
  }
[@@deriving sexp]

type t =
  { env : Eio_unix.Stdenv.base
  ; roots : roots
  ; failure_bundle : Artifact_bundle.t
  }

let roots t = t.roots
let path t native_path = Eio.Path.(Eio.Stdenv.fs t.env / native_path)
let fs t = Eio.Stdenv.fs t.env
let register_secret t secret = Artifact_bundle.register_secret t.failure_bundle secret
let temporary_base () = Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp"

let unique_suffix () =
  Agent_protocol.Id.Transaction.create ()
  |> Agent_protocol.Id.Transaction.to_string
  |> Fn.flip String.prefix 20
;;

let root_path suffix = Filename.concat (temporary_base ()) ("oe2e-" ^ suffix)
let socket_root_path suffix = Filename.concat "/tmp" ("oe2e-s-" ^ suffix)
let create_directory fs path = Eio.Path.mkdir ~perm:0o700 Eio.Path.(fs / path)

let make_roots () =
  let suffix = unique_suffix () in
  let root = root_path suffix in
  let make name = Filename.concat root name in
  { root
  ; home = make "home"
  ; data = make "data"
  ; cache = make "cache"
  ; config = make "config"
  ; workspaces = make "workspaces"
  ; logs = make "logs"
  ; artifacts = make "artifacts"
  ; sockets = socket_root_path suffix
  ; temporary = make "tmp"
  }
;;

let create_roots ~mkdir ~owned roots =
  List.iter [ roots.root; roots.sockets ] ~f:(fun root ->
    Eio.Cancel.protect (fun () ->
      mkdir root;
      owned := root :: !owned));
  List.iter
    [ roots.home
    ; roots.data
    ; roots.cache
    ; roots.config
    ; roots.workspaces
    ; roots.logs
    ; roots.artifacts
    ; roots.temporary
    ]
    ~f:mkdir
;;

let cleanup ~rmtree paths =
  Eio.Cancel.protect (fun () ->
    List.map paths ~f:(fun path -> Or_error.try_with (fun () -> rmtree path))
    |> Or_error.combine_errors_unit
    |> Or_error.ok_exn)
;;

let safe_scenario_name scenario =
  String.map scenario ~f:(function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.') as value -> value
    | _ -> '-')
;;

let failure_root env override =
  match override, Sys.getenv "OCHAT_E2E_ARTIFACT_ROOT" with
  | Some root, _ | None, Some root -> root
  | None, None ->
    Filename.concat
      (Eio.Path.native_exn (Eio.Stdenv.cwd env))
      "_build/agent-e2e-artifacts"
;;

let preserve_failure t ~scenario ~failure_artifact_root exn =
  let destination =
    Filename.concat
      (failure_root t.env failure_artifact_root)
      (safe_scenario_name scenario ^ "-" ^ unique_suffix ())
  in
  Artifact_bundle.write_text
    t.failure_bundle
    ~name:"failure.sexp"
    ~contents:(Exn.to_string exn)
  |> Or_error.ok_exn;
  Artifact_bundle.preserve t.failure_bundle ~destination |> Or_error.ok_exn
;;

let run t ~scenario ~failure_artifact_root f =
  try f t with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let preservation =
      Eio.Cancel.protect (fun () ->
        Or_error.try_with (fun () ->
          preserve_failure t ~scenario ~failure_artifact_root exn))
    in
    let diagnostic =
      (Exn.to_string exn
       ^
       match preservation with
       | Ok () -> ""
       | Error error -> "\nArtifact preservation failed: " ^ Error.to_string_hum error)
      |> Artifact_bundle.redact t.failure_bundle
    in
    Exn.raise_with_original_backtrace (Failure diagnostic) backtrace
;;

let with_operations
      ~mkdir
      ~rmtree
      ?(scenario = "temporary-environment")
      ?failure_artifact_root
      ~env
      f
  =
  let roots = make_roots () in
  let owned = ref [] in
  let failure_bundle =
    Artifact_bundle.create ~fs:(Eio.Stdenv.fs env) ~root:roots.artifacts ~secrets:[]
  in
  let t = { env; roots; failure_bundle } in
  Exn.protect
    ~f:(fun () ->
      create_roots ~mkdir ~owned roots;
      run t ~scenario ~failure_artifact_root f)
    ~finally:(fun () ->
      try cleanup ~rmtree (List.rev !owned) with
      | exn -> failwith (Artifact_bundle.redact failure_bundle (Exn.to_string exn)))
;;

let with_ ?scenario ?failure_artifact_root ~env f =
  let fs = Eio.Stdenv.fs env in
  with_operations
    ~mkdir:(create_directory fs)
    ~rmtree:(fun root -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / root))
    ?scenario
    ?failure_artifact_root
    ~env
    f
;;

module For_testing = struct
  let with_operations = with_operations
end

let environment_key entry =
  match String.lsplit2 entry ~on:'=' with
  | Some (key, _) -> key
  | None -> entry
;;

let restricted_keys =
  String.Set.of_list
    [ "ANTHROPIC_API_KEY"
    ; "AWS_ACCESS_KEY_ID"
    ; "AWS_SECRET_ACCESS_KEY"
    ; "AZURE_OPENAI_API_KEY"
    ; "COHERE_API_KEY"
    ; "GOOGLE_API_KEY"
    ; "OPENAI_API_KEY"
    ; "OPENAI_ORG_ID"
    ]
;;

let child_environment t ~base =
  let override_keys =
    String.Set.of_list
      [ "HOME"
      ; "OCHAT_E2E_ROOT"
      ; "OCHAT_E2E_SOCKET_ROOT"
      ; "OCHAT_AGENT_DATA_ROOT"
      ; "TMPDIR"
      ; "XDG_CACHE_HOME"
      ; "XDG_CONFIG_HOME"
      ]
  in
  let inherited =
    Array.to_list base
    |> List.filter ~f:(fun entry ->
      let key = environment_key entry in
      not (Set.mem restricted_keys key || Set.mem override_keys key))
  in
  let roots = t.roots in
  Array.of_list
    (inherited
     @ [ "HOME=" ^ roots.home
       ; "OCHAT_E2E_ROOT=" ^ roots.root
       ; "OCHAT_E2E_SOCKET_ROOT=" ^ roots.sockets
       ; "OCHAT_AGENT_DATA_ROOT=" ^ roots.data
       ; "TMPDIR=" ^ roots.temporary
       ; "XDG_CACHE_HOME=" ^ roots.cache
       ; "XDG_CONFIG_HOME=" ^ roots.config
       ])
;;
