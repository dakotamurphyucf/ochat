open Core
module Host = Chatml_git_host
module Process = Shell_access.Bounded_process

external virtual_bytes : unit -> int = "ochat_test_virtual_bytes"

let memory_cap () =
  let inherited = virtual_bytes () in
  (* Git's native loader/linked libraries and worker creation require test-only
     explicit headroom beyond the observed OCaml floor. Production caps remain
     exactly those supplied by the host. *)
  let headroom = 1024 * 1024 * 1024 in
  assert (inherited <= Int.max_value - headroom);
  inherited + headroom
;;

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "Git host admission failed", (error : Host.Error.t)]
;;

let admitted = function
  | Ok value -> value
  | Error (`Host error) ->
    raise_s [%sexp "Git host bracket failed", (error : Host.Error.t)]
  | Error (`Callback ()) -> failwith "unexpected callback rejection"
;;

let deployment_directory () =
  match Sys.getenv "OCHAT_TEST_GIT_2_56_DIRECTORY" with
  | Some path when String.is_prefix path ~prefix:"/" -> path
  | None | Some _ ->
    failwith
      "required Git2.56 qualification input missing: OCHAT_TEST_GIT_2_56_DIRECTORY must \
       be an absolute directory containing built git and git-remote-https"
;;

let native path = Eio.Path.native_exn path

let process_limits () =
  Process.Limits.create
    ~maximum_request_bytes:65_536
    ~maximum_output_bytes:1_048_576
    ~maximum_wall_time:(Time_ns.Span.of_sec 10.)
    ~cpu_seconds_per_process:10
    ~memory_bytes_per_process:(memory_cap ())
    ~file_size_bytes:16_777_216
    ~open_files_per_process:128
  |> Or_error.ok_exn
;;

let run_git env cwd ~arguments ~stdin =
  let executable = Filename.concat (deployment_directory ()) "git" in
  let invocation =
    Process.create
      ~limits:(process_limits ())
      ~executable
      ~arguments
      ~environment:
        [ "PATH", deployment_directory ()
        ; "HOME", native cwd
        ; "GIT_CONFIG_NOSYSTEM", "1"
        ; "GIT_CONFIG_SYSTEM", "/dev/null"
        ; "GIT_CONFIG_GLOBAL", "/dev/null"
        ; "GIT_AUTHOR_NAME", "fixture"
        ; "GIT_AUTHOR_EMAIL", "fixture@example.invalid"
        ; "GIT_COMMITTER_NAME", "fixture"
        ; "GIT_COMMITTER_EMAIL", "fixture@example.invalid"
        ; "GIT_AUTHOR_DATE", "2000-01-01T00:00:00Z"
        ; "GIT_COMMITTER_DATE", "2000-01-01T00:00:00Z"
        ; "LC_ALL", "C"
        ]
      ~stdin
  in
  match invocation with
  | Error error -> raise_s [%sexp (error : Process.Error.t)]
  | Ok invocation ->
    Eio.Switch.run (fun sw ->
      let directory = Eio.Path.open_in ~sw Eio.Path.(cwd / ".") in
      let fd = Option.value_exn (Eio_unix.Resource.fd_opt directory) in
      match
        Process.run
          invocation
          ~cwd:fd
          ~clock:(Eio.Stdenv.mono_clock env)
          ~checkpoint:ignore
      with
      | Error error -> raise_s [%sexp (error : Process.Error.t)]
      | Ok outcome ->
        (match Process.Outcome.status outcome with
         | `Exited 0 -> Process.Outcome.stdout outcome
         | `Exited code -> raise_s [%sexp "fixture Git exited", (code : int)]
         | `Signaled signal -> raise_s [%sexp "fixture Git signaled", (signal : int)]))
;;

let store_limits =
  Host.Store_limits.create
    ~max_cache_repositories:8
    ~max_cache_payload_bytes:1_048_576
    ~max_managed_entries:1024
    ~max_path_bytes:4096
    ~max_work_steps:100_000
  |> Or_error.ok_exn
;;

let limits
      ?(metadata = 2048)
      ?(pack = 16_384)
      ?(payload = 262_144)
      ?(output = 65_536)
      ?(work = 2_000_000)
      ?(request = 65_536)
      ?(wall_time = 15.)
      ()
  =
  Host.Limits.create
    ~metadata_file_bytes:metadata
    ~pack_file_bytes:pack
    ~max_acquisition_payload_bytes:payload
    ~max_request_bytes:request
    ~max_output_bytes:output
    ~max_work_steps:work
    ~wall_time:(Time_ns.Span.of_sec wall_time)
    ~cpu_seconds_per_process:10
    ~memory_bytes_per_process:(memory_cap ())
    ~open_files_per_process:128
  |> Or_error.ok_exn
;;

let with_workspace ?(trust_fixture_certificate = false) f =
  Eio_main.run (fun env ->
    let temporary =
      Eio_unix.run_in_systhread (fun () -> Core_unix.mkdtemp "/tmp/ochat-git-host-XXXXXX")
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Fun.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      (fun () ->
         let helper = Eio.Path.(anchor / "helpers") in
         Eio.Path.mkdir ~perm:0o700 helper;
         List.iter
           [ "git", "git"
           ; "git-upload-pack", "git"
           ; "git-remote-https", "git-remote-https"
           ]
           ~f:(fun (name, target) ->
             Eio.Path.symlink
               ~link_to:(Filename.concat (deployment_directory ()) target)
               Eio.Path.(helper / name));
         let native_ca_bundle =
           if trust_fixture_certificate
           then (
             let ca = Eio.Path.(anchor / "test-ca.pem") in
             Eio.Path.save ~create:(`Exclusive 0o600) ca Tls_fixture.certificate;
             Some (native ca))
           else None
         in
         Eio.Path.with_open_dir helper (fun helper_root ->
           let deployment =
             Host.Deployment.admit_git_2_56_0
               helper_root
               ~native_helper_directory:(native helper)
               ~native_git_executable:(Filename.concat (native helper) "git")
               ~native_ca_bundle
               ~clock:(Eio.Stdenv.mono_clock env)
               ~probe_limits:(process_limits ())
               ~max_deployment_bytes:4096
               ~checkpoint:ignore
             |> ok
           in
           let cache = Eio.Path.(anchor / "store") in
           Eio.Path.mkdir ~perm:0o700 cache;
           Eio.Path.with_open_dir cache (fun root ->
             Eio.Switch.run (fun sw ->
               let store =
                 Host.Store.open_root
                   root
                   ~sw
                   ~deployment
                   ~clock:(Eio.Stdenv.mono_clock env)
                   ~limits:store_limits
                   ~checkpoint:ignore
                 |> ok
               in
               f env anchor cache root deployment store)))))
;;

let fixture env anchor format =
  let source =
    Eio.Path.(
      anchor
      /
      match format with
      | Host.Pin.Sha1 -> "origin-sha1"
      | Sha256 -> "origin-sha256")
  in
  Eio.Path.mkdir ~perm:0o700 source;
  ignore
    (run_git
       env
       source
       ~arguments:
         [ "init"
         ; "--bare"
         ; "--quiet"
         ; ("--object-format="
            ^
            match format with
            | Host.Pin.Sha1 -> "sha1"
            | Sha256 -> "sha256")
         ]
       ~stdin:"");
  let body = "let meaning = 42\n" in
  let blob =
    run_git env source ~arguments:[ "hash-object"; "-w"; "--stdin" ] ~stdin:body
    |> String.strip
  in
  let tree =
    run_git
      env
      source
      ~arguments:[ "mktree" ]
      ~stdin:("100644 blob " ^ blob ^ "\tcode.chatml\n")
    |> String.strip
  in
  let commit =
    run_git env source ~arguments:[ "commit-tree"; tree ] ~stdin:"fixture\n"
    |> String.strip
  in
  ignore
    (run_git env source ~arguments:[ "update-ref"; "refs/heads/main"; commit ] ~stdin:"");
  let pin = Host.Pin.create ~object_format:format ~commit |> Or_error.ok_exn in
  let oid = Host.Object_id.create ~object_format:format blob |> Or_error.ok_exn in
  source, pin, oid, body
;;

let acquire ?(checkpoint = ignore) store source pin ~limits ~f =
  let grant =
    Host.Grant.local ~repository:(native source) ~max_input_bytes:4096 |> Or_error.ok_exn
  in
  let request = Host.Request.prepare store ~grant ~pin ~limits |> ok in
  Host.with_acquisition request ~checkpoint ~f
;;

let expect_code result code =
  match result with
  | Error (`Host error) -> assert (Host.Error.equal_code (Host.Error.code error) code)
  | Ok _ | Error (`Callback _) -> failwith "expected bounded host rejection"
;;

let expect_raw repository oid body =
  let raw =
    Host.Repository.raw_object repository oid ~max_output_bytes:4096 ~checkpoint:ignore
    |> ok
  in
  assert (
    String.equal
      raw
      (Host.Object_id.to_string oid
       ^ " blob "
       ^ Int.to_string (String.length body)
       ^ "\n"
       ^ body
       ^ "\n"));
  raw
;;

let smart_git_response env source request =
  let module R = Https_fixture.Request in
  let body, content_type =
    match R.method_ request, R.target request with
    | "GET", "/repo/info/refs?service=git-upload-pack" ->
      let advertisement =
        run_git
          env
          source
          ~arguments:[ "upload-pack"; "--stateless-rpc"; "--advertise-refs"; "." ]
          ~stdin:""
      in
      ( "001e# service=git-upload-pack\n0000" ^ advertisement
      , "application/x-git-upload-pack-advertisement" )
    | "POST", "/repo/git-upload-pack" ->
      ( run_git
          env
          source
          ~arguments:[ "upload-pack"; "--stateless-rpc"; "." ]
          ~stdin:(R.body request)
      , "application/x-git-upload-pack-result" )
    | _ -> failwith "unexpected smart Git route"
  in
  { Https_fixture.Response.status = "200 OK"
  ; headers = [ "Content-Type", content_type ]
  ; body
  }
;;
