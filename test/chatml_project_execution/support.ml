open Core
module P = Chatml_project_prepare
module B = Chatml_project_build
module S = Chatml_source
module Contract = Chatml.Chatml_compiler_contract

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected project execution fixture rejection"
;;

let path text = Source.Portable_path.create text |> Or_error.ok_exn

let id name =
  S.Id.create ~package_namespace:"shared-counter" ~relative_path:name |> Or_error.ok_exn
;;

let save directory name contents =
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(directory / name) contents
;;

let contract () = Contract.for_target One_off_v1 ~checkpoint:ignore

let root_limits () =
  Chatml_local_roots.Limits.create
    ~max_locator_bytes:4096
    ~max_locator_components:64
    ~max_anchors:8
    ~max_retained_roots:32
    ~max_work_steps:10_000
  |> Or_error.ok_exn
;;

let composition_limits () =
  B.Limits.create
    ~max_work_steps:1_000_000
    ~max_input_bytes:(Some 1_000_000)
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let policy () =
  P.Policy.create
    ~roots:(root_limits ())
    ~local:
      (Chatml_local_capture.Limits.create
         ~max_directory_entries:100
         ~max_sources:100
         ~max_source_bytes:100_000
         ~max_total_source_bytes:1_000_000
         ~max_path_components:64
         ~max_work_steps:1_000_000
       |> Or_error.ok_exn)
    ~git_objects:
      (Chatml_git_catalog.Limits.create
         ~max_object_bytes:100_000
         ~max_total_object_bytes:1_000_000
         ~max_objects:100
         ~max_tree_entries:100
         ~max_sources:100
         ~max_source_bytes:100_000
         ~max_total_source_bytes:1_000_000
         ~max_path_components:64
         ~max_work_steps:1_000_000
         ~document:Document_schema.Limits.default
       |> Or_error.ok_exn)
    ~git_native:
      (Chatml_git_host.Limits.create
         ~metadata_file_bytes:100_000
         ~pack_file_bytes:1_000_000
         ~max_acquisition_payload_bytes:10_000_000
         ~max_request_bytes:100_000
         ~max_output_bytes:1_000_000
         ~max_work_steps:100_000
         ~wall_time:(Time_ns.Span.of_sec 60.)
         ~cpu_seconds_per_process:10
         ~memory_bytes_per_process:100_000_000
         ~open_files_per_process:64
       |> Or_error.ok_exn)
    ~composition:(composition_limits ())
;;

let limits () =
  P.Limits.create
    ~max_packages:100
    ~max_dependency_visits:100
    ~max_source_count:100
    ~max_source_bytes:1_000_000
    ~max_local_directory_entries:100
    ~max_git_objects:100
    ~max_git_object_bytes:1_000_000
    ~max_git_tree_entries:100
    ~max_native_request_bytes:100_000
    ~max_native_output_bytes:1_000_000
    ~max_acquisition_payload_reserved:10_000_000
    ~max_work_steps:10_000_000
    ~max_prepared_input_bytes:1_000_000
    ~wall_time:(Time_ns.Span.of_sec 60.)
  |> Or_error.ok_exn
;;

let with_project f =
  Eio_main.run (fun env ->
    let bytes = Cstruct.create 16 in
    Eio.Flow.read_exact env#secure_random bytes;
    let suffix =
      Cstruct.to_string bytes
      |> String.to_list
      |> List.map ~f:(fun byte -> sprintf "%02x" (Char.to_int byte))
      |> String.concat
    in
    let directory = Eio.Path.(env#fs / "/tmp" / ("och193-project-" ^ suffix)) in
    Eio.Path.mkdir ~perm:0o700 directory;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
      (fun () ->
         Eio.Path.with_open_dir directory (fun directory ->
           let fixture =
             Eio.Path.(
               env#fs
               / (native_exn env#cwd
                  ^ "/../../docs-src/examples/chatml/projects/shared-counter"))
           in
           (* Dune presents documented fixture files as build-tree symlinks. This is
              test setup under the explicit fs capability; the admitted temporary
              package contains regular copies and production capture stays nofollow. *)
           let rec copy relative =
             let source = Eio.Path.(fixture / relative) in
             match Eio.Path.kind ~follow:true source with
             | `Directory ->
               if not (String.is_empty relative)
               then Eio.Path.mkdir ~perm:0o700 Eio.Path.(directory / relative);
               List.iter (Eio.Path.read_dir source) ~f:(fun name ->
                 copy (if String.is_empty relative then name else relative ^ "/" ^ name))
             | `Regular_file -> save directory relative (Eio.Path.load source)
             | _ -> failwith "documented fixture has unexpected member kind"
           in
           copy "";
           let anchor =
             Chatml_local_roots.Anchor.create
               directory
               ~host_prefix:"/project"
               ~max_path_components:64
               ~limits:(root_limits ())
               ~checkpoint:ignore
             |> ok
           in
           let host =
             P.Host.create
               [ anchor ]
               ~git_store:None
               ~git_grants:[]
               ~max_grants:0
               ~max_grant_bytes:0
             |> Or_error.ok_exn
           in
           f (env :> Eio_unix.Stdenv.base) directory host)))
;;

let prepare host ~environment ~operation ~clock =
  let request =
    P.Request.create
      (Local { locator = "/project"; project = path "chatml.project.json" })
      ~operation
      ~selection:(Entry { name = "main"; environment })
      ~entry:Declared
      ~contract:(contract ())
      ~lock_id:(id "selected.lock.json")
      ~max_request_bytes:4096
    |> Or_error.ok_exn
  in
  let tools =
    Chat_response.Tool_capability.create
      ~owner:"project-execution"
      ~resource_fingerprint:
        (S.Digest.of_contents "empty" ~checkpoint:ignore |> S.Digest.to_hex)
      []
    |> ok
  in
  P.prepare
    host
    request
    ~tools
    ~policy:(policy ())
    ~limits:(limits ())
    ~clock
    ~checkpoint:ignore
  |> ok
  |> P.build
;;

let compiled env build =
  match
    Chatml_compilation.compile_prepared_with_policy
      (B.prepared build)
      ~policy:(Bounded Chatml_compilation.default_limits)
      ~max_dependency_depth:32
      ~env
      ~target:One_off_v1
      ()
  with
  | Ok program -> program
  | Error error -> failwith error.message
;;

let run env surface program =
  match
    Chatml_execution.run_prepared
      program
      ~env
      ~config:(Chatml_host_runtime.default_runtime_config ~surface ())
      ~entrypoint:"main"
      ~arguments:[ Chatml.Chatml_lang.VVariant ("Null", []) ]
      ()
  with
  | Ok value -> value
  | Error error -> failwith error.message
;;

let marked_surface log =
  let module Builtin = Chatml.Chatml_builtin_spec in
  let module L = Chatml.Chatml_lang in
  let surface = Chatml.Chatml_extension_surface.one_off_v1 in
  { surface with
    globals =
      List.map surface.globals ~f:(fun (builtin : Builtin.builtin) ->
        if String.equal builtin.name "to_string"
        then
          { builtin with
            impl =
              (fun arguments ->
                (match arguments with
                 | [ L.VString name ] -> log := name :: !log
                 | _ -> ());
                builtin.impl arguments)
          }
        else builtin)
  }
;;

let prepare_library host ~clock =
  let entry_id =
    S.Id.create ~package_namespace:"host" ~relative_path:"moderator.chatml"
    |> Or_error.ok_exn
  in
  let source =
    S.create
      entry_id
      ~contents:
        "let initial_state = Common.next() let on_event ctx state event = \
         Task.pure(Common.next())"
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let contract = Contract.for_target Moderator_v1 ~checkpoint:ignore in
  let request =
    P.Request.create
      (Local { locator = "/project"; project = path "chatml.project.json" })
      ~operation:Initial_prepare
      ~selection:(Library { environment = Some "branch" })
      ~entry:(Captured source)
      ~contract
      ~lock_id:(id "moderator.lock.json")
      ~max_request_bytes:4096
    |> Or_error.ok_exn
  in
  let tools =
    Chat_response.Tool_capability.create
      ~owner:"project-execution"
      ~resource_fingerprint:
        (S.Digest.of_contents "empty" ~checkpoint:ignore |> S.Digest.to_hex)
      []
    |> ok
  in
  P.prepare
    host
    request
    ~tools
    ~policy:(policy ())
    ~limits:(limits ())
    ~clock
    ~checkpoint:ignore
  |> ok
  |> P.build
;;
