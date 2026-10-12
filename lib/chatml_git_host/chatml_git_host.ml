open Core
module Profile = Git_host_profile
module Storage = Git_host_storage
module Native = Git_host_native
module Attempt = Git_host_attempt
module Process = Shell_access.Bounded_process
module Pin = Chatml_project.Git_pin
module Error = Profile.Error
module Grant = Profile.Grant
module Object_id = Profile.Object_id
module Limits = Attempt.Limits

let storage_error = Profile.Error.of_storage_error
let stored result = Result.map_error result ~f:storage_error

let process_error = function
  | Process.Error.Timed_out -> Error.create Timed_out
  | Output_limit -> Error.create Limit_exceeded
  | Invalid_request -> Error.create Invalid_input
  | Unsupported_platform -> Error.create Unsupported_provider
  | Spawn_failed | Process_failed | Io_failed -> Error.create Process_failed
;;

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ()
;;

let protected f = Eio.Cancel.protect f

module Deployment = struct
  type t = Profile.Deployment.t

  let admit_git_2_56_0
        root
        ~native_helper_directory
        ~native_git_executable
        ~native_ca_bundle
        ~clock
        ~probe_limits
        ~max_deployment_bytes
        ~checkpoint
    =
    let open Result.Let_syntax in
    let%bind deployment =
      Profile.Deployment.inputs
        ~native_helper_directory
        ~native_git_executable
        ~native_ca_bundle
        ~max_bytes:max_deployment_bytes
    in
    let%bind directory = stored (Storage.borrow_deployment root ~checkpoint) in
    Fun.protect
      ~finally:(fun () -> Storage.close directory)
      (fun () ->
         let%bind names = stored (Storage.names directory ~max_entries:3 ~checkpoint) in
         let sorted = List.sort names ~compare:String.compare in
         if
           not
             (List.equal
                String.equal
                sorted
                (List.sort Profile.Deployment.helper_names ~compare:String.compare))
         then Error (Error.create Unsupported_profile)
         else (
           let%bind command =
             Result.map_error
               (Process.create
                  ~limits:probe_limits
                  ~executable:native_git_executable
                  ~arguments:Profile.Deployment.version_arguments
                  ~environment:
                    [ "PATH", native_helper_directory
                    ; "GIT_EXEC_PATH", native_helper_directory
                    ; "GIT_CONFIG_NOSYSTEM", "1"
                    ; "LC_ALL", "C"
                    ]
                  ~stdin:"")
               ~f:process_error
           in
           let%bind outcome =
             Storage.with_cwd directory ~f:(fun cwd ->
               Result.map_error
                 (Process.run command ~cwd ~clock ~checkpoint)
                 ~f:process_error)
           in
           match Process.Outcome.status outcome with
           | `Exited 0
             when Profile.Deployment.admitted_version (Process.Outcome.stdout outcome) ->
             Ok deployment
           | `Exited _ | `Signaled _ -> Error (Error.create Unsupported_profile)))
  ;;
end

module Owner = Git_host_store
module Store_limits = Owner.Limits
module Store = Owner
module Request = Owner.Request

module Receipt = struct
  type disposition =
    | Cached
    | Fetched
  [@@deriving equal, sexp_of]

  type t =
    { disposition : disposition
    ; payload_bytes_reserved : int
    ; request_bytes : int
    ; output_bytes : int
    ; work_steps : int
    }

  let disposition t = t.disposition
  let payload_bytes_reserved t = t.payload_bytes_reserved
  let request_bytes t = t.request_bytes
  let output_bytes t = t.output_bytes
  let work_steps t = t.work_steps
end

module Repository = Git_host_repository

let setup_workspace store ~checkpoint =
  let open Result.Let_syntax in
  let%bind names =
    stored (Storage.names (Owner.stages_directory store) ~max_entries:2 ~checkpoint)
  in
  if not (List.is_empty names)
  then Error (Error.create Cache_corrupt)
  else (
    let%bind workspace =
      stored
        (Storage.directory
           (Owner.stages_directory store)
           ~name:"active"
           ~create:true
           ~exclusive:true)
    in
    Fun.protect
      ~finally:(fun () -> Storage.close workspace)
      (fun () ->
         let%bind () =
           List.fold_result [ "home"; "xdg"; "tmp"; "empty" ] ~init:() ~f:(fun () name ->
             poll checkpoint;
             Result.map_error
               (Storage.with_directory
                  workspace
                  ~name
                  ~create:true
                  ~exclusive:true
                  ~f:(fun _ -> Ok ()))
               ~f:storage_error)
         in
         stored (Storage.create_file workspace ~name:"config" ~contents:"" ~checkpoint)))
;;

let cleanup_workspace store =
  protected (fun () ->
    List.iter [ "copy"; "active" ] ~f:(fun name ->
      match Storage.entry (Owner.stages_directory store) ~name with
      | Error Native.Code.Missing -> ()
      | Error _ -> ()
      | Ok _ ->
        ignore
          (Storage.remove_tree
             (Owner.stages_directory store)
             ~name
             ~max_entries:64
             ~max_depth:5
             ~checkpoint:(fun () -> ()))))
;;

let with_directory_owned result ~f =
  match result with
  | Error error -> Error (`Host (storage_error error))
  | Ok directory ->
    Fun.protect ~finally:(fun () -> Storage.close directory) (fun () -> f directory)
;;

let receipt attempt ~disposition ~reservation =
  { Receipt.disposition
  ; payload_bytes_reserved = reservation
  ; request_bytes = Attempt.request_bytes attempt
  ; output_bytes = Attempt.output_bytes attempt
  ; work_steps = Attempt.work_steps attempt
  }
;;

let cached store ~pin ~limits ~key ~attempt ~reservation ~checkpoint ~f =
  with_directory_owned
    (Storage.directory
       (Owner.cache_directory store)
       ~name:key
       ~create:false
       ~exclusive:false)
    ~f:(fun directory ->
      Repository.with_directory store ~directory ~pin ~attempt ~limits ~f:(fun repo ->
        let open Result.Let_syntax in
        let%bind _ =
          Result.map_error (Repository.validate repo ~checkpoint) ~f:(fun error ->
            `Host error)
        in
        let%bind value = Repository.verify repo ~f in
        let%map () =
          Result.map_error (Attempt.poll attempt ~checkpoint) ~f:(fun error ->
            `Host error)
        in
        value, receipt attempt ~disposition:Cached ~reservation))
;;

let fetched request ~key ~attempt ~checkpoint ~f =
  let store = Request.store request in
  let host result = Result.map_error result ~f:(fun error -> `Host error) in
  let open Result.Let_syntax in
  with_directory_owned
    (Storage.directory
       (Owner.stages_directory store)
       ~name:"active"
       ~create:false
       ~exclusive:false)
    ~f:(fun workspace ->
      with_directory_owned
        (Storage.directory workspace ~name:"repo" ~create:true ~exclusive:true)
        ~f:(fun directory ->
          let%bind value, inventory =
            Repository.with_directory
              store
              ~directory
              ~pin:(Request.pin request)
              ~attempt
              ~limits:(Request.limits request)
              ~f:(fun repo ->
                let%bind _ =
                  host
                    (Repository.run
                       repo
                       (Profile.Init (Pin.object_format (Request.pin request)))
                       ~file_size_bytes:
                         (Limits.metadata_file_bytes (Request.limits request))
                       ~max_output_bytes:
                         (Limits.maximum_request_bytes (Request.limits request))
                       ~checkpoint)
                in
                let%bind _ =
                  host
                    (Repository.run
                       repo
                       (Profile.Fetch (Request.grant request, Request.pin request))
                       ~file_size_bytes:(Limits.pack_file_bytes (Request.limits request))
                       ~max_output_bytes:
                         (Limits.maximum_request_bytes (Request.limits request))
                       ~checkpoint)
                in
                let%bind inventory = host (Repository.validate repo ~checkpoint) in
                let%map value = Repository.verify repo ~f in
                value, inventory)
          in
          let%bind () = host (Attempt.poll attempt ~checkpoint) in
          let%bind () = host (stored (Storage.validate_root (Owner.root store))) in
          with_directory_owned
            (Storage.directory
               (Owner.stages_directory store)
               ~name:"copy"
               ~create:true
               ~exclusive:true)
            ~f:(fun destination ->
              let%bind () =
                host
                  (stored
                     (Storage.copy_inventory directory ~destination inventory ~checkpoint))
              in
              let%bind () = host (Attempt.poll attempt ~checkpoint) in
              let%bind () = host (stored (Storage.validate_root (Owner.root store))) in
              (* Allocate final accounting/result BEFORE the atomic publication boundary.
           No callback or cancellation poll follows successful publication. *)
              let success =
                Ok
                  ( value
                  , receipt
                      attempt
                      ~disposition:Fetched
                      ~reservation:(Request.payload_bytes_reserved request) )
              in
              match Owner.publish_copy store ~key inventory with
              | Error error -> Error (`Host error)
              | Ok () -> success)))
;;

let timed store limits ~f =
  match
    Eio.Time.Timeout.run
      (Eio.Time.Timeout.seconds
         (Owner.clock store)
         (Time_ns.Span.to_sec
            (let t = limits in
             (* The whole-attempt timer shares the same literal limits as the process ledger. *)
             Attempt.Limits.wall_time t)))
      (fun () ->
         Result.map_error (f ()) ~f:(function
           | `Host error -> `Host error
           | `Callback error -> `Callback error))
  with
  | Ok _ as success -> success
  | Error `Timeout -> Error (`Host (Error.create Timed_out))
  | Error (`Host _ | `Callback _) as failure -> failure
;;

let cache_key attempt ~limits ~repository ~pin ~checkpoint =
  let bytes = String.length repository in
  if
    bytes = 0
    || bytes > Limits.maximum_request_bytes limits
    || bytes > Int.max_value - 256
  then Error (Error.create Limit_exceeded)
  else
    let open Result.Let_syntax in
    let%bind () = Attempt.reserve_work attempt (bytes + 256) in
    Profile.cache_key
      ~repository
      ~max_repository_bytes:(Limits.maximum_request_bytes limits)
      ~poll:(fun () -> Attempt.poll attempt ~checkpoint)
      pin
;;

let with_acquisition request ~checkpoint ~f =
  let store = Request.store request in
  Owner.with_operation store ~generation:(Request.generation request) ~f:(fun () ->
    let attempt = Attempt.create (Request.limits request) ~clock:(Owner.clock store) in
    let host result = Result.map_error result ~f:(fun error -> `Host error) in
    let open Result.Let_syntax in
    let%bind units =
      host (Attempt.acquisition_work_reservation (Request.limits request))
    in
    let%bind () = host (Attempt.reserve_work attempt units) in
    Fun.protect
      ~finally:(fun () -> cleanup_workspace store)
      (fun () ->
         timed store (Request.limits request) ~f:(fun () ->
           let%bind () = host (Attempt.poll attempt ~checkpoint) in
           let%bind key =
             host
               (cache_key
                  attempt
                  ~limits:(Request.limits request)
                  ~repository:(Grant.repository (Request.grant request))
                  ~pin:(Request.pin request)
                  ~checkpoint)
           in
           let cached_entry = Owner.contains store ~key in
           let%bind () =
             host
               (if cached_entry
                then Owner.admit_workspace store
                else Owner.admit_acquisition store (Request.limits request))
           in
           let%bind () = host (setup_workspace store ~checkpoint) in
           if cached_entry
           then
             cached
               store
               ~pin:(Request.pin request)
               ~limits:(Request.limits request)
               ~key
               ~attempt
               ~reservation:(Request.payload_bytes_reserved request)
               ~checkpoint
               ~f
           else fetched request ~key ~attempt ~checkpoint ~f)))
;;

let with_cached_repository store ~repository ~pin ~limits ~checkpoint ~f =
  Owner.with_operation store ~generation:(Owner.generation store) ~f:(fun () ->
    let host result = Result.map_error result ~f:(fun error -> `Host error) in
    let open Result.Let_syntax in
    let attempt = Attempt.create limits ~clock:(Owner.clock store) in
    Fun.protect
      ~finally:(fun () -> cleanup_workspace store)
      (fun () ->
         timed store limits ~f:(fun () ->
           let%bind key = host (cache_key attempt ~limits ~repository ~pin ~checkpoint) in
           match Owner.cache store ~key with
           | None -> Error (`Host (Error.create Cache_corrupt))
           | Some item ->
             let bytes = Owner.Cache.payload_bytes item in
             if bytes > (Int.max_value - 1024) / 4
             then Error (`Host (Error.create Limit_exceeded))
             else (
               let%bind () = host (Attempt.reserve_work attempt ((4 * bytes) + 1024)) in
               let%bind () = host (Attempt.poll attempt ~checkpoint) in
               let%bind () = host (Owner.admit_workspace store) in
               let%bind () = host (setup_workspace store ~checkpoint) in
               cached store ~pin ~limits ~key ~attempt ~reservation:0 ~checkpoint ~f))))
;;
