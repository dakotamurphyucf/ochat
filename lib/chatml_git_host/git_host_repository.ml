open Core
module Profile = Git_host_profile
module Storage = Git_host_storage
module Attempt = Git_host_attempt
module Owner = Git_host_store
module Process = Shell_access.Bounded_process
module Pin = Chatml_project.Git_pin
module Error = Profile.Error
module Object_id = Profile.Object_id
module Limits = Attempt.Limits

let stored result = Result.map_error result ~f:Error.of_storage_error
let protected f = Eio.Cancel.protect f

let process_error = function
  | Process.Error.Timed_out -> Error.create Timed_out
  | Output_limit -> Error.create Limit_exceeded
  | Invalid_request -> Error.create Invalid_input
  | Unsupported_platform -> Error.create Unsupported_provider
  | Spawn_failed | Process_failed | Io_failed -> Error.create Process_failed
;;

type t =
  { store : Owner.t
  ; directory : Storage.t
  ; workspace : string
  ; pin : Pin.t
  ; attempt : Attempt.t
  ; limits : Limits.t
  ; query_switch : Eio.Switch.t
  ; mutable live : bool
  ; mutable querying : bool
  }

let run repo command ~file_size_bytes ~max_output_bytes ~checkpoint =
  let open Result.Let_syntax in
  let attempt = repo.attempt in
  let run () =
    let%bind () = Attempt.poll attempt ~checkpoint in
    let%bind () = stored (Storage.validate_root (Owner.root repo.store)) in
    let%bind () = Attempt.reserve_work attempt 1 in
    let arguments =
      Profile.arguments (Owner.deployment repo.store) ~workspace:repo.workspace command
    in
    let environment =
      Profile.environment (Owner.deployment repo.store) ~workspace:repo.workspace command
    in
    let stdin = Profile.stdin command in
    let executable = Profile.Deployment.executable (Owner.deployment repo.store) in
    let%bind limits =
      Attempt.process_limits
        attempt
        ~file_size_bytes
        ~maximum_output_bytes:max_output_bytes
    in
    let%bind () =
      Attempt.charge_request attempt ~executable ~arguments ~environment ~stdin
    in
    let%bind invocation =
      Result.map_error
        (Process.create ~limits ~executable ~arguments ~environment ~stdin)
        ~f:process_error
    in
    let%bind outcome =
      Storage.with_cwd repo.directory ~f:(fun cwd ->
        Result.map_error
          (Process.run invocation ~cwd ~clock:(Owner.clock repo.store) ~checkpoint)
          ~f:process_error)
    in
    let%bind () =
      Attempt.charge_output
        attempt
        ~stdout:(Process.Outcome.stdout outcome)
        ~stderr_bytes:(Process.Outcome.stderr_bytes outcome)
    in
    let%bind () = Attempt.poll attempt ~checkpoint in
    match Process.Outcome.status outcome with
    | `Exited 0 -> Ok (Process.Outcome.stdout outcome)
    | `Exited _ | `Signaled _ -> Error (Error.create Process_failed)
  in
  try
    match run () with
    | Ok _ as success -> success
    | Error _ as failure ->
      Attempt.poison attempt;
      failure
  with
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Attempt.poison attempt;
    Exn.raise_with_original_backtrace exn backtrace
;;

let pin t = t.pin

let checkpoint t =
  if not t.live
  then Error (Error.create Closed)
  else Attempt.poll t.attempt ~checkpoint:(fun () -> ())
;;

let raw_object t id ~max_output_bytes ~checkpoint =
  if not t.live
  then Error (Error.create Closed)
  else if t.querying
  then Error (Error.create Busy)
  else if
    (not (Pin.equal_object_format (Object_id.object_format id) (Pin.object_format t.pin)))
    || max_output_bytes < 0
  then (
    Attempt.poison t.attempt;
    Error (Error.create Invalid_input))
  else (
    let completed, resolve_completed = Eio.Promise.create () in
    let context = ref None in
    t.querying <- true;
    Eio.Fiber.fork_daemon ~sw:t.query_switch (fun () ->
      let outcome =
        try
          let value =
            Fun.protect
              ~finally:(fun () ->
                context := None;
                t.querying <- false)
              (fun () ->
                 Eio.Cancel.sub (fun cancellation ->
                   context := Some cancellation;
                   run
                     t
                     (Profile.Raw_object id)
                     ~file_size_bytes:0
                     ~max_output_bytes
                     ~checkpoint))
          in
          Ok value
        with
        | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      Eio.Promise.resolve resolve_completed outcome;
      `Stop_daemon);
    try
      match Eio.Promise.await completed with
      | Ok result -> result
      | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      Attempt.poison t.attempt;
      protected (fun () ->
        Option.iter !context ~f:(fun cancellation -> Eio.Cancel.cancel cancellation exn);
        ignore (Eio.Promise.await completed));
      Exn.raise_with_original_backtrace exn backtrace)
;;

let validate repo ~checkpoint =
  let open Result.Let_syntax in
  let max_bytes = Owner.maximum_cache_payload_bytes repo.store in
  let%bind inventory =
    stored
      (Storage.inventory
         repo.directory
         ~max_entries:16
         ~max_work_steps:1024
         ~max_depth:3
         ~max_payload_bytes:max_bytes
         ~checkpoint)
  in
  let valid =
    List.for_all (Storage.Inventory.directories inventory) ~f:Profile.permitted_directory
    && List.for_all (Storage.Inventory.files inventory) ~f:(fun file ->
      Profile.permitted_file
        ~object_format:(Pin.object_format repo.pin)
        file.Storage.Inventory.path)
  in
  if not valid
  then Error (Error.create Cache_corrupt)
  else (
    let%bind config =
      stored
        (Storage.read_file
           repo.directory
           ~name:"config"
           ~max_bytes:(Limits.metadata_file_bytes repo.limits)
           ~checkpoint)
    in
    let%bind head =
      stored
        (Storage.read_file
           repo.directory
           ~name:"HEAD"
           ~max_bytes:(Limits.metadata_file_bytes repo.limits)
           ~checkpoint)
    in
    if Profile.validate_metadata ~object_format:(Pin.object_format repo.pin) ~config ~head
    then Ok inventory
    else Error (Error.create Cache_corrupt))
;;

let with_directory store ~directory ~pin ~attempt ~limits ~f =
  Eio.Switch.run (fun query_switch ->
    let repository =
      { store
      ; directory
      ; workspace = Owner.native_workspace store
      ; pin
      ; attempt
      ; limits
      ; query_switch
      ; live = true
      ; querying = false
      }
    in
    Fun.protect
      ~finally:(fun () ->
        repository.live <- false;
        if repository.querying then Attempt.poison attempt)
      (fun () -> f repository))
;;

let verify repo ~f =
  Fun.protect
    ~finally:(fun () -> repo.live <- false)
    (fun () ->
       match f repo with
       | Error error -> Error (`Callback error)
       | Ok value ->
         if repo.querying then Attempt.poison repo.attempt;
         if Attempt.is_poisoned repo.attempt
         then Error (`Host (Error.create Process_failed))
         else Ok value)
;;
