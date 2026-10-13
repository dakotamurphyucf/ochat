open Core
module Sources = Agent_session.Prompt_script_preparation
module Git = Chatml_git_host
module Configuration = Project_source_configuration
module Policy = Project_source_policy
module Activity = Agent_session.Runtime_activity

module Error = struct
  type t =
    | Invalid_configuration of Core.Error.t
    | Filesystem
    | Authorization_header_too_large
    | External_root of Chatml_local_roots.Error.t
    | Git of Git.Error.t
    | Closed
    | Foreign_domain

  let sexp_of_t = function
    | Invalid_configuration error ->
      [%sexp "Invalid_configuration", (error : Core.Error.t)]
    | Filesystem -> Sexp.Atom "Filesystem"
    | Authorization_header_too_large -> Sexp.Atom "Authorization_header_too_large"
    | External_root error ->
      [%sexp
        "External_root"
      , (Chatml_local_roots.Error.code error : Chatml_local_roots.Error.code)]
    | Git error -> [%sexp "Git", (error : Git.Error.t)]
    | Closed -> Sexp.Atom "Closed"
    | Foreign_domain -> Sexp.Atom "Foreign_domain"
  ;;
end

type t =
  { domain : int
  ; policy : Policy.t
  ; source_preparation : Sources.Policy.t
  ; authoring_roots : (string * Chatml_local_roots.Anchor.t) list
  ; project_policy : Chatml_project_prepare.Policy.t
  ; project_limits : Chatml_project_prepare.Limits.t
  ; max_project_request_bytes : int
  ; host : Sources.Host.t
  ; activity : Activity.t
  ; cancel : Eio.Cancel.t
  ; cancel_requested : bool ref
  ; finished : unit Eio.Promise.t
  ; mutable closed : bool
  }

let domain () = (Stdlib.Domain.self () :> int)

let check_domain t =
  if not (Int.equal t.domain (domain ()))
  then invalid_arg "Project_source_host: foreign domain"
;;

let policy t =
  check_domain t;
  t.policy
;;

let close t =
  check_domain t;
  if not t.closed
  then (
    t.closed <- true;
    t.cancel_requested := true;
    Eio.Cancel.cancel t.cancel Exit)
;;

let close_and_wait t =
  check_domain t;
  Eio.Cancel.protect (fun () ->
    close t;
    Eio.Promise.await t.finished)
;;

let invalid () =
  Error.Invalid_configuration (Core.Error.of_string "Invalid project source deployment")
;;

let open_anchors configuration ~sw ~fs ~checkpoint =
  let open Result.Let_syntax in
  let limits =
    Chatml_local_roots.Limits.create
      ~max_locator_bytes:4_096
      ~max_locator_components:64
      ~max_anchors:33
      ~max_retained_roots:128
      ~max_work_steps:100_000
    |> Or_error.ok_exn
  in
  List.fold_result
    configuration.Configuration.external_roots
    ~init:[]
    ~f:(fun anchors prefix ->
      checkpoint ();
      let directory = Eio.Path.open_dir ~sw Eio.Path.(fs / prefix) in
      let%map anchor =
        Chatml_local_roots.Anchor.create
          directory
          ~host_prefix:prefix
          ~max_path_components:64
          ~limits
          ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.External_root error)
      in
      anchor :: anchors)
  |> Result.map ~f:List.rev
;;

let open_git configuration ~sw ~fs ~data_root ~clock ~configured ~checkpoint =
  match configuration.Configuration.git with
  | None -> Ok (None, [])
  | Some git ->
    let open Result.Let_syntax in
    let%bind probes =
      match Policy.Configured.git_probe_limits configured with
      | Some probes -> Ok probes
      | None -> Error (invalid ())
    in
    checkpoint ();
    let directory = Eio.Path.open_dir ~sw Eio.Path.(fs / git.helper_directory) in
    let%bind deployment =
      Git.Deployment.admit_git_2_56_0
        directory
        ~native_helper_directory:git.helper_directory
        ~native_git_executable:(Filename.concat git.helper_directory "git")
        ~native_ca_bundle:git.ca_bundle
        ~clock
        ~probe_limits:probes
        ~max_deployment_bytes:Configuration.maximum_authority_bytes
        ~checkpoint
      |> Result.map_error ~f:(fun error -> Error.Git error)
    in
    let%bind grants, _ =
      List.fold_result
        git.repositories
        ~init:([], 0)
        ~f:(fun (grants, bytes) repository ->
          checkpoint ();
          let remaining = Configuration.maximum_authority_bytes - bytes in
          let%bind grant =
            match repository with
            | Configuration.Repository.Local repository ->
              Git.Grant.local ~repository ~max_input_bytes:remaining
              |> Result.map_error ~f:(fun _ -> invalid ())
            | Https { repository; authorization_header_file } ->
              let%bind authorization =
                match authorization_header_file with
                | None -> Ok None
                | Some path ->
                  let remaining = remaining - String.length repository in
                  if remaining <= 0
                  then Error (invalid ())
                  else
                    Project_source_header.read
                      fs
                      ~path
                      ~max_bytes:
                        (Int.min
                           Configuration.maximum_authorization_header_bytes
                           remaining)
                      ~checkpoint
                    |> Result.map ~f:Option.some
                    |> Result.map_error ~f:(function
                      | Project_source_header.Filesystem -> Error.Filesystem
                      | Too_large -> Authorization_header_too_large)
              in
              Git.Grant.https ~repository ~authorization ~max_input_bytes:remaining
              |> Result.map_error ~f:(fun _ -> invalid ())
          in
          let input = Git.Grant.input_bytes grant in
          if input > remaining
          then Error (invalid ())
          else Ok (grant :: grants, bytes + input))
    in
    let root = Agent_store.Data_root.path data_root in
    if String.length root > 3_840
    then Error (invalid ())
    else (
      let cache = Eio.Path.(fs / root / "project-git") in
      let%bind () =
        match Eio.Path.kind ~follow:false cache with
        | `Not_found ->
          Eio.Path.mkdir ~perm:0o700 cache;
          Ok ()
        | `Directory -> Ok ()
        | _ -> Error Error.Filesystem
      in
      checkpoint ();
      let cache = Eio.Path.open_dir ~sw cache in
      let%map store =
        Git.Store.open_root
          cache
          ~sw
          ~deployment
          ~clock
          ~limits:(Policy.Configured.git_store_limits configured)
          ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Git error)
      in
      Some store, List.rev grants)
;;

let open_owned
      configuration
      ~sw
      ~fs
      ~data_root
      ~clock
      ?(policy = Policy.default)
      ~checkpoint
      ()
  =
  let open Result.Let_syntax in
  let%bind configured =
    Policy.configure policy configuration
    |> Result.map_error ~f:(fun error -> Error.Invalid_configuration error)
  in
  Eio.Fiber.check ();
  Eio.Switch.check sw;
  let module Callback = struct
    exception Raised of exn * Stdlib.Printexc.raw_backtrace
  end
  in
  let poll () =
    Eio.Fiber.check ();
    (try checkpoint () with
     | exn -> raise (Callback.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())));
    Eio.Fiber.check ()
  in
  let ready, resolve_ready = Eio.Promise.create () in
  let finished, resolve_finished = Eio.Promise.create () in
  let ready_settled = ref false in
  let finished_settled = ref false in
  let resolve_ready outcome =
    if not !ready_settled
    then (
      ready_settled := true;
      Eio.Promise.resolve resolve_ready outcome)
  in
  let resolve_finished () =
    if not !finished_settled
    then (
      finished_settled := true;
      Eio.Promise.resolve resolve_finished ())
  in
  let context = ref None in
  let cancel_requested = ref false in
  let owner = ref None in
  Eio.Switch.on_release sw (fun () ->
    Option.iter !owner ~f:(fun t -> t.closed <- true);
    resolve_ready (Ok (Error Error.Closed));
    if Option.is_none !context then resolve_finished ());
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let failure =
      try
        Eio.Cancel.sub (fun cancel ->
          context := Some cancel;
          Eio.Switch.run (fun owned ->
            Fun.protect
              ~finally:(fun () -> Option.iter !owner ~f:(fun t -> t.closed <- true))
              (fun () ->
                 if !cancel_requested then Eio.Cancel.cancel cancel Exit;
                 let startup =
                   try
                     poll ();
                     let%bind additional_anchors =
                       open_anchors configuration ~sw:owned ~fs ~checkpoint:poll
                     in
                     let%bind git_store, git_grants =
                       open_git
                         configuration
                         ~sw:owned
                         ~fs
                         ~data_root
                         ~clock
                         ~configured
                         ~checkpoint:poll
                     in
                     let activity = Activity.create ~sw:owned in
                     poll ();
                     Ok
                       { domain = domain ()
                       ; policy
                       ; source_preparation =
                           Policy.Configured.source_preparation configured
                       ; authoring_roots =
                           List.zip_exn configuration.external_roots additional_anchors
                       ; project_policy = Policy.Configured.project_policy configured
                       ; project_limits = Policy.Configured.project_limits configured
                       ; max_project_request_bytes =
                           Policy.Configured.max_project_request_bytes configured
                       ; host = { Sources.Host.additional_anchors; git_store; git_grants }
                       ; activity
                       ; cancel
                       ; cancel_requested
                       ; finished
                       ; closed = false
                       }
                   with
                   | Eio.Io _ -> Error Error.Filesystem
                 in
                 match startup with
                 | Error error -> resolve_ready (Ok (Error error))
                 | Ok t ->
                   owner := Some t;
                   resolve_ready (Ok (Ok t));
                   Eio.Fiber.await_cancel ())));
        None
      with
      | exn ->
        let trace = Stdlib.Printexc.get_raw_backtrace () in
        resolve_ready (Error (exn, trace));
        (match exn with
         | Eio.Cancel.Cancelled _ when !cancel_requested || Eio.Fiber.is_cancelled () ->
           None
         | _ when Option.is_some !owner -> Some (exn, trace)
         | _ -> None)
    in
    context := None;
    resolve_finished ();
    match failure with
    | None -> `Stop_daemon
    | Some (exn, trace) -> Exn.raise_with_original_backtrace exn trace);
  let deliver () =
    match Eio.Promise.await ready with
    | Ok (Ok t) ->
      Eio.Fiber.check ();
      Ok t
    | Ok (Error error) ->
      Eio.Cancel.protect (fun () -> Eio.Promise.await finished);
      Eio.Fiber.check ();
      Error error
    | Error (exn, trace) -> Exn.raise_with_original_backtrace exn trace
  in
  try deliver () with
  | exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      cancel_requested := true;
      Option.iter !context ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
      Eio.Promise.await finished);
    (match exn with
     | Callback.Raised (original, trace) ->
       Exn.raise_with_original_backtrace original trace
     | _ -> Exn.raise_with_original_backtrace exn trace)
;;

let with_host t ~f =
  if not (Int.equal t.domain (domain ()))
  then Error Error.Foreign_domain
  else if t.closed
  then Error Error.Closed
  else
    let module Callback = struct
      exception Raised of exn * Stdlib.Printexc.raw_backtrace
    end
    in
    try
      let result =
        Activity.run t.activity (fun () ->
          if t.closed then raise Activity.Closed;
          try f t.source_preparation t.host with
          | exn -> raise (Callback.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())))
      in
      Eio.Fiber.check ();
      Ok result
    with
    | Callback.Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace
    | Activity.Closed -> Error Error.Closed
;;

let with_authoring t ~f =
  if not (Int.equal t.domain (domain ()))
  then Error Error.Foreign_domain
  else if t.closed
  then Error Error.Closed
  else
    let module Callback = struct
      exception Raised of exn * Stdlib.Printexc.raw_backtrace
    end
    in
    try
      let result =
        Activity.run t.activity (fun () ->
          if t.closed then raise Activity.Closed;
          try
            f
              ~roots:t.authoring_roots
              ~policy:t.project_policy
              ~limits:t.project_limits
              ~max_request_bytes:t.max_project_request_bytes
          with
          | exn -> raise (Callback.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())))
      in
      Eio.Fiber.check ();
      Ok result
    with
    | Callback.Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace
    | Activity.Closed -> Error Error.Closed
;;
