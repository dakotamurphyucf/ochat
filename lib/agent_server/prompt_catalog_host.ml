open Core
module Catalog = Agent_session.Prompt_catalog
module Baseline = Agent_session.Prompt_project_baseline
module History = Agent_session.Prompt_project_baseline_store
module Publication = Prompt_catalog_publication
module Source_host = Project_source_host

module Store_origin = struct
  type t =
    | Created
    | Existing
end

module Error = struct
  type t =
    | Store of Agent_store.Store_error.t
    | History of History.Error.t
    | Sources of Source_host.Error.t
    | Publication of Publication.Error.t
    | Preparation_timeout

  let history = function
    | History.Error.Busy -> "dependency history is busy"
    | Closed -> "dependency history is closed"
    | Uncertain | Publication_uncertain _ -> "dependency history publication is uncertain"
    | Stale_snapshot -> "dependency history changed during preparation"
    | Foreign_snapshot -> "dependency history snapshot belongs to another owner"
    | Untracked _ ->
      "dependency history is missing or invalid; automatic replacement is prohibited"
    | Baseline _ -> "dependency history failed its association or capacity checks"
    | Store error -> Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error)
  ;;

  let publication = function
    | Publication.Error.Busy -> "prompt catalog publication is busy"
    | Closed -> "prompt catalog host is closed"
    | Foreign_domain -> "prompt catalog belongs to another Eio domain"
    | Invalid_token -> "prompt publication token is not valid for this operation"
    | Publication_uncertain ->
      "prompt publication is uncertain; restart or reconcile the host"
    | Invalid_update -> "invalid dependency update selection"
    | Stale_generation -> "dependency selection generation changed"
    | Unknown_definition id ->
      "unknown prompt definition: " ^ Agent_protocol.Id.Prompt_definition.to_string id
    | History error -> history error
    | Association _ ->
      "prepared scripts do not match their retained dependency associations or limits"
    | Catalog error -> Core.Error.to_string_hum error
    | Seal error -> Sexp.to_string_hum (Catalog.Seal.Error.sexp_of_t error)
    | Artifacts error -> Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error)
  ;;

  let message = function
    | Store error -> Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error)
    | History error -> history error
    | Sources error -> Sexp.to_string_hum (Source_host.Error.sexp_of_t error)
    | Publication error -> publication error
    | Preparation_timeout ->
      "catalog preparation exceeded the original source attempt deadline"
  ;;
end

type t =
  { sw : Eio.Switch.t
  ; env : Eio_unix.Stdenv.base
  ; sources : Source_host.t
  ; catalog : Catalog.t
  ; publication : Publication.t
  }

module Prepared = struct
  type t =
    { publication : Publication.Prepared.t
    ; checkpoint : unit -> unit
    }

  let entries t = Publication.Prepared.entries t.publication
end

let history_limits () =
  let document =
    Document_schema.Limits.create
      ~max_bytes:(8 * 1024 * 1024)
      ~max_depth:128
      ~max_fields:262144
      ~max_nodes:1000000
    |> Result.map_error ~f:(fun error ->
      Core.Error.create_s (Document_schema.Error.sexp_of_t error))
    |> Or_error.ok_exn
  in
  Baseline.Limits.create
    ~max_definitions:1024
    ~max_references:65536
    ~max_identity_bytes:4096
    ~max_work_steps:10_000_000
    ~document
  |> Or_error.ok_exn
;;

let original_source_limits () =
  Agent_session.Prompt_source_owner.Limits.create
    ~max_path_components:512
    ~max_runtime_leases:4096
  |> Or_error.ok_exn
;;

let create ~sw ~env ~store ~origin ~sources ~content_limits ~checkpoint =
  let open Result.Let_syntax in
  let root = Agent_store.Session_store.data_root store in
  let%bind artifacts =
    Agent_store.Prompt_artifact_store.create
      ~env
      ~root:(Agent_store.Data_root.prompt_artifacts_path root)
    |> Result.map_error ~f:(fun error -> Error.Store error)
  in
  let path = Agent_store.Data_root.indexes_path root in
  let%bind directory =
    try Ok (Eio.Path.open_dir ~sw Eio.Path.(Eio.Stdenv.fs env / path)) with
    | Eio.Io _ as exn ->
      Error
        (Error.Store
           (Agent_store.Store_error.of_exn
              ~operation:"open dependency history directory"
              ~path
              exn))
  in
  let baseline_limits = history_limits () in
  let%bind baselines =
    (match origin with
     | Store_origin.Created ->
       History.create_new directory ~sw ~limits:baseline_limits ~checkpoint
     | Existing -> History.open_existing directory ~sw ~limits:baseline_limits ~checkpoint)
    |> Result.map_error ~f:(fun error -> Error.History error)
  in
  let catalog = Catalog.create ~env ~artifact_store:artifacts ~content_limits in
  let%bind publication =
    Source_host.with_host sources ~f:(fun source_preparation _ ->
      let policy =
        Publication.Policy.create
          ~source_preparation
          ~custody_limits:
            (Project_source_policy.custody_limits (Source_host.policy sources))
          ~baseline_limits
          ~source_limits:(original_source_limits ())
          ~max_roots:1024
        |> Or_error.ok_exn
      in
      Publication.create ~store ~catalog ~artifacts ~baselines ~policy ~content_limits)
    |> Result.map_error ~f:(fun error -> Error.Sources error)
  in
  Ok { sw; env; sources; catalog; publication }
;;

let catalog t = t.catalog

exception Deadline_exceeded

let attempt_checkpoint t checkpoint =
  let clock = Eio.Stdenv.mono_clock t.env in
  let started = Eio.Time.Mono.now clock in
  let seconds =
    Project_source_policy.attempt_wall_time (Source_host.policy t.sources)
    |> Time_ns.Span.to_sec
  in
  fun () ->
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ();
    let elapsed =
      Mtime.span started (Eio.Time.Mono.now clock) |> Mtime.Span.to_float_ns
    in
    if Float.(elapsed /. 1e9 >= seconds) then raise Deadline_exceeded
;;

let prepare t ~mode ~created_at ~checkpoint definitions =
  let checkpoint = attempt_checkpoint t checkpoint in
  let prepared = ref None in
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      Option.iter !prepared ~f:(fun value ->
        ignore (Publication.discard value : Publication.certainty)))
  in
  match
    let open Result.Let_syntax in
    let%bind result =
      Source_host.with_host t.sources ~f:(fun _ sources ->
        let result =
          Publication.prepare
            t.publication
            ~sources
            ~sw:t.sw
            ~env:t.env
            ~mode
            ~created_at
            ~checkpoint
            definitions
        in
        (match result with
         | Ok value -> prepared := Some value
         | Error _ -> ());
        result)
      |> Result.map_error ~f:(fun error -> Error.Sources error)
    in
    let%bind publication =
      Result.map_error result ~f:(fun error -> Error.Publication error)
    in
    checkpoint ();
    Ok { Prepared.publication; checkpoint }
  with
  | Ok _ as result -> result
  | Error _ as result ->
    cleanup ();
    result
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    (try cleanup () with
     | _ -> ());
    (match exn with
     | Deadline_exceeded -> Error Error.Preparation_timeout
     | _ -> Exn.raise_with_original_backtrace exn trace)
;;

let publish t =
  try
    match Publication.publish t.Prepared.publication ~checkpoint:t.checkpoint with
    | Ok () -> Ok ()
    | Error error -> Error (Error.Publication error)
  with
  | Deadline_exceeded -> Error Error.Preparation_timeout
;;

let commit t = Publication.commit t.Prepared.publication
let discard t = Publication.discard t.Prepared.publication
let retire t = Publication.retire t.Prepared.publication

let prune t ~additional ~checkpoint =
  Publication.prune t.publication ~additional ~checkpoint
  |> Result.map_error ~f:(fun error -> Error.Publication error)
;;
