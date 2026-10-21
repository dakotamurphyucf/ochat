open! Core
module Store = Agent_store.Developer_run_store
module Definition = Agent_session.Workspace_definition
module Lease = Agent_session.Workspace_lease
module Resolver = Agent_session.Workspace_resolver
module Cleanup = Agent_session.Workspace_cleanup

module Error = struct
  type t =
    | Store of Store.Error.t
    | Workspace of Agent_store.Store_error.t
    | Lease of Agent_protocol.Error.t
    | Unsupported_cleanup of Definition.cleanup
    | Foreign_or_stale
  [@@deriving sexp_of]
end

exception Cleanup_failed of Error.t

type t =
  { env : Eio_unix.Stdenv.base
  ; directory : Store.Live_directory.t
  ; owned : Resolver.Owned.t
  ; instance : Agent_session.Workspace_instance.t
  ; leases : Lease.t
  ; lease : Lease.lease
  ; checkpoint : unit -> unit
  ; mutable workspace_closed : bool
  ; mutable closed : bool
  }

let ( let* ) result f = Result.bind result ~f

let mode definition =
  match definition.Definition.access with
  | Read_only | Shared_write -> Lease.Shared
  | Exclusive -> Lease.Exclusive
;;

let allowed definition =
  match definition.Definition.source with
  | Physical _ -> Ok ()
  | Temporary { cleanup = On_session_stop; _ } -> Ok ()
  | Temporary { cleanup = (On_session_delete | Retain) as cleanup; _ } ->
    Error (Error.Unsupported_cleanup cleanup)
;;

let instance t = t.instance
let conflict_domain t = t.instance.conflict_domain
let runtime_directory t = Store.Live_directory.runtime_directory t.directory
let cache_directory t = Store.Live_directory.cache_directory t.directory
let response_directory t = Store.Live_directory.response_directory t.directory

let check_live t =
  if t.closed || t.workspace_closed || not (Lease.is_active t.leases t.lease)
  then Error Error.Foreign_or_stale
  else
    let* () =
      Store.Live_directory.check_live t.directory
      |> Result.map_error ~f:(fun e -> Error.Store e)
    in
    Resolver.verify_available ~env:t.env t.instance
    |> Result.map_error ~f:(fun e -> Error.Workspace e)
;;

let close_and_wait t =
  if t.closed
  then Ok ()
  else
    let* () =
      if t.workspace_closed
      then Ok ()
      else
        let* () =
          Resolver.Owned.close t.owned |> Result.map_error ~f:(fun e -> Error.Workspace e)
        in
        t.workspace_closed <- true;
        Ok ()
    in
    let* () =
      Store.Live_directory.close_and_wait t.directory ~checkpoint:t.checkpoint
      |> Result.map_error ~f:(fun e -> Error.Store e)
    in
    t.closed <- true;
    Ok ()
;;

let open_live
      ~env
      ~sw
      ~handle
      ~definition
      ~workspace_leases
      ~physical_roots
      ~cleanup_limits
      ~checkpoint
  =
  let* () = allowed definition in
  let owner = Store.owner handle in
  let initial_lease =
    match definition.conflict_domain with
    | None -> Ok None
    | Some conflict_domain ->
      Lease.acquire_developer
        workspace_leases
        ~conflict_domain
        ~owner
        ~mode:(mode definition)
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun error -> Error.Lease error)
  in
  let* initial_lease = initial_lease in
  let lease_ref = ref initial_lease in
  let* directory =
    match Store.Live_directory.open_ handle ~sw ~checkpoint with
    | Ok directory -> Ok directory
    | Error error ->
      Option.iter initial_lease ~f:(Lease.release workspace_leases);
      Error (Error.Store error)
    | exception exn ->
      let trace = Backtrace.Exn.most_recent () in
      (* Resolver has not acquired a workspace yet. Store.open_ owns any
         provisional Live child through its own switch callback. *)
      Eio.Cancel.protect (fun () ->
        Option.iter initial_lease ~f:(Lease.release workspace_leases));
      Exn.raise_with_original_backtrace exn trace
  in
  let cleanup_has_active_lease ~conflict_domain =
    match !lease_ref with
    | None -> Lease.active workspace_leases ~conflict_domain > 0
    | Some lease -> Lease.active_other_than workspace_leases lease ~conflict_domain
  in
  let cleanup_on_complete () =
    Option.iter !lease_ref ~f:(Lease.release workspace_leases);
    lease_ref := None
  in
  let resolved =
    Resolver.resolve_owned
      ~cleanup_limits
      ~cleanup_checkpoint:checkpoint
      ~cleanup_has_active_lease
      ~cleanup_on_complete
      ~sw
      ~env
      ~physical_roots
      ~instance_id:(Agent_protocol.Id.Workspace_instance.create ())
      ~session_directory:(Store.Live_directory.workspace_parent_directory directory)
      definition
  in
  let* owned =
    match resolved with
    | Ok owned -> Ok owned
    | Error error ->
      ignore
        (Store.Live_directory.close_and_wait directory ~checkpoint
         : (unit, Store.Error.t) result);
      Error (Error.Workspace error)
  in
  let instance = Resolver.Owned.instance owned in
  let selected_lease =
    match initial_lease with
    | Some lease -> Ok lease
    | None ->
      Lease.acquire_developer
        workspace_leases
        ~conflict_domain:instance.conflict_domain
        ~owner
        ~mode:(mode definition)
      |> Result.map_error ~f:(fun error -> Error.Lease error)
  in
  let* lease =
    match selected_lease with
    | Ok lease -> Ok lease
    | Error error ->
      ignore (Resolver.Owned.close owned : (unit, Agent_store.Store_error.t) result);
      ignore
        (Store.Live_directory.close_and_wait directory ~checkpoint
         : (unit, Store.Error.t) result);
      Error error
  in
  lease_ref := Some lease;
  let t =
    { env
    ; directory
    ; owned
    ; instance
    ; leases = workspace_leases
    ; lease
    ; checkpoint
    ; workspace_closed = false
    ; closed = false
    }
  in
  Eio.Switch.on_release sw (fun () ->
    match close_and_wait t with
    | Ok () -> ()
    | Error error -> raise (Cleanup_failed error));
  Ok t
;;
