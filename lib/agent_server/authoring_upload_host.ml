open! Core
module Store = Agent_store.Authoring_upload_store
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Actor = Operator_authorization
module Validation = Authoring_upload_validation

module Upload_policy = struct
  type t =
    { authorize_upload : Actor.t -> bool
    ; authorize_parent :
        Actor.t -> Agent_protocol.Authoring_context.Context.Expected.t -> bool
    }

  let create ~authorize_upload ~authorize_parent = { authorize_upload; authorize_parent }
end

module Publication_policy = struct
  module Selected = Validation.Selected
  include Validation.Policy
end

module Publication = struct
  type t =
    { policy : Publication_policy.t
    ; limits : Chatml_authoring_bundle_project.Limits.t
    ; document_dir : Eio.Fs.dir_ty Eio.Path.t
    }

  let create ~policy ~limits ~document_dir = { policy; limits; document_dir }
end

module Error = struct
  module Store_code = struct
    type t =
      | Invalid_manifest
      | Limit_exceeded
      | Idempotency_conflict
      | Invalid_offset
      | Incomplete
      | Sealed
      | Expired
      | Missing_or_foreign
      | Busy
      | Storage_failure
    [@@deriving sexp_of]
  end

  type t =
    | Unauthorized
    | Expired_actor
    | Foreign_domain
    | Closed
    | Busy
    | Selection_unavailable
    | Invalid_closure
    | Resource_limit
    | Store of Store_code.t
  [@@deriving sexp_of]

  let of_store = function
    | Store.Error.Invalid_manifest -> Store Store_code.Invalid_manifest
    | Limit_exceeded -> Store Store_code.Limit_exceeded
    | Idempotency_conflict -> Store Store_code.Idempotency_conflict
    | Invalid_offset -> Store Store_code.Invalid_offset
    | Incomplete -> Store Store_code.Incomplete
    | Sealed -> Store Store_code.Sealed
    | Expired -> Store Store_code.Expired
    | Missing_or_foreign -> Store Store_code.Missing_or_foreign
    | Busy -> Store Store_code.Busy
    | Closed -> Closed
    | Storage _ -> Store Store_code.Storage_failure
  ;;
end

type t =
  { domain : int
  ; store : Store.t
  ; now : unit -> Agent_protocol.Timestamp.t
  ; upload_policy : Upload_policy.t
  ; publication : Publication.t option
  ; gate : Eio.Mutex.t
  ; mutable active_cancel : Eio.Cancel.t option
  ; mutable closed : bool
  ; mutable release_hook : Eio.Switch.hook
  }

exception Revoked of Error.t

let domain () = (Stdlib.Domain.self () :> int)
let active_hosts : t list Eio.Fiber.key = Eio.Fiber.create_key ()

let is_active t =
  Option.value (Eio.Fiber.get active_hosts) ~default:[] |> List.exists ~f:(phys_equal t)
;;

let with_active t f =
  let stack = Option.value (Eio.Fiber.get active_hosts) ~default:[] in
  Eio.Fiber.with_binding active_hosts (t :: stack) f
;;

let check_host t =
  if not (Int.equal t.domain (domain ()))
  then Error Error.Foreign_domain
  else if t.closed
  then Error Error.Closed
  else Ok ()
;;

let check t actor parent =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  if not (Actor.is_current actor)
  then Error Error.Expired_actor
  else if not (t.upload_policy.authorize_upload actor)
  then Error Error.Unauthorized
  else (
    match parent with
    | None -> Ok ()
    | Some expected ->
      if t.upload_policy.authorize_parent actor expected
      then Ok ()
      else Error Error.Unauthorized)
;;

let check_or_raise t actor parent =
  match check t actor parent with
  | Ok () -> ()
  | Error error -> raise (Revoked error)
;;

let with_request t ~actor ~parent ~checkpoint f =
  let open Result.Let_syntax in
  let%bind () = if is_active t then Error Error.Busy else Ok () in
  let%bind () = check t actor parent in
  let outcome =
    Eio.Mutex.use_rw ~protect:false t.gate (fun () ->
      match
        Eio.Cancel.sub (fun cancel ->
          t.active_cancel <- Some cancel;
          Exn.protect
            ~finally:(fun () -> t.active_cancel <- None)
            ~f:(fun () ->
              with_active t (fun () ->
                let%bind () = check t actor parent in
                let guarded_checkpoint () =
                  check_or_raise t actor parent;
                  checkpoint ();
                  check_or_raise t actor parent
                in
                let result =
                  f
                    ~principal:(Actor.principal actor).id
                    ~now:t.now
                    ~checkpoint:guarded_checkpoint
                in
                let%bind () = check t actor parent in
                Result.map_error result ~f:Error.of_store)))
      with
      | result -> `Returned result
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | `Returned result -> result
  | `Raised (Revoked error, _) -> Error error
  | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let close_and_wait t =
  if not (Int.equal t.domain (domain ()))
  then Error Error.Foreign_domain
  else if is_active t
  then Error Error.Busy
  else (
    t.closed <- true;
    Option.iter t.active_cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
    Eio.Cancel.protect (fun () ->
      let outcome =
        Eio.Mutex.use_rw ~protect:false t.gate (fun () ->
          match Store.close_and_wait t.store |> Result.map_error ~f:Error.of_store with
          | result -> `Returned result
          | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
      in
      ignore (Eio.Switch.try_remove_hook t.release_hook : bool);
      match outcome with
      | `Returned result -> result
      | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let create ?publication store ~sw ~clock ~upload_policy =
  Eio.Switch.check sw;
  let now () =
    Eio.Time.now clock
    |> Time_ns.Span.of_sec
    |> Time_ns.of_span_since_epoch
    |> Agent_protocol.Timestamp.of_time_ns
  in
  let t =
    { domain = domain ()
    ; store
    ; now
    ; upload_policy
    ; publication
    ; gate = Eio.Mutex.create ()
    ; active_cancel = None
    ; closed = false
    ; release_hook = Eio.Switch.null_hook
    }
  in
  t.release_hook
  <- Eio.Switch.on_release_cancellable sw (fun () ->
       ignore (close_and_wait t : (unit, Error.t) result));
  t
;;

let begin_upload t ~actor ~(request : Bundle.Begin.request) ~checkpoint =
  with_request
    t
    ~actor
    ~parent:request.parent_context
    ~checkpoint
    (fun ~principal ~now ~checkpoint ->
       Store.begin_upload t.store ~principal ~request ~now ~checkpoint)
;;

let chunk t ~actor ~request ~checkpoint =
  with_request t ~actor ~parent:None ~checkpoint (fun ~principal ~now ~checkpoint ->
    Store.chunk t.store ~principal ~request ~now ~checkpoint)
;;

let claim_commit t ~actor ~request ~checkpoint =
  with_request t ~actor ~parent:None ~checkpoint (fun ~principal ~now ~checkpoint ->
    Store.claim_commit t.store ~principal ~request ~now ~checkpoint)
;;

let publish t ~actor ~sealed ~checkpoint =
  match t.publication with
  | None -> Error Error.Selection_unavailable
  | Some publication ->
    with_request t ~actor ~parent:None ~checkpoint (fun ~principal ~now ~checkpoint ->
      let snapshot = Store.snapshot t.store ~principal sealed ~now ~checkpoint in
      match snapshot with
      | Error _ as error -> error
      | Ok snapshot ->
        let manifest = Store.Snapshot.manifest snapshot in
        let selection =
          Validation.admit publication.policy ~actor ~manifest ~checkpoint
        in
        (match selection with
         | Error (Unavailable | Invalid_closure | Resource_limit) ->
           raise (Revoked Error.Selection_unavailable)
         | Ok selection ->
           let selected_checkpoint () =
             checkpoint ();
             if
               not
                 (Validation.check_current
                    selection
                    ~policy:publication.policy
                    ~actor
                    ~checkpoint)
             then raise (Revoked Error.Selection_unavailable)
           in
           let validated =
             Validation.validate
               selection
               ~snapshot
               ~document_dir:publication.document_dir
               ~limits:publication.limits
               ~checkpoint
           in
           selected_checkpoint ();
           (match validated with
            | Error Unavailable -> raise (Revoked Error.Selection_unavailable)
            | Error ((Invalid_closure | Resource_limit) as problem) ->
              let reason =
                match problem with
                | Invalid_closure -> Bundle.Rejection.Invalid_closure
                | Resource_limit -> Bundle.Rejection.Resource_limit
                | Unavailable -> assert false
              in
              (match
                 Store.reject
                   t.store
                   ~principal
                   sealed
                   ~reason
                   ~now
                   ~checkpoint:selected_checkpoint
               with
               | Error _ as error -> error
               | Ok () ->
                 raise
                   (Revoked
                      (match problem with
                       | Invalid_closure -> Error.Invalid_closure
                       | Resource_limit -> Error.Resource_limit
                       | Unavailable -> assert false)))
            | Ok proof ->
              let final_admission () =
                check_or_raise t actor None;
                if
                  not
                    (Validation.check_current
                       selection
                       ~policy:publication.policy
                       ~actor
                       ~checkpoint:Fn.id)
                then raise (Revoked Error.Selection_unavailable)
              in
              Store.publish
                t.store
                ~principal
                sealed
                ~snapshot
                ~proof
                ~now
                ~checkpoint:selected_checkpoint
                ~final_admission)))
;;

let status t ~actor ~(request : Bundle.Status.request) ~checkpoint =
  with_request t ~actor ~parent:None ~checkpoint (fun ~principal ~now ~checkpoint ->
    Store.status t.store ~principal ~upload_id:request.upload_id ~now ~checkpoint)
;;

let cancel t ~actor ~upload_id ~checkpoint =
  with_request t ~actor ~parent:None ~checkpoint (fun ~principal ~now ~checkpoint ->
    Store.cancel t.store ~principal ~upload_id ~now ~checkpoint)
;;
