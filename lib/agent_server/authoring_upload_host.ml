open! Core
module Store = Agent_store.Authoring_upload_store
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Actor = Operator_authorization
module Validation = Authoring_upload_validation
module Rebind_validation = Authoring_upload_rebind

module Upload_policy = struct
  module Operation = struct
    type t =
      | Write
      | Commit
      | Publish
  end

  type t =
    { authorize_write : Actor.t -> bool
    ; authorize_publish : Actor.t -> bool
    ; authorize_parent :
        (Actor.t -> Agent_protocol.Authoring_context.Context.Expected.t -> bool) option
    }

  let create ~authorize_write ~authorize_publish ~authorize_parent =
    { authorize_write; authorize_publish; authorize_parent }
  ;;

  let authorize t actor = function
    | Operation.Write -> t.authorize_write actor
    | Commit -> t.authorize_write actor && t.authorize_publish actor
    | Publish -> t.authorize_publish actor
  ;;
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

module Rebind_policy = struct
  module Selected = Rebind_validation.Selected
  include Rebind_validation.Policy
end

module Rebind = struct
  type t =
    { policy : Rebind_policy.t
    ; limits : Chatml_authoring_bundle_project.Limits.t
    ; document_dir : Eio.Fs.dir_ty Eio.Path.t
    }

  let create ~policy ~limits ~document_dir = { policy; limits; document_dir }
end

module Rebound = Rebind_validation.Rebound

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
    | Stale_capture
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

module Parent_admission = struct
  module Operation = struct
    type t =
      | Begin
      | Publish
    [@@deriving equal]
  end

  type t =
    { host_id : Agent_protocol.Id.Server.t
    ; expected : Agent_protocol.Authoring.Context.Expected.t
    ; check_now :
        Operation.t
        -> connection:Connection_context.t
        -> actor:Actor.t
        -> expected:Agent_protocol.Authoring.Context.Expected.t
        -> (unit, Error.t) result
    ; previous_for :
        Bundle.Manifest.t
        -> selector:Bundle.Manifest.Script.Selector.t option
        -> checkpoint:(unit -> unit)
        -> (Chatml_project_build.t option, Error.t) result
    ; parent_pin : Store.Published_pin.t option
    }

  let create ~host_id ~expected ~check_now ~previous_for ~parent_pin () =
    { host_id; expected; check_now; previous_for; parent_pin }
  ;;
end

type t =
  { domain : int
  ; store : Store.t
  ; now : unit -> Agent_protocol.Timestamp.t
  ; upload_policy : Upload_policy.t
  ; publication : Publication.t option
  ; rebind : Rebind.t option
  ; gate : Eio.Mutex.t
  ; mutable active_cancel : Eio.Cancel.t option
  ; mutable closed : bool
  ; mutable release_hook : Eio.Switch.hook
  }

let host_id t = Store.host_id t.store

module Context_capture = struct
  type nonrec t =
    { owner : t
    ; original_actor : Actor.t
    ; selected : Rebind_validation.t
    ; pin : Store.Published_pin.t
    ; rebound : Rebound.t
    }

  let rebound t = t.rebound
  let published_pin t = t.pin
  let project_contract t = Rebind_validation.project_contract t.selected
end

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

let check t actor operation parent =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  if not (Actor.is_current actor)
  then Error Error.Expired_actor
  else if not (Upload_policy.authorize t.upload_policy actor operation)
  then Error Error.Unauthorized
  else (
    match parent with
    | None -> Ok ()
    | Some expected ->
      (match t.upload_policy.authorize_parent with
       | None -> Error Error.Selection_unavailable
       | Some authorize_parent ->
         if authorize_parent actor expected then Ok () else Error Error.Unauthorized))
;;

let check_parent_admission t ~actor ~operation ~connection ~parent ~admission =
  match parent, admission with
  | None, None -> Ok ()
  | Some expected, Some admission ->
    if
      (not
         (Agent_protocol.Id.Server.equal admission.Parent_admission.host_id (host_id t)))
      || not (Agent_protocol.Authoring.Context.Expected.equal admission.expected expected)
    then Error Error.Stale_capture
    else admission.check_now operation ~connection ~actor ~expected
  | None, Some _ | Some _, None -> Error Error.Selection_unavailable
;;

let check_upload_current_now t ~actor ~operation = check t actor operation None

let check_read t actor policy =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  if not (Actor.is_current actor)
  then Error Error.Expired_actor
  else if
    not
      (Rebind_policy.authorize_read policy actor
       && Rebind_policy.authorize_rebind policy actor)
  then Error Error.Unauthorized
  else Ok ()
;;

let check_or_raise check =
  match check () with
  | Ok () -> ()
  | Error error -> raise (Revoked error)
;;

let with_operation t ~actor ~check ~map_error ~checkpoint f =
  let open Result.Let_syntax in
  let%bind () = if is_active t then Error Error.Busy else Ok () in
  let%bind () = check () in
  let outcome =
    Eio.Mutex.use_rw ~protect:false t.gate (fun () ->
      match
        Eio.Cancel.sub (fun cancel ->
          t.active_cancel <- Some cancel;
          Exn.protect
            ~finally:(fun () -> t.active_cancel <- None)
            ~f:(fun () ->
              with_active t (fun () ->
                let%bind () = check () in
                let guarded_checkpoint () =
                  check_or_raise check;
                  checkpoint ();
                  check_or_raise check
                in
                let result =
                  f
                    ~principal:(Actor.principal actor).id
                    ~now:t.now
                    ~checkpoint:guarded_checkpoint
                in
                let%bind () = check () in
                Result.map_error result ~f:map_error)))
      with
      | result -> `Returned result
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | `Returned result -> result
  | `Raised (Revoked error, _) -> Error error
  | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let with_request ?connection ?parent_admission t ~actor ~operation ~parent ~checkpoint f =
  with_operation
    t
    ~actor
    ~check:(fun () ->
      let open Result.Let_syntax in
      let%bind () = check t actor operation parent in
      match parent, connection, parent_admission with
      | None, _, None -> Ok ()
      | Some _, Some connection, _ ->
        check_parent_admission
          t
          ~actor
          ~operation:
            (match operation with
             | Upload_policy.Operation.Write -> Parent_admission.Operation.Begin
             | Publish | Commit -> Parent_admission.Operation.Publish)
          ~connection
          ~parent
          ~admission:parent_admission
      | None, _, Some _ | Some _, None, _ -> Error Error.Selection_unavailable)
    ~map_error:Error.of_store
    ~checkpoint
    f
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

let create ?publication ?rebind store ~sw ~clock ~upload_policy =
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
    ; rebind
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

let begin_upload
      t
      ~actor
      ~connection
      ~parent_admission
      ~(request : Bundle.Begin.request)
      ~checkpoint
  =
  with_request
    ~connection
    ?parent_admission
    t
    ~actor
    ~operation:Upload_policy.Operation.Write
    ~parent:request.parent_context
    ~checkpoint
    (fun ~principal ~now ~checkpoint ->
       Store.begin_upload t.store ~principal ~request ~now ~checkpoint)
;;

let chunk t ~actor ~request ~checkpoint =
  with_request
    t
    ~actor
    ~operation:Upload_policy.Operation.Write
    ~parent:None
    ~checkpoint
    (fun ~principal ~now ~checkpoint ->
       Store.chunk t.store ~principal ~request ~now ~checkpoint)
;;

let claim_commit t ~actor ~request ~checkpoint =
  with_request
    t
    ~actor
    ~operation:Upload_policy.Operation.Commit
    ~parent:None
    ~checkpoint
    (fun ~principal ~now ~checkpoint ->
       Store.claim_commit t.store ~principal ~request ~now ~checkpoint)
;;

let publish t ~actor ~connection ~parent_admission ~sealed ~checkpoint =
  match t.publication with
  | None -> Error Error.Selection_unavailable
  | Some publication ->
    with_request
      ~connection
      ?parent_admission
      t
      ~actor
      ~operation:Upload_policy.Operation.Publish
      ~parent:(Store.sealed_parent_context sealed)
      ~checkpoint
      (fun ~principal ~now ~checkpoint ->
         let snapshot = Store.snapshot t.store ~principal sealed ~now ~checkpoint in
         match snapshot with
         | Error _ as error -> error
         | Ok snapshot ->
           let manifest = Store.Snapshot.manifest snapshot in
           let parent_previous =
             match parent_admission with
             | None -> Ok None
             | Some admission ->
               Validation.Parent_previous.prepare
                 manifest
                 ~checkpoint
                 ~select:(fun selector ~checkpoint ->
                   admission.Parent_admission.previous_for manifest ~selector ~checkpoint)
               |> Result.map ~f:Option.some
           in
           let parent_previous =
             match parent_previous with
             | Ok value -> value
             | Error error -> raise (Revoked error)
           in
           let selection =
             Validation.admit
               publication.policy
               ~actor
               ~manifest
               ~parent_previous
               ~checkpoint
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
                   check_or_raise (fun () ->
                     let open Result.Let_syntax in
                     let%bind () =
                       check
                         t
                         actor
                         Upload_policy.Operation.Publish
                         (Store.sealed_parent_context sealed)
                     in
                     check_parent_admission
                       t
                       ~actor
                       ~operation:Parent_admission.Operation.Publish
                       ~connection
                       ~parent:(Store.sealed_parent_context sealed)
                       ~admission:parent_admission);
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
                   ~parent_pin:
                     (Option.bind parent_admission ~f:(fun admitted ->
                        admitted.Parent_admission.parent_pin))
                   ~now
                   ~checkpoint:selected_checkpoint
                   ~final_admission)))
;;

let rebind_with_pin t ~actor ~bundle_id ~checkpoint =
  match t.rebind with
  | None -> Error Error.Selection_unavailable
  | Some config ->
    let admitted = ref None in
    let check_current () =
      let open Result.Let_syntax in
      let%bind () = check_read t actor config.policy in
      match !admitted with
      | None -> Ok ()
      | Some admitted ->
        if
          Rebind_validation.check_current
            admitted
            ~policy:config.policy
            ~actor
            ~checkpoint:Fn.id
        then Ok ()
        else Error Error.Stale_capture
    in
    with_operation
      t
      ~actor
      ~check:check_current
      ~map_error:Fn.id
      ~checkpoint
      (fun ~principal ~now ~checkpoint ->
         let selected_checkpoint = checkpoint in
         let admit_metadata receipt manifest =
           check_or_raise check_current;
           let selected =
             Rebind_validation.admit
               config.policy
               ~actor
               ~receipt
               ~manifest
               ~checkpoint:Fn.id
           in
           (match selected with
            | Error Rebind_validation.Error.Selection_unavailable ->
              raise (Revoked Error.Selection_unavailable)
            | Error Stale_capture -> raise (Revoked Error.Stale_capture)
            | Error Invalid_closure -> raise (Revoked Error.Invalid_closure)
            | Error Resource_limit -> raise (Revoked Error.Resource_limit)
            | Ok selected -> admitted := Some selected);
           check_or_raise check_current
         in
         match
           Store.with_published
             t.store
             ~principal
             ~bundle_id
             ~now
             ~checkpoint:selected_checkpoint
             ~admit_metadata
             ~f:(fun capture ->
               let selected = Option.value_exn !admitted in
               let result =
                 Rebind_validation.validate
                   selected
                   ~capture
                   ~document_dir:config.document_dir
                   ~limits:config.limits
                   ~checkpoint:selected_checkpoint
               in
               selected_checkpoint ();
               Result.map_error result ~f:(function
                 | Rebind_validation.Error.Selection_unavailable ->
                   Error.Selection_unavailable
                 | Stale_capture -> Error.Stale_capture
                 | Invalid_closure -> Error.Invalid_closure
                 | Resource_limit -> Error.Resource_limit)
               |> Result.map ~f:(fun rebound ->
                 rebound, Store.Published_capture.pin capture, selected))
         with
         | Error problem -> Error (Error.of_store problem)
         | Ok result -> result)
;;

let rebind t ~actor ~bundle_id ~checkpoint =
  Result.map (rebind_with_pin t ~actor ~bundle_id ~checkpoint) ~f:(fun (result, _, _) ->
    result)
;;

let capture_for_context t ~actor ~bundle_id ~checkpoint =
  Result.map
    (rebind_with_pin t ~actor ~bundle_id ~checkpoint)
    ~f:(fun (result, pin, selected) ->
      Context_capture.
        { owner = t; original_actor = actor; selected; pin; rebound = result })
;;

let check_context_selection_policy_now t ~actor (capture : Context_capture.t) =
  match t.rebind with
  | None -> Error Error.Selection_unavailable
  | Some config ->
    if not (phys_equal t capture.owner)
    then Error Error.Stale_capture
    else (
      let check () =
        let open Result.Let_syntax in
        let%bind () = check_read t capture.original_actor config.policy in
        let%bind () = check_read t actor config.policy in
        if
          Rebind_validation.check_current
            capture.selected
            ~policy:config.policy
            ~actor:capture.original_actor
            ~checkpoint:Fn.id
          && Rebind_validation.check_current
               capture.selected
               ~policy:config.policy
               ~actor
               ~checkpoint:Fn.id
        then Ok ()
        else Error Error.Stale_capture
      in
      check ())
;;

let check_context_selection_now t ~actor (capture : Context_capture.t) =
  let open Result.Let_syntax in
  let%bind () = check_context_selection_policy_now t ~actor capture in
  Store.check_published_pin_now
    t.store
    ~principal:(Actor.principal actor).id
    capture.pin
    ~now:t.now
  |> Result.map_error ~f:Error.of_store
;;

let check_context_current t ~actor (capture : Context_capture.t) ~checkpoint =
  match t.rebind with
  | None -> Error Error.Selection_unavailable
  | Some _ ->
    if not (phys_equal t capture.owner)
    then Error Error.Stale_capture
    else
      with_operation
        t
        ~actor
        ~check:(fun () -> check_context_selection_policy_now t ~actor capture)
        ~map_error:Fn.id
        ~checkpoint
        (fun ~principal ~now ~checkpoint ->
           Store.check_published_pin t.store ~principal capture.pin ~now ~checkpoint
           |> Result.map_error ~f:Error.of_store)
;;

let status t ~actor ~(request : Bundle.Status.request) ~checkpoint =
  with_request
    t
    ~actor
    ~operation:Upload_policy.Operation.Write
    ~parent:None
    ~checkpoint
    (fun ~principal ~now ~checkpoint ->
       Store.status t.store ~principal ~upload_id:request.upload_id ~now ~checkpoint)
;;

let cancel t ~actor ~upload_id ~checkpoint =
  with_request
    t
    ~actor
    ~operation:Upload_policy.Operation.Write
    ~parent:None
    ~checkpoint
    (fun ~principal ~now ~checkpoint ->
       Store.cancel t.store ~principal ~upload_id ~now ~checkpoint)
;;
