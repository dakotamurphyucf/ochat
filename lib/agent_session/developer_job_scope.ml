open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store

type t =
  { store : Store.t
  ; mutable current : P.Execution_owner.developer
  ; max_active : int
  ; domain_id : int
  ; mutable closed : bool
  ; mutable active : active list
  }

and active =
  { issuer : t
  ; claim : Store.Claim.t
  ; domain_id : int
  ; live : bool Atomic.t
  ; mutable cancel : Eio.Cancel.t option
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  }

let active_key = Eio.Fiber.create_key ()
let invalid message = Error (P.Error.invalid_request message)
let same_domain t = Int.equal t.domain_id (Stdlib.Domain.self () :> int)

let of_store_error _error =
  P.Error.create
    P.Error.Persistence_error
    ~message:"developer job claim is unavailable"
    ~retryable:true
    ()
;;

let create ~store ~initial ~max_active =
  if max_active <= 0 || not (Store.issued_by store initial)
  then invalid "invalid developer job scope owner or bound"
  else (
    let owner = Store.owner initial in
    match Store.is_latest_active initial with
    | Error error -> Error (of_store_error error)
    | Ok false -> invalid "developer job scope requires the active latest epoch"
    | Ok true ->
      Ok
        { store
        ; current = owner
        ; max_active
        ; domain_id = (Stdlib.Domain.self () :> int)
        ; closed = false
        ; active = []
        })
;;

let claim active = active.claim

let check_local t active =
  phys_equal active.issuer t
  && same_domain t
  && Int.equal active.domain_id t.domain_id
  && (not t.closed)
  && Atomic.get active.live
  && Option.value_map active.cancel ~default:false ~f:(fun context ->
    Option.is_none (Eio.Cancel.get_error context))
  && Option.value_map (Eio.Fiber.get active_key) ~default:false ~f:(fun current ->
    phys_equal current active)
;;

let check_claim_now t active ~owner ~capture_fingerprint ~job_id ~attempt =
  if not (check_local t active)
  then invalid "developer job parent is no longer live"
  else (
    let claim = active.claim in
    let job = Store.Claim.job claim in
    if
      P.Execution_owner.equal_developer (Store.Claim.owner claim) owner
      && P.Execution_owner.equal_developer owner t.current
      && Chatml_source.Digest.equal
           (P.Developer_job.capture_fingerprint job)
           capture_fingerprint
      && P.Id.Job.equal (P.Developer_job.id job) job_id
      && Int.equal (Store.Claim.attempt claim) attempt
    then Ok ()
    else invalid "developer job parent does not match the claimed attempt")
;;

let with_claim t ~handle ~claim f =
  if t.closed || not (same_domain t)
  then invalid "developer job scope has closed or changed domain"
  else if List.length t.active >= t.max_active
  then invalid "developer job scope is full"
  else if
    (not (Store.issued_by t.store handle))
    || (not (P.Execution_owner.equal_developer (Store.owner handle) t.current))
    || not (P.Execution_owner.equal_developer (Store.Claim.owner claim) t.current)
  then invalid "foreign developer job claim"
  else (
    let finished, finish = Eio.Promise.create () in
    let active =
      { issuer = t
      ; claim
      ; domain_id = t.domain_id
      ; live = Atomic.make false
      ; cancel = None
      ; finished
      ; finish
      }
    in
    (* Admission itself owns a registry slot. Closing the issuer cancels and
       joins a Store read even if it has not yet consumed the Claim. *)
    t.active <- active :: t.active;
    let entered_this_call = ref false in
    let callback_started = ref false in
    Exn.protect
      ~finally:(fun () ->
        if !entered_this_call && not !callback_started
        then
          ignore
            (Store.Claim.abandon_before_callback claim ~handle
             : (unit, Store.Error.t) result);
        Atomic.set active.live false;
        active.cancel <- None;
        t.active <- List.filter t.active ~f:(fun item -> not (phys_equal item active));
        Eio.Promise.resolve active.finish ())
      ~f:(fun () ->
        Eio.Cancel.sub (fun context ->
          active.cancel <- Some context;
          match
            Store.Claim.try_enter
              claim
              ~handle
              ~admit:(fun () ->
                (not t.closed)
                && P.Execution_owner.equal_developer t.current (Store.Claim.owner claim))
              ~on_enter:(fun () -> entered_this_call := true)
          with
          | Error error -> Error (of_store_error error)
          | Ok false -> invalid "developer job claim entered already"
          | Ok true ->
            if
              t.closed
              || not
                   (P.Execution_owner.equal_developer t.current (Store.Claim.owner claim))
            then invalid "developer job claim expired during admission"
            else (
              Atomic.set active.live true;
              callback_started := true;
              Eio.Fiber.with_binding active_key active (fun () ->
                Eio.Switch.run (fun sw -> Ok (f ~active ~sw)))))))
;;

let check_parent t active ~owner ~capture_fingerprint ~job_id ~attempt =
  let open Result.Let_syntax in
  let%bind () = check_claim_now t active ~owner ~capture_fingerprint ~job_id ~attempt in
  match Store.Claim.current_for_effect active.claim with
  | Error error -> Error (of_store_error error)
  | Ok false -> invalid "developer job parent is no longer current"
  | Ok true -> check_claim_now t active ~owner ~capture_fingerprint ~job_id ~attempt
;;

type issuer_state = t

module Parent = struct
  type t =
    { issuer : issuer_state
    ; active : active
    ; job_id : P.Id.Job.t
    ; attempt : int
    ; nested_depth : int
    }

  let job_id t = t.job_id
  let attempt t = t.attempt
  let nested_depth t = t.nested_depth

  let same_active t active =
    phys_equal t.active active
    && phys_equal t.issuer active.issuer
    && check_local t.issuer active
  ;;

  let same_claim left right =
    phys_equal left.issuer right.issuer
    && phys_equal left.active right.active
    && check_local left.issuer left.active
    && P.Id.Job.equal left.job_id right.job_id
    && Int.equal left.attempt right.attempt
  ;;

  let check t ~owner ~capture_fingerprint =
    check_parent
      t.issuer
      t.active
      ~owner
      ~capture_fingerprint
      ~job_id:t.job_id
      ~attempt:t.attempt
  ;;

  let check_now t ~owner ~capture_fingerprint =
    check_claim_now
      t.issuer
      t.active
      ~owner
      ~capture_fingerprint
      ~job_id:t.job_id
      ~attempt:t.attempt
  ;;
end

let parent t active =
  let claim = active.claim in
  let job = Store.Claim.job claim in
  let open Result.Let_syntax in
  let%map () =
    check_claim_now
      t
      active
      ~owner:(Store.Claim.owner claim)
      ~capture_fingerprint:(P.Developer_job.capture_fingerprint job)
      ~job_id:(P.Developer_job.id job)
      ~attempt:(Store.Claim.attempt claim)
  in
  Parent.
    { issuer = t
    ; active
    ; job_id = P.Developer_job.id job
    ; attempt = Store.Claim.attempt claim
    ; nested_depth = P.Developer_job.Launch.nested_depth (P.Developer_job.launch job)
    }
;;

let owns_parent t parent = phys_equal t parent.Parent.issuer

let advance_epoch t ~new_current =
  if t.closed || not (same_domain t)
  then invalid "developer job scope has closed or changed domain"
  else if not (Store.issued_by t.store new_current)
  then invalid "foreign developer job store handle"
  else (
    let owner = Store.owner new_current in
    let prior = t.current in
    if
      (not (P.Id.Server.equal owner.host_id prior.host_id))
      || (not (P.Id.Principal.equal owner.principal_id prior.principal_id))
      || (not (P.Id.Developer_run.equal owner.run_id prior.run_id))
      || not (Int.equal owner.epoch (prior.epoch + 1))
    then invalid "developer job epoch is not the exact successor"
    else (
      match Store.is_latest_active new_current with
      | Error error -> Error (of_store_error error)
      | Ok false -> invalid "new developer job epoch is inactive or superseded"
      | Ok true ->
        if
          t.closed
          || (not (P.Execution_owner.equal_developer t.current prior))
          || not (same_domain t)
        then invalid "developer job scope changed during epoch advance"
        else (
          t.current <- owner;
          Ok ())))
;;

let close_and_wait t =
  if not (same_domain t)
  then invalid "developer job scope belongs to another domain"
  else if
    Option.value_map (Eio.Fiber.get active_key) ~default:false ~f:(fun active ->
      phys_equal active.issuer t)
  then invalid "developer job worker cannot close its own scope"
  else (
    t.closed <- true;
    let active = t.active in
    Eio.Cancel.protect (fun () ->
      List.iter active ~f:(fun active ->
        Option.iter active.cancel ~f:(fun context -> Eio.Cancel.cancel context Exit));
      List.iter active ~f:(fun active -> Eio.Promise.await active.finished));
    Ok ())
;;
