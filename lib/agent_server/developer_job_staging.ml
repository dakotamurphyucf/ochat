open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)
let ( let* ) value f = Result.bind value ~f

type status =
  | Provisional
  | Aborted
  | Published

type staged =
  { job : P.Developer_job.t
  ; reservation : Job_capacity.reservation
  ; mutable phase : status
  }

type settlement =
  | Selected_installed
  | Selected_omitted
  | Absent
[@@deriving equal]

type t =
  { max_staged_jobs : int
  ; staged : (P.Id.Job.t, staged) Hashtbl.t
  ; mutable phase : phase
  }

and phase =
  | Open
  | Sealed of selection
  | Handed_off of selection
  | Settled of selection * settlement
  | Closed_before_handoff

and selection =
  { issuer : t
  ; jobs : P.Developer_job.t list
  ; ids : P.Id.Job.t list
  }

module Selection = struct
  type t = selection

  let jobs t = t.jobs
  let ids t = t.ids
end

let create ~max_staged_jobs =
  if max_staged_jobs <= 0
  then invalid "developer staged job bound must be positive"
  else Ok { max_staged_jobs; staged = Hashtbl.create (module P.Id.Job); phase = Open }
;;

let count t = Hashtbl.length t.staged
let is_empty t = Hashtbl.is_empty t.staged
let contains t id = Hashtbl.mem t.staged id
let status t id = Option.map (Hashtbl.find t.staged id) ~f:(fun staged -> staged.phase)

let reserve t ~capacity ~key ~prepared =
  match t.phase with
  | Sealed _ | Handed_off _ | Settled _ | Closed_before_handoff ->
    invalid "developer job selection is no longer open"
  | Open ->
    if count t >= t.max_staged_jobs
    then
      Error
        (P.Error.create
           P.Error.Resource_limit
           ~message:"developer root staged job limit was reached"
           ~retryable:false
           ())
    else (
      let job = Agent_session.Developer_root_job_admission.Prepared.job prepared in
      let id = P.Developer_job.id job in
      if Hashtbl.mem t.staged id
      then invalid "developer root job ID was already staged"
      else
        let* host_job = P.Host_job.of_developer job in
        let* reserved = Job_capacity.reserve_host_job capacity key ~job:host_job in
        match reserved with
        | None ->
          Error
            (P.Error.create
               P.Error.Resource_limit
               ~message:"shared job capacity is full"
               ~retryable:true
               ())
        | Some reservation ->
          Hashtbl.set t.staged ~key:id ~data:{ job; reservation; phase = Provisional };
          Ok id)
;;

let rollback t id =
  match Hashtbl.find t.staged id with
  | None -> invalid "developer staged job is not owned by this action"
  | Some { phase = Aborted; _ } -> Ok ()
  | Some { phase = Published; _ } ->
    invalid "published developer job cannot be rolled back"
  | Some ({ phase = Provisional; _ } as staged) ->
    let allowed =
      match t.phase with
      | Open -> true
      | Sealed selection -> not (List.mem selection.ids id ~equal:P.Id.Job.equal)
      | Handed_off _ | Settled _ | Closed_before_handoff -> false
    in
    if not allowed
    then invalid "selected developer job retains its physical handoff"
    else (
      Job_capacity.abort staged.reservation;
      staged.phase <- Aborted;
      Ok ())
;;

let abort_pre_handoff t =
  match t.phase with
  | Handed_off _ | Settled _ -> invalid "developer job handoff retains staged capacity"
  | Closed_before_handoff -> Ok ()
  | Open | Sealed _ ->
    Hashtbl.iter t.staged ~f:(fun staged ->
      match staged.phase with
      | Provisional ->
        Job_capacity.abort staged.reservation;
        staged.phase <- Aborted
      | Aborted | Published -> ());
    t.phase <- Closed_before_handoff;
    Ok ()
;;

let seal t ~selected =
  match t.phase with
  | Sealed _ | Handed_off _ | Settled _ | Closed_before_handoff ->
    invalid "developer root action cannot be sealed"
  | Open ->
    let seen = Hash_set.create (module P.Id.Job) in
    let* jobs, _count =
      List.fold_result selected ~init:([], 0) ~f:(fun (jobs, count) id ->
        if count >= t.max_staged_jobs
        then invalid "developer root selection exceeds its bound"
        else if Hash_set.mem seen id
        then invalid "duplicate developer root selection"
        else (
          Hash_set.add seen id;
          match Hashtbl.find t.staged id with
          | None -> invalid "developer root selection is not staged here"
          | Some { phase = Provisional; job; _ } -> Ok (job :: jobs, count + 1)
          | Some { phase = Aborted | Published; _ } ->
            invalid "developer root selection is no longer provisional"))
    in
    let selection = { issuer = t; jobs = List.rev jobs; ids = selected } in
    t.phase <- Sealed selection;
    Ok selection
;;

let mark_handoff t selection =
  if not (phys_equal selection.issuer t)
  then invalid "developer selection belongs to another action"
  else (
    match t.phase with
    | Sealed issued when phys_equal issued selection ->
      t.phase <- Handed_off selection;
      Ok ()
    | Handed_off issued when phys_equal issued selection -> Ok ()
    | Open | Sealed _ | Handed_off _ | Settled _ | Closed_before_handoff ->
      invalid "developer selection is no longer available for handoff")
;;

let settle t selection disposition =
  if not (phys_equal selection.issuer t)
  then invalid "developer selection belongs to another action"
  else (
    match t.phase with
    | Settled (issued, prior)
      when phys_equal issued selection && equal_settlement prior disposition -> Ok ()
    | Handed_off issued when phys_equal issued selection ->
      let selected_ids = Hash_set.of_list (module P.Id.Job) selection.ids in
      Hashtbl.iteri t.staged ~f:(fun ~key:id ~data:staged ->
        match staged.phase with
        | Provisional ->
          if
            match disposition with
            | Selected_installed -> Hash_set.mem selected_ids id
            | Selected_omitted | Absent -> false
          then (
            Job_capacity.publish staged.reservation;
            staged.phase <- Published)
          else (
            Job_capacity.abort staged.reservation;
            staged.phase <- Aborted)
        | Aborted | Published -> ());
      t.phase <- Settled (selection, disposition);
      Ok ()
    | Open | Sealed _ | Handed_off _ | Settled _ | Closed_before_handoff ->
      invalid "developer selection cannot be settled here")
;;
