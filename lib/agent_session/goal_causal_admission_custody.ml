(** Actor-private process-local causal Attempt custody. These representations
    own one actual prospective canonical occurrence and its ordered remote
    reservations. No value/ID/receipt creates permission or authenticates an
    Actor, source, Store or original. The issuing Actor performs those checks.
    Mutable access is confined to the issuing Eio domain. ACK installers are
    pure assignments prevalidated before the actual writer; unknown attempts
    remain owned and are never replaced by another canonical invocation. *)
open! Core

module P = Agent_protocol
module C = Policy_cause_data
module PC = Policy_canonical_usage
module Book = Goal_policy_contributions
module Existing = Goal_descendant_admission_custody
module Basis = Existing.Child_accounting_basis
module Occurrence = Goal_descendant_occurrence

let invalid message = Error (P.Error.create Conflict ~message ~retryable:false ())
let on_domain domain = Int.equal domain (Stdlib.Domain.self () :> int)

module Preparation = struct
  (* Created only from actual callee inference preparation, after original/source
     selection and actual extant caused-work membership checks. Fields do not
     themselves grant authority. Actual native/Graph/original stay in Actor Plan. *)
  type t =
    { identity : unit ref
    ; basis : Basis.t
    ; cause : C.Key.t
    ; work : C.Allocation_work.t
    ; occurrence : Occurrence.t
    ; mutable set_claim : unit ref option
    }

  let on_domain t = on_domain t.basis.domain

  let check_owner t ~issuer =
    if on_domain t && phys_equal t.basis.issuing_actor issuer
    then Ok ()
    else invalid "causal preparation belongs to another actual Actor/domain"
  ;;
end

module Route = struct
  (* Negative matching evidence, copied from authenticated current governance.
     None of these DATA alternatives creates permission. Direct Local is selected
     by actual Caller issuer; forwarding revalidates 605's complete source export;
     Inherited retains exact Scope and actual lower records, never synthesized. *)
  type evidence =
    | Caller_local
    | Inherited_scope of Agent_store.Delegation_goal_origin.Scope.t
    | Forwarded of C.Forwarding.t

  type t =
    { preparation : Preparation.t
    ; owner : P.Session_ref.t
    ; target : C.Goal_target.t
    ; evidence : evidence
    ; controller : Recurrence_controller.t
    ; root_issuer : unit ref
    ; source_owner : Runtime_source_owner.t
    ; domain : int
    ; records : Agent_store.Delegation_store.record list
    }
end

module Terminal_operation = struct
  (* Shared immutable receipt. It never retains a prior settlement or write,
     so newest imported ACKs cannot retain predecessor correction chains. *)
  type t =
    { identity : unit ref
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; writer : Session_commit_attempt.t
    }
end

module Remote = struct
  (* One recursion group is necessary: the installed reservation points to SAME
     attempt/write; the already-owned attempt phase then points to reservation.
     Construct attempt first, write and reservation later; prebuild next phase
     before writer. No recursive module, Actor import or initialization cycle. *)
  type attempt =
    { identity : unit ref
    ; set_identity : unit ref
    ; route : Route.t
    ; basis : Basis.t
    ; policy : Goal_root_policy.t
    ; request : Book.Request.t
    ; mutable phase : phase
    }

  and phase =
    | Begun
    | Writer_prepared of write
    | Reserved of reservation
    | Not_reserved of not_reserved

  and write =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; kind : write_kind
    ; writer : Session_commit_attempt.t
    }

  and write_kind =
    | Reserve of Book.Pending.t
    | Policy_hold of P.Id.Goal.t

  and not_reserved =
    | Blocked of Goal_admission_policy.Count_availability.t
    | Rejected_before_writer of write * Session_commit_attempt.No_writer.t
    | Policy_hold_acknowledged of write

  and reservation =
    { attempt : attempt
    ; write : write
    ; pending : Book.Pending.t
    ; installed : Session_state.t
    ; mutable terminal : terminal
    }

  and terminal =
    | Settled of settled
    | Terminal_prepared of terminal_write

  and settled =
    | Pending
    | Confirmed of import_ack
    | No_writer_closed of close_ack

  and terminal_write =
    { operation : Terminal_operation.t
    ; reservation : reservation
    ; prior : settled
    ; kind : terminal_kind
    ; mutable acknowledged : terminal_ack option
    }

  and terminal_kind =
    | Import of PC.Projection.t
    | Close of no_writer
    | Correction of PC.Projection.t

  and terminal_ack =
    | Imported of import_ack
    | Close_installed of close_ack

  and import_ack =
    { reservation : reservation
    ; operation : Terminal_operation.t
    ; installed : Session_state.t
    ; projection : PC.Projection.t
    }

  and close_ack =
    { reservation : reservation
    ; previous : Session_state.t
    ; candidate : Session_transition.t
    ; installed : Session_state.t
    ; closed : no_writer
    }

  (* no_writer is an opaque actual callee seal, never serialized. It either seals
     a slot whose canonical candidate was never invoked, or carries the actual
     same-candidate commit kernel's observed no-writer token. *)
  and no_writer =
    { preparation : Preparation.t
    ; slot_identity : unit ref
    ; reason : no_writer_reason
    }

  and no_writer_reason =
    | Never_invoked
    | Observed_before_writer of Session_commit_attempt.No_writer.t
end

module Frozen_routes = struct
  (* Issued by finite callee selection AFTER actual physical receipts/native
     preview are available and BEFORE any causal Root Begin. Exact request
     order/cause/work/occurrence comes from this same PC selection. *)
  type t =
    { selection : PC.Selection.t
    ; requests : Book.Request.t list
    }
end

module Set = struct
  type phase =
    | Unselected
    | Acquiring of Frozen_routes.t
    | Complete of Frozen_routes.t

  type t =
    { identity : unit ref
    ; preparation : Preparation.t
    ; mutable attempts : Remote.attempt list
    ; mutable phase : phase
    }

  let create preparation =
    if not (Preparation.on_domain preparation)
    then invalid "causal reservation set belongs to another domain"
    else (
      match preparation.Preparation.set_claim with
      | Some _ -> invalid "causal preparation already owns its reservation set"
      | None ->
        let identity = ref () in
        let t = { identity; preparation; attempts = []; phase = Unselected } in
        preparation.set_claim <- Some identity;
        Ok t)
  ;;

  let check_claim t =
    if not (Preparation.on_domain t.preparation)
    then invalid "causal reservation set belongs to another domain"
    else (
      match t.preparation.set_claim with
      | Some claim when phys_equal claim t.identity -> Ok ()
      | None | Some _ -> invalid "causal reservation set lost its exact preparation claim")
  ;;

  let freeze t ~cause ~selection =
    let open Result.Let_syntax in
    let%bind () = check_claim t in
    let%bind () =
      if
        C.Key.equal t.preparation.cause (C.key cause)
        && PC.Selection.matches_subject
             selection
             ~cause:t.preparation.cause
             ~work:t.preparation.work
             ~occurrence:t.preparation.occurrence
      then Ok ()
      else invalid "causal selection has another actual cause/work/occurrence"
    in
    let%bind () =
      match t.phase with
      | Unselected -> Ok ()
      | Acquiring _ | Complete _ -> invalid "causal target selection is already frozen"
    in
    let callee = Occurrence.Frame.session (Occurrence.frame t.preparation.occurrence) in
    let targets =
      PC.Selection.choices selection
      |> List.filter_map ~f:(fun choice ->
        match PC.Choice.coverage choice with
        | Covered_local | Covered_physical -> None
        | Causal_overlay ->
          let target = PC.Choice.target choice in
          if P.Session_ref.equal (C.Goal_target.owner target) callee
          then None
          else Some target)
      |> List.sort ~compare:(fun a b ->
        String.compare (C.Goal_target.root a) (C.Goal_target.root b))
    in
    let%bind () =
      List.fold_result targets ~init:None ~f:(fun previous target ->
        let key = C.Goal_target.root target in
        match previous with
        | Some before when String.compare before key >= 0 ->
          invalid "causal selection has conflicting duplicate governing roots"
        | None | Some _ -> Ok (Some key))
      |> Result.map ~f:ignore
    in
    let%map requests =
      List.map targets ~f:(fun target ->
        Book.Request.create
          cause
          ~target
          ~work:t.preparation.work
          ~occurrence:t.preparation.occurrence)
      |> Result.all
    in
    let frozen = { Frozen_routes.selection; requests } in
    t.phase <- Acquiring frozen;
    frozen
  ;;

  let frozen t =
    let%bind.Result () = check_claim t in
    match t.phase with
    | Unselected -> invalid "causal selection is not frozen"
    | Acquiring frozen | Complete frozen -> Ok frozen
  ;;

  let next_request t (route : Route.t) =
    let open Result.Let_syntax in
    let%bind () = check_claim t in
    let%bind () =
      if phys_equal route.preparation t.preparation
      then Ok ()
      else invalid "causal route belongs to another preparation"
    in
    match t.phase with
    | Unselected -> invalid "causal Root Begin precedes frozen selection"
    | Complete _ -> invalid "causal reservation vector is already complete"
    | Acquiring frozen ->
      let%bind request =
        List.nth frozen.requests (List.length t.attempts)
        |> Result.of_option
             ~error:(P.Error.invalid_request "causal reservation exceeds frozen vector")
      in
      let target = PC.Stream.target (Book.Request.stream request) in
      if
        C.Goal_target.equal route.target target
        && P.Session_ref.equal route.owner (C.Goal_target.owner target)
      then Ok request
      else invalid "causal Root Begin differs from the next exact frozen target"
  ;;

  let retain_next t (attempt : Remote.attempt) =
    let open Result.Let_syntax in
    let%bind request = next_request t attempt.route in
    if phys_equal attempt.set_identity t.identity && phys_equal attempt.request request
    then (
      t.attempts <- t.attempts @ [ attempt ];
      Ok ())
    else invalid "causal Root Begin has another exact set/request owner"
  ;;

  let complete t =
    let open Result.Let_syntax in
    let%bind frozen = frozen t in
    let%bind () =
      if
        List.equal
          phys_equal
          frozen.requests
          (List.map t.attempts ~f:(fun (attempt : Remote.attempt) -> attempt.request))
      then Ok ()
      else invalid "causal reservation vector is incomplete or displaced"
    in
    match t.phase with
    | Unselected -> invalid "causal selection is not frozen"
    | Acquiring _ ->
      t.phase <- Complete frozen;
      Ok ()
    | Complete _ -> Ok ()
  ;;
end

module Canonical_write = struct
  type t =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; canonical : Goal_descendants.Canonical.t
    ; selection : PC.Selection.t
    ; writer : Session_commit_attempt.t
    }
end

module Acknowledgement = struct
  type t =
    { identity : unit ref
    ; preparation : Preparation.t
    ; write : Canonical_write.t
    ; installed : Session_state.t
    ; reservations : Set.t
    }
end

module Slot = struct
  type phase =
    | Preparing
    | Writer_prepared of Canonical_write.t
    | Acknowledged of Acknowledgement.t
    | Sealed of Remote.no_writer
    | Released of release

  and release =
    | Unwritten of Remote.no_writer
    | Confirmed of Acknowledgement.t * Remote.import_ack list

  type cleanup =
    | Idle
    | Running of unit Actor_reply.t Eio.Promise.t

  type t =
    { identity : unit ref
    ; preparation : Preparation.t
    ; reservations : Set.t
    ; mutable phase : phase
    ; mutable cancellation_requested : bool
    ; mutable cleanup : cleanup
    }

  let create preparation =
    let%map.Result reservations = Set.create preparation in
    { identity = ref ()
    ; preparation
    ; reservations
    ; phase = Preparing
    ; cancellation_requested = false
    ; cleanup = Idle
    }
  ;;

  let check_owner t ~issuer = Preparation.check_owner t.preparation ~issuer

  let check_open t ~issuer =
    let open Result.Let_syntax in
    let%bind () = check_owner t ~issuer in
    let%bind () = Set.check_claim t.reservations in
    if t.cancellation_requested
    then invalid "causal canonical admission was cancelled"
    else (
      match t.phase with
      | Preparing -> Ok ()
      | Writer_prepared _ | Acknowledged _ | Sealed _ | Released _ ->
        invalid "causal canonical admission is no longer preparing")
  ;;

  let seal t ~issuer =
    let open Result.Let_syntax in
    let%bind () = check_owner t ~issuer in
    match t.phase with
    | Sealed closed | Released (Unwritten closed) -> Ok closed
    | Acknowledged _ | Released (Confirmed _) ->
      invalid "acknowledged causal admission cannot prove no writer"
    | Preparing ->
      let closed =
        { Remote.preparation = t.preparation
        ; slot_identity = t.identity
        ; reason = Never_invoked
        }
      in
      t.cancellation_requested <- true;
      t.phase <- Sealed closed;
      Ok closed
    | Writer_prepared write ->
      let%bind observed = Session_commit_attempt.No_writer.seal write.writer in
      let%bind () =
        if
          Session_commit_attempt.No_writer.matches_canonical
            observed
            ~attempt:write.writer
            ~slot:t.identity
            ~preparation:t.preparation.identity
            ~previous:write.previous
            ~candidate:write.candidate
        then Ok ()
        else invalid "causal no-writer token has another exact canonical owner"
      in
      let closed =
        { Remote.preparation = t.preparation
        ; slot_identity = t.identity
        ; reason = Observed_before_writer observed
        }
      in
      t.cancellation_requested <- true;
      t.phase <- Sealed closed;
      Ok closed
  ;;
end

module Install_state = struct
  type t =
    | Prepared
    | Installed
    | Rejected
end

let check_root (attempt : Remote.attempt) ~issuer =
  let basis = attempt.basis in
  let route = attempt.route in
  if
    (not (on_domain basis.domain))
    || (not (on_domain route.domain))
    || not (Preparation.on_domain route.preparation)
  then invalid "causal root write belongs to another domain"
  else if
    (not (phys_equal basis.issuing_actor issuer))
    || (not (phys_equal route.root_issuer issuer))
    || (not (phys_equal basis.controller route.controller))
    || (not
          (Runtime_source_owner.Identity.equal
             (Runtime_source_owner.identity basis.source_owner)
             (Runtime_source_owner.identity route.source_owner)))
    || (not
          (P.Session_ref.equal
             route.owner
             (Recurrence_controller.session basis.controller)))
    || (not (P.Session_ref.equal route.owner (C.Goal_target.owner route.target)))
    || (not
          (C.Goal_target.equal
             route.target
             (PC.Stream.target (Book.Request.stream attempt.request))))
    || not
         (P.Id.Goal.equal
            (Goal_root_policy.goal_id attempt.policy)
            (C.Goal_target.goal_id route.target))
  then invalid "causal root write has another retained root owner"
  else (
    match route.preparation.set_claim with
    | Some claim when phys_equal claim attempt.set_identity -> Ok ()
    | None | Some _ -> invalid "causal root write lost its exact reservation set")
;;

let check_state (attempt : Remote.attempt) (state : Session_state.t) =
  if
    P.Id.Session.equal
      state.identity.session_id
      (P.Session_ref.session_id attempt.route.owner)
  then Ok ()
  else invalid "causal root candidate belongs to another session"
;;

let check_begun attempt ~issuer ~previous ~candidate =
  let open Result.Let_syntax in
  let%bind () = check_root attempt ~issuer in
  let%bind () = check_state attempt previous in
  let%bind () = check_state attempt candidate.Session_transition.state in
  if not (phys_equal attempt.basis.state previous)
  then invalid "causal root preparation has another original State"
  else (
    match attempt.phase with
    | Begun -> Ok ()
    | Writer_prepared _ | Reserved _ | Not_reserved _ ->
      invalid "causal root attempt already owns a write or outcome")
;;

let check_pending (attempt : Remote.attempt) pending =
  let preparation = attempt.route.preparation in
  if
    P.Session_ref.equal (Book.Pending.owner pending) attempt.route.owner
    && P.Id.Goal.equal
         (Book.Pending.goal_id pending)
         (C.Goal_target.goal_id attempt.route.target)
    && C.Key.equal (Book.Pending.cause pending) preparation.cause
    && C.Allocation_work.equal (Book.Pending.work pending) preparation.work
    && Occurrence.equal (Book.Pending.occurrence pending) preparation.occurrence
  then Ok ()
  else invalid "causal Pending has another retained request subject"
;;

let existing_pending (attempt : Remote.attempt) (state : Session_state.t) pending =
  let open Result.Let_syntax in
  let%bind result =
    Book.reserve
      state.goal_policy_contributions
      ~owner:attempt.route.owner
      ~request:attempt.request
  in
  match result with
  | Already_pending actual when Book.Pending.equal actual pending -> Ok ()
  | Reserved _ | Already_pending _ | Busy _ ->
    invalid "causal candidate does not retain the exact Pending"
;;

let book (state : Session_state.t) =
  Result.of_option
    state.goal_policy_contributions
    ~error:(P.Error.invalid_request "causal root Book is unavailable")
;;

let metadata (candidate : Session_transition.t) =
  match candidate.delta with
  | Goal_policy_contributions_changed update -> Ok update
  | _ -> invalid "causal root write requires one direct Book metadata leaf"
;;

let reserve_writer (attempt : Remote.attempt) ~previous ~candidate ~kind =
  let writer =
    Session_commit_attempt.create
      ~context:(Causal_reserve { attempt = attempt.identity; set = attempt.set_identity })
      ~previous
      ~candidate
  in
  Remote.{ previous; candidate; kind; writer }
;;

let check_reserve_write
      (attempt : Remote.attempt)
      (write : Remote.write)
      ~issuer
      ~state
      ~candidate
  =
  let open Result.Let_syntax in
  let%bind () = check_root attempt ~issuer in
  if not (phys_equal state write.Remote.previous && phys_equal candidate write.candidate)
  then invalid "causal root writer has another original State or candidate"
  else (
    match attempt.phase with
    | Writer_prepared current when phys_equal current write -> Ok ()
    | Begun | Writer_prepared _ | Reserved _ | Not_reserved _ ->
      invalid "causal root writer is no longer the retained attempt")
;;

let reject_reserve_write (attempt : Remote.attempt) (write : Remote.write) =
  let open Result.Let_syntax in
  let%bind token = Session_commit_attempt.No_writer.seal write.Remote.writer in
  if
    Session_commit_attempt.No_writer.matches_reserve
      token
      ~attempt:write.writer
      ~reservation_attempt:attempt.Remote.identity
      ~set:attempt.set_identity
      ~previous:write.previous
      ~candidate:write.candidate
  then Ok (Remote.Not_reserved (Rejected_before_writer (write, token)))
  else invalid "causal reserve rejection has another exact kernel context"
;;

module Reserve_install = struct
  type t =
    { attempt : Remote.attempt
    ; write : Remote.write
    ; next : Remote.phase
    ; mutable state : Install_state.t
    }

  let prepare attempt ~issuer ~previous ~candidate =
    let open Result.Let_syntax in
    let%bind () = check_begun attempt ~issuer ~previous ~candidate in
    let%bind update = metadata candidate in
    let%bind () =
      match Book.Update.reason update with
      | Reserve request when phys_equal request attempt.Remote.request -> Ok ()
      | Initialize_goal _ | Reserve _ | Import _ | Close_before_writer _ ->
        invalid "causal Reserve metadata has another retained Request"
    in
    let%bind reserved =
      Book.reserve
        previous.Session_state.goal_policy_contributions
        ~owner:attempt.route.owner
        ~request:attempt.request
    in
    let%bind pending =
      match reserved with
      | Reserved (_, pending) -> Ok pending
      | Already_pending _ | Busy _ ->
        invalid "causal Reserve does not create this new exact Pending"
    in
    let%bind () = check_pending attempt pending in
    let%bind () = existing_pending attempt candidate.state pending in
    let write = reserve_writer attempt ~previous ~candidate ~kind:(Reserve pending) in
    let terminal = Remote.Settled Pending in
    let acknowledged =
      Remote.{ attempt; write; pending; installed = candidate.state; terminal }
    in
    let next = Remote.Reserved acknowledged in
    let plan = { attempt; write; next; state = Prepared } in
    let prepared = Remote.Writer_prepared write in
    let result = Ok plan in
    attempt.phase <- prepared;
    result
  ;;

  let write t = t.write

  let check_basis t ~issuer ~state ~candidate =
    let open Result.Let_syntax in
    let%bind () = check_root t.attempt ~issuer in
    match t.state with
    | Prepared -> check_reserve_write t.attempt t.write ~issuer ~state ~candidate
    | Installed | Rejected -> invalid "causal Reserve install plan is already spent"
  ;;

  let install t =
    (* Only the private State-ACK hook calls this after the final exact fence.
       Duplicate/displaced plans are harmless and cannot rewind a later phase. *)
    if on_domain t.attempt.basis.domain
    then (
      match t.state with
      | Prepared ->
        (match t.attempt.phase with
         | Writer_prepared current when phys_equal current t.write ->
           t.attempt.phase <- t.next;
           t.state <- Installed
         | Begun | Writer_prepared _ | Reserved _ | Not_reserved _ -> ())
      | Installed | Rejected -> ())
  ;;

  let reject_before_writer t =
    let open Result.Let_syntax in
    let%bind () = check_root t.attempt ~issuer:t.attempt.route.root_issuer in
    match t.state with
    | Rejected -> Ok ()
    | Installed -> invalid "acknowledged causal Reserve cannot be rejected"
    | Prepared ->
      let%bind () =
        check_reserve_write
          t.attempt
          t.write
          ~issuer:t.attempt.route.root_issuer
          ~state:t.write.previous
          ~candidate:t.write.candidate
      in
      let%map next = reject_reserve_write t.attempt t.write in
      t.attempt.phase <- next;
      t.state <- Rejected
  ;;
end

module Hold_install = struct
  type t =
    { attempt : Remote.attempt
    ; write : Remote.write
    ; next : Remote.phase
    ; mutable state : Install_state.t
    }

  let prepare attempt ~issuer ~previous ~candidate =
    let open Result.Let_syntax in
    let%bind () = check_begun attempt ~issuer ~previous ~candidate in
    let goal_id = C.Goal_target.goal_id attempt.Remote.route.target in
    let%bind catalog =
      match candidate.Session_transition.delta with
      | Goal_catalog_changed catalog -> Ok catalog
      | _ -> invalid "causal Hold requires one direct Goal catalog leaf"
    in
    let%bind reason =
      match Goal_catalog.find catalog goal_id with
      | Some goal ->
        (match P.Goal.state goal with
         | Open (Held reason) -> Ok reason
         | Open Enabled | Closed _ -> invalid "causal Hold does not hold its target Goal")
      | None -> invalid "causal Hold target Goal is absent"
    in
    let%bind expected = Goal_charge_admission.hold_delta previous ~goal_id ~reason in
    let%bind expected =
      match expected with
      | Some (Goal_catalog_changed catalog) -> Ok catalog
      | None | Some _ -> invalid "causal Hold has no new exact catalog transition"
    in
    let%bind expected_json = Goal_catalog.to_jsonaf expected in
    let%bind actual_json = Goal_catalog.to_jsonaf catalog in
    let%bind installed_catalog =
      Result.of_option
        candidate.state.goal_catalog
        ~error:(P.Error.invalid_request "causal Hold candidate lost its catalog")
    in
    let%bind installed_json = Goal_catalog.to_jsonaf installed_catalog in
    let%bind () =
      if
        Document_schema.Json.equal expected_json actual_json
        && Document_schema.Json.equal actual_json installed_json
        && Option.equal
             phys_equal
             previous.goal_policy_contributions
             candidate.state.goal_policy_contributions
        && phys_equal previous.inference_ledger candidate.state.inference_ledger
        && Option.equal
             phys_equal
             previous.policy_canonical_usage
             candidate.state.policy_canonical_usage
      then Ok ()
      else invalid "causal Hold changes another catalog or canonical accounting input"
    in
    let write = reserve_writer attempt ~previous ~candidate ~kind:(Policy_hold goal_id) in
    let next = Remote.Not_reserved (Policy_hold_acknowledged write) in
    let plan = { attempt; write; next; state = Prepared } in
    let prepared = Remote.Writer_prepared write in
    let result = Ok plan in
    attempt.phase <- prepared;
    result
  ;;

  let write t = t.write

  let check_basis t ~issuer ~state ~candidate =
    let open Result.Let_syntax in
    let%bind () = check_root t.attempt ~issuer in
    match t.state with
    | Prepared -> check_reserve_write t.attempt t.write ~issuer ~state ~candidate
    | Installed | Rejected -> invalid "causal Hold install plan is already spent"
  ;;

  let install t =
    if on_domain t.attempt.basis.domain
    then (
      match t.state with
      | Prepared ->
        (match t.attempt.phase with
         | Writer_prepared current when phys_equal current t.write ->
           t.attempt.phase <- t.next;
           t.state <- Installed
         | Begun | Writer_prepared _ | Reserved _ | Not_reserved _ -> ())
      | Installed | Rejected -> ())
  ;;

  let reject_before_writer t =
    let open Result.Let_syntax in
    let%bind () = check_root t.attempt ~issuer:t.attempt.route.root_issuer in
    match t.state with
    | Rejected -> Ok ()
    | Installed -> invalid "acknowledged causal Hold cannot be rejected"
    | Prepared ->
      let%bind () =
        check_reserve_write
          t.attempt
          t.write
          ~issuer:t.attempt.route.root_issuer
          ~state:t.write.previous
          ~candidate:t.write.candidate
      in
      let%map next = reject_reserve_write t.attempt t.write in
      t.attempt.phase <- next;
      t.state <- Rejected
  ;;
end

module Canonical_install = struct
  type t =
    { slot : Slot.t
    ; write : Canonical_write.t
    ; acknowledgement : Acknowledgement.t
    ; next : Slot.phase
    }
end

module Terminal_install = struct
  type t =
    { reservation : Remote.reservation
    ; write : Remote.terminal_write
    ; acknowledgement : Remote.terminal_ack option
    ; next : Remote.terminal
    ; restore : Remote.terminal
    ; mutable state : Install_state.t
    }

  let check_reservation (reservation : Remote.reservation) ~issuer =
    let open Result.Let_syntax in
    let attempt = reservation.attempt in
    let%bind () = check_root attempt ~issuer in
    let%bind () =
      match attempt.phase with
      | Reserved actual when phys_equal actual reservation -> Ok ()
      | Begun | Writer_prepared _ | Reserved _ | Not_reserved _ ->
        invalid "causal terminal operation lost its exact installed reservation"
    in
    match reservation.write.kind with
    | Reserve pending
      when phys_equal reservation.installed reservation.write.candidate.state
           && Book.Pending.equal pending reservation.pending ->
      check_pending attempt pending
    | Reserve _ | Policy_hold _ ->
      invalid "causal terminal operation has another reservation acknowledgement"
  ;;

  let check_import
        (reservation : Remote.reservation)
        ~previous
        ~candidate
        ~prior
        projection
    =
    let open Result.Let_syntax in
    let attempt = reservation.attempt in
    let%bind () =
      if
        PC.Stream.equal
          (Book.Request.stream attempt.request)
          (PC.Projection.stream projection)
        && Int64.equal
             (PC.Projection.admitted_attempts projection)
             (Book.Pending.next_prefix reservation.pending)
      then Ok ()
      else invalid "causal Import changed its reserved stream or prefix"
    in
    let%bind () =
      match prior with
      | Remote.Pending -> existing_pending attempt previous reservation.pending
      | Confirmed acknowledgement when phys_equal acknowledgement.reservation reservation
        -> Ok ()
      | Confirmed _ | No_writer_closed _ ->
        invalid "causal Import has another or closed prior acknowledgement"
    in
    let%bind previous_book = book previous in
    let%bind _, disposition = Book.import previous_book ~projection in
    let%bind () =
      match prior, disposition with
      | Remote.Pending, Book.Disposition.Added | Confirmed _, Replaced -> Ok ()
      | _ -> invalid "causal Import is not the new owned confirmation or correction"
    in
    let%bind candidate_book = book candidate.Session_transition.state in
    let%bind _, candidate_disposition = Book.import candidate_book ~projection in
    match candidate_disposition with
    | Book.Disposition.Duplicate -> Ok ()
    | Added | Replaced | Stale ->
      invalid "causal Import candidate does not retain the exact projection"
  ;;

  let equal_book_document t other =
    let open Result.Let_syntax in
    let%bind before = Book.to_document t in
    let%bind after = Book.to_document other in
    Ok
      (Document_schema.Json.equal
         (Document_schema.Document.json before)
         (Document_schema.Document.json after))
  ;;

  let check_correction
        (reservation : Remote.reservation)
        ~previous
        ~candidate
        ~prior
        projection
    =
    let open Result.Let_syntax in
    let%bind () =
      if
        PC.Stream.equal
          (Book.Request.stream reservation.attempt.request)
          (PC.Projection.stream projection)
        && Int64.( >= )
             (PC.Projection.admitted_attempts projection)
             (Book.Pending.next_prefix reservation.pending)
      then Ok ()
      else
        invalid "causal Correction changed its owned stream or precedes its reservation"
    in
    let%bind () =
      match prior with
      | Remote.Confirmed acknowledgement
        when phys_equal acknowledgement.reservation reservation -> Ok ()
      | Pending | Confirmed _ | No_writer_closed _ ->
        invalid "causal Correction requires the same already confirmed reservation"
    in
    let%bind previous_book = book previous in
    let%bind imported, disposition = Book.import previous_book ~projection in
    let imported_book = Book.Update.candidate imported in
    let%bind () =
      match disposition with
      | Book.Disposition.Replaced -> Ok ()
      | Duplicate ->
        let%bind unchanged = equal_book_document previous_book imported_book in
        if unchanged
        then invalid "exact duplicate causal usage does not own a Correction writer"
        else Ok ()
      | Added | Stale ->
        invalid "causal Correction cannot consume Pending or import superseded usage"
    in
    let%bind candidate_book = book candidate.Session_transition.state in
    let%bind same_candidate = equal_book_document candidate_book imported_book in
    let%bind () =
      if same_candidate
      then Ok ()
      else invalid "causal Correction candidate differs from its complete Book import"
    in
    let%bind repeated, repeated_disposition = Book.import candidate_book ~projection in
    match repeated_disposition with
    | Book.Disposition.Duplicate ->
      let%bind unchanged =
        equal_book_document candidate_book (Book.Update.candidate repeated)
      in
      if unchanged
      then Ok ()
      else invalid "causal Correction candidate is not a complete import fixed point"
    | Added | Replaced | Stale ->
      invalid "causal Correction candidate does not retain the exact projection"
  ;;

  let check_close (reservation : Remote.reservation) ~previous ~candidate ~prior closed =
    let open Result.Let_syntax in
    let%bind () =
      match prior with
      | Remote.Pending
        when phys_equal closed.Remote.preparation reservation.attempt.route.preparation ->
        Ok ()
      | Pending | Confirmed _ | No_writer_closed _ ->
        invalid "causal Close does not own the same pending callee seal"
    in
    let%bind () = existing_pending reservation.attempt previous reservation.pending in
    let%bind previous_book = book previous in
    let%bind _ = Book.close_before_writer previous_book ~pending:reservation.pending in
    let%bind candidate_book = book candidate.Session_transition.state in
    let%bind repeated =
      Book.close_before_writer candidate_book ~pending:reservation.pending
    in
    let%bind before = Book.to_document candidate_book in
    let%bind after = Book.to_document (Book.Update.candidate repeated) in
    if
      Document_schema.Json.equal
        (Document_schema.Document.json before)
        (Document_schema.Document.json after)
    then Ok ()
    else invalid "causal Close candidate still retains an open Pending"
  ;;

  let prepare reservation ~issuer ~previous ~candidate ~kind =
    let open Result.Let_syntax in
    let%bind () = check_reservation reservation ~issuer in
    let%bind () = check_state reservation.attempt previous in
    let%bind () = check_state reservation.attempt candidate.Session_transition.state in
    let%bind prior =
      match reservation.terminal with
      | Remote.Settled (Pending as prior) | Settled (Confirmed _ as prior) -> Ok prior
      | Settled (No_writer_closed _) | Terminal_prepared _ ->
        invalid "causal terminal operation is closed or already owns a writer"
    in
    let%bind update = metadata candidate in
    let%bind () =
      match kind, Book.Update.reason update with
      | Remote.Import projection, Book.Update.Reason.Import actual
        when phys_equal projection actual ->
        check_import reservation ~previous ~candidate ~prior projection
      | Correction projection, Import actual when phys_equal projection actual ->
        check_correction reservation ~previous ~candidate ~prior projection
      | Close closed, Close_before_writer pending
        when phys_equal pending reservation.pending ->
        check_close reservation ~previous ~candidate ~prior closed
      | _ -> invalid "causal terminal metadata has another exact operation witness"
    in
    let identity = ref () in
    let writer =
      Session_commit_attempt.create
        ~context:
          (Causal_terminal
             { operation = identity; reservation = reservation.attempt.identity })
        ~previous
        ~candidate
    in
    let operation = Terminal_operation.{ identity; previous; candidate; writer } in
    let write = Remote.{ operation; reservation; prior; kind; acknowledged = None } in
    let acknowledgement, next =
      match kind with
      | Remote.Import projection | Correction projection ->
        let acknowledged =
          Remote.{ reservation; operation; installed = candidate.state; projection }
        in
        Some (Remote.Imported acknowledged), Remote.Settled (Confirmed acknowledged)
      | Close closed ->
        let acknowledged =
          Remote.{ reservation; previous; candidate; installed = candidate.state; closed }
        in
        ( Some (Remote.Close_installed acknowledged)
        , Remote.Settled (No_writer_closed acknowledged) )
    in
    let restore = Remote.Settled prior in
    let plan = { reservation; write; acknowledgement; next; restore; state = Prepared } in
    let prepared = Remote.Terminal_prepared write in
    let result = Ok plan in
    reservation.terminal <- prepared;
    result
  ;;

  let write t = t.write

  let check_basis t ~issuer ~state ~candidate =
    let open Result.Let_syntax in
    let%bind () = check_reservation t.reservation ~issuer in
    match t.state with
    | Installed | Rejected -> invalid "causal terminal install plan is already spent"
    | Prepared ->
      if
        not
          (phys_equal state t.write.operation.previous
           && phys_equal candidate t.write.operation.candidate)
      then invalid "causal terminal writer has another original State or candidate"
      else (
        match t.reservation.terminal with
        | Terminal_prepared actual when phys_equal actual t.write -> Ok ()
        | Settled _ | Terminal_prepared _ ->
          invalid "causal terminal writer is no longer the retained operation")
  ;;

  let install t =
    (* Only the exact private State-ACK hook may spend this plan. All assigned
       options and phases were allocated before Terminal_prepared publication. *)
    if on_domain t.reservation.attempt.basis.domain
    then (
      match t.state with
      | Prepared ->
        (match t.reservation.attempt.phase with
         | Reserved actual when phys_equal actual t.reservation ->
           (match t.reservation.terminal with
            | Terminal_prepared write when phys_equal write t.write ->
              t.write.acknowledged <- t.acknowledgement;
              t.reservation.terminal <- t.next;
              t.state <- Installed
            | Settled _ | Terminal_prepared _ -> ())
         | Begun | Writer_prepared _ | Reserved _ | Not_reserved _ -> ())
      | Installed | Rejected -> ())
  ;;

  let reject_before_writer t =
    let open Result.Let_syntax in
    let%bind () =
      check_root t.reservation.attempt ~issuer:t.reservation.attempt.route.root_issuer
    in
    match t.state with
    | Rejected -> Ok ()
    | Installed -> invalid "acknowledged causal terminal operation cannot be rejected"
    | Prepared ->
      let%bind () =
        check_basis
          t
          ~issuer:t.reservation.attempt.route.root_issuer
          ~state:t.write.operation.previous
          ~candidate:t.write.operation.candidate
      in
      let%bind token = Session_commit_attempt.No_writer.seal t.write.operation.writer in
      if
        Session_commit_attempt.No_writer.matches_terminal
          token
          ~attempt:t.write.operation.writer
          ~operation:t.write.operation.identity
          ~reservation:t.reservation.attempt.identity
          ~previous:t.write.operation.previous
          ~candidate:t.write.operation.candidate
      then (
        t.reservation.terminal <- t.restore;
        t.state <- Rejected;
        Ok ())
      else invalid "causal terminal rejection has another exact kernel context"
  ;;
end

module Root_terminal_write = struct
  (* Stored as SAME reservation.terminal=Terminal_prepared before invocation.
     Unknown cannot be overwritten by another factual request. Prior settled ACK
     remains retained inside the write. Later actual usage corrections use the
     existing factual observation/import path, never a new provider/Handle. *)
  type t = Remote.terminal_write
end

module Confirmation = struct
  type t =
    { canonical : Acknowledgement.t
    ; imports : Remote.import_ack list
    }
end
