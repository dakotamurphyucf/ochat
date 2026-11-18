open! Core
module A = Agent_session
module B = A.Runtime_builder
module P = Agent_protocol
module D = Agent_store.Delegation_store
module Governance = A.Session_actor.Causal_governance
module Resource = Runtime_owner.Delegated_policy_resource
module Scope = Runtime_owner.Scope_outcome

type host =
  { registry : Session_registry.t
  ; runtime_source_owner : A.Runtime_source_owner.t
  ; native_metadata_target :
      P.Id.Session.t -> (Session_registry.entry * A.Session_state.t, P.Error.t) result
  }

let unavailable code message = P.Error.create code ~message ~retryable:false ()
let reject message = Error (unavailable Permission_denied message)

module Member = struct
  module Policy = struct
    type t =
      | Absent
      | Required of P.Invocation.observer
  end

  type t =
    { id : P.Id.Session.t
    ; target : Session_registry.entry
    ; source : B.Source.t
    ; policy : Policy.t
    ; check_target : unit -> (unit, P.Error.t) result
    ; check_current :
        authorize:(unit -> (unit, P.Error.t) result) -> (unit, P.Error.t) result
    }

  let check_target (t : t) = t.check_target ()
  let check_current (t : t) ~authorize = t.check_current ~authorize

  let with_member host ~sources ~authorize (record : D.record) ~f =
    let open Result.Let_syntax in
    let%bind () = authorize () in
    let%bind target, _ = host.native_metadata_target record.key.parent_session_id in
    let%bind () = authorize () in
    sources.B.with_source record.key.parent_session_id ~f:(fun ~source ->
      (* Metadata lookup may have waited while the canonical entry changed.
         Bind THIS captured target to the Source view's current provider; an
         equal or shared Source alone cannot establish Owner affinity. *)
      let check_target () =
        match Session_registry.find host.registry record.key.parent_session_id with
        | Some current
          when phys_equal current.actor target.actor
               && phys_equal current.runtime target.runtime
               && Option.equal phys_equal current.store_handle target.store_handle
               && Runtime_owner.belongs_to_actor target.runtime target.actor -> Ok ()
        | Some _ | None ->
          reject "delegation.resources_unavailable: ancestor target changed"
      in
      let check_source ~authorize actual =
        let%bind () =
          if phys_equal actual source
          then Ok ()
          else reject "delegation.resources_unavailable: ancestor Source changed"
        in
        let%bind () = check_target () in
        let%bind () = authorize () in
        let%bind () = check_target () in
        let%bind expected = A.Session_actor.state target.actor in
        let%bind () = check_target () in
        let%bind () = authorize () in
        let%bind () = check_target () in
        let%bind () =
          if
            P.Id.Session.equal expected.identity.session_id record.key.parent_session_id
            && Int.equal expected.identity.generation record.key.parent_generation
          then Ok ()
          else reject "delegation.resources_unavailable: ancestor Source owner changed"
        in
        B.Source.validate_target actual expected
      in
      let check_current ~authorize =
        sources.with_source record.key.parent_session_id ~f:(fun ~source ->
          check_source ~authorize source)
      in
      let%bind () = check_source ~authorize source in
      let%bind policy =
        match B.Source.moderator source with
        | No_moderator -> Ok Policy.Absent
        | Extension observer -> Ok (Policy.Required observer)
        | Legacy _ ->
          reject
            "delegation.owner_mediation_unavailable: legacy ancestor policy is \
             unsupported"
      in
      let result =
        f
          { id = record.key.parent_session_id
          ; target
          ; source
          ; policy
          ; check_target
          ; check_current
          }
      in
      (* No cleanup belongs to this view. Preserve the actual callback failure;
         only success can be superseded by factual target displacement. *)
      match result with
      | Error _ -> result
      | Ok value ->
        let%map () = check_target () in
        value)
  ;;
end

(* Only actual captured scope reports enter this sink. None means no inner
   report, never a fabricated successful join. Each nesting level has one sink,
   so its complete child report is prepended exactly once. *)
module Reports = struct
  type t = Scope.Cleanup.t option ref

  let create () : t = ref None

  let project (t : t) report =
    t
    := Some
         (match !t with
          | None -> Scope.cleanup report
          | Some previous -> Scope.Cleanup.combine previous (Scope.cleanup report));
    match Scope.primary report with
    | Returned result -> result
    | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace
  ;;

  let prepend (t : t) report =
    match !t with
    | None -> report
    | Some cleanup -> Scope.prepend_cleanup report ~cleanup
  ;;
end

type validation =
  { authorize : unit -> (unit, P.Error.t) result
  ; read_ancestry : unit -> (A.Delegation_authority.Checked_ancestry.t, P.Error.t) result
  }

type membership =
  { host : host
  ; references : D.Reference.t list
  ; members : Member.t list
  ; validation : validation
  }

module Pinned = struct
  type t =
    | Absent of Member.t
    | Required of
        { member : Member.t
        ; resource : Resource.t
        }

  let check_current ~authorize = function
    | Absent member -> Member.check_current member ~authorize
    | Required { member; resource } ->
      let open Result.Let_syntax in
      let%bind () = Member.check_current member ~authorize in
      Resource.check_source resource ~source:member.source
  ;;
end

type prepared =
  { membership : membership
  ; actor : A.Session_actor.t
  ; request : A.Script_tool_calls.preparation
  ; preflight : Governance.t
  ; pinned : Pinned.t list
  ; reports : Reports.t
  }

type bound =
  { prepared : prepared
  ; governance : Governance.t
  }

let check_ancestry (membership : membership) ~read_ancestry =
  let open Result.Let_syntax in
  let%bind ancestry = read_ancestry () in
  let references =
    List.map (A.Delegation_authority.Checked_ancestry.records ancestry) ~f:D.reference
  in
  if
    A.Delegation_authority.Checked_ancestry.issued_by
      ancestry
      ~source_owner:membership.host.runtime_source_owner
    && List.equal D.Reference.equal membership.references references
  then Ok ancestry
  else reject "delegation.ancestry_changed: policy vector membership changed"
;;

(* Final callback-free factual cut after all source/Actor/authority waits.
   This checks only captured physical affinity, not execution permission. *)
let check_targets (membership : membership) =
  List.fold_result membership.members ~init:() ~f:(fun () member ->
    Member.check_target member)
;;

let check_members ?validation (membership : membership) =
  let open Result.Let_syntax in
  let { authorize; read_ancestry } =
    Option.value validation ~default:membership.validation
  in
  let%bind () = authorize () in
  let%bind _ = check_ancestry membership ~read_ancestry in
  let%bind () =
    List.fold_result membership.members ~init:() ~f:(fun () member ->
      Member.check_current member ~authorize)
  in
  let%bind ancestry = check_ancestry membership ~read_ancestry in
  let%bind () = authorize () in
  let%map () = check_targets membership in
  ancestry
;;

let with_members host ~ancestry ~sources ~(validation : validation) ~f =
  let open Result.Let_syntax in
  let%bind () =
    if
      A.Delegation_authority.Checked_ancestry.issued_by
        ancestry
        ~source_owner:host.runtime_source_owner
    then Ok ()
    else reject "delegation.ancestry_changed: policy vector has a foreign issuer"
  in
  let records = A.Delegation_authority.Checked_ancestry.records ancestry in
  let%bind () =
    if List.is_empty records
    then reject "delegation.ancestry_changed: policy vector has no private edge"
    else Ok ()
  in
  let rec enter reversed = function
    | [] ->
      let membership =
        { host
        ; references = List.map records ~f:D.reference
        ; members = List.rev reversed
        ; validation
        }
      in
      let%bind _ = check_members membership in
      f membership
    | record :: rest ->
      Member.with_member
        host
        ~sources
        ~authorize:validation.authorize
        record
        ~f:(fun member -> enter (member :: reversed) rest)
  in
  enter [] records
;;

let with_resources
      (membership : membership)
      ~actor
      ~request
      ~governance
      ~authorize
      ~authorize_restoration
      ~f
  =
  let open Result.Let_syntax in
  let make_prepared pinned reports =
    { membership; actor; request; preflight = governance; pinned; reports }
  in
  let enter member ~f =
    Runtime_owner.with_delegated_policy_resource_reported
      member.Member.target.runtime
      ~request
      ~governance
      ~authorize
      ~authorize_restoration:(fun () -> authorize_restoration member.Member.id)
      ~f:(fun resource ->
        let%bind () = Resource.check_source resource ~source:member.source in
        let%bind () = authorize () in
        f resource)
  in
  let rec inner remaining reversed reports =
    match remaining with
    | [] -> f (make_prepared reversed reports)
    | member :: rest ->
      let%bind () =
        Member.check_current member ~authorize:membership.validation.authorize
      in
      (match member.policy with
       | Absent -> inner rest (Pinned.Absent member :: reversed) reports
       | Required _ ->
         let nested = Reports.create () in
         let report =
           enter member ~f:(fun resource ->
             inner rest (Pinned.Required { member; resource } :: reversed) nested)
         in
         Reports.project reports (Reports.prepend nested report))
  in
  let rec outer remaining reversed =
    match remaining with
    | [] ->
      (* No Required scope exists: no report or successful cleanup is minted. *)
      f (make_prepared reversed (Reports.create ()))
    | member :: rest ->
      let%bind () =
        Member.check_current member ~authorize:membership.validation.authorize
      in
      (match member.policy with
       | Absent -> outer rest (Pinned.Absent member :: reversed)
       | Required _ ->
         let nested = Reports.create () in
         let report =
           enter member ~f:(fun resource ->
             inner rest (Pinned.Required { member; resource } :: reversed) nested)
         in
         Scope.resume (Reports.prepend nested report))
  in
  outer (List.rev membership.members) []
;;

let check_prepared (prepared : prepared) =
  let open Result.Let_syntax in
  let%bind _ = check_members prepared.membership in
  let authorize = prepared.membership.validation.authorize in
  let%bind () =
    List.fold_result prepared.pinned ~init:() ~f:(fun () pinned ->
      Pinned.check_current pinned ~authorize)
  in
  let%bind () = authorize () in
  check_targets prepared.membership
;;

let bind_current (prepared : prepared) =
  let open Result.Let_syntax in
  let%bind () = check_prepared prepared in
  let%bind ancestry = check_members prepared.membership in
  (* Capture and bind SAME request immediately. No awaited check can stale the
     captured Actor State pointer between these two existing operations. *)
  let%bind captured = Governance.capture prepared.actor ~request:prepared.request in
  let%bind governance =
    Governance.bind_registered
      prepared.actor
      captured
      ~source_owner:prepared.membership.host.runtime_source_owner
      ~ancestry:(Some ancestry)
  in
  let%bind () = Governance.check_current prepared.preflight in
  let%map () = Governance.check_current governance in
  { prepared; governance }
;;

let check_bound (bound : bound) =
  let open Result.Let_syntax in
  let%bind () = check_prepared bound.prepared in
  (* Both compare against the SAME physical request.caller's frozen DATA.
     Keep these nonyielding checks adjacent after all original/member waits.
     The resource still owns preflight governance; every effect also requires
     final governance through evaluate_reported's mandatory authorizer. *)
  let%bind () = Governance.check_current bound.prepared.preflight in
  Governance.check_current bound.governance
;;

let evaluate (bound : bound) ~(reference : D.Reference.t) =
  let open Result.Let_syntax in
  let prepared = bound.prepared in
  let request = prepared.request in
  let delegation : P.Moderator_execution.delegation =
    { child_session_id = request.session_id
    ; child_generation = request.generation
    ; child_invocation_id = request.invocation_id
    ; admission_sha256 = reference.admission_sha256
    }
  in
  let rec loop call = function
    | [] ->
      let%map () = check_bound bound in
      Delegated_policy_call.finish call ~original:request.call
    | selected :: rest ->
      let%bind () = check_bound bound in
      let%bind step =
        match selected with
        | Pinned.Absent _ ->
          let%map call = Delegated_policy_call.validate call ~request in
          Delegated_policy_call.Step.Continue call
        | Required { member = _; resource } ->
          let report =
            Resource.evaluate_reported
              resource
              ~delegation
              ~event:(Pre_tool_call call)
              ~authorize:(fun () -> check_bound bound)
          in
          let%bind receipt = Reports.project prepared.reports report in
          let%bind () = check_bound bound in
          let%bind decision =
            Result.of_option
              receipt.P.Moderator_execution.decision
              ~error:
                (unavailable
                   Invalid_state
                   "delegation.owner_mediation_unavailable: parent policy runtime is \
                    unavailable")
          in
          Delegated_policy_call.apply call ~request ~decision
      in
      let%bind () = check_bound bound in
      (match step with
       | Rejected ->
         Ok
           (Some
              (Chat_response.Moderation.Tool_moderation.Reject
                 "Parent policy rejected the call."))
       | Continue call -> loop call rest)
  in
  loop request.call prepared.pinned
;;
