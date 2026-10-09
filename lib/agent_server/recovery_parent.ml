open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module D = Agent_store.Delegation_store
module State = Agent_session.Session_state

module Session_key = struct
  module T = struct
    type t = P.Id.Session.t [@@deriving sexp_of]

    let compare = P.Id.Session.compare
  end

  include T
  include Comparator.Make (T)
end

module Disposition = struct
  type t =
    | Retired
    | Retained of { stop_epoch : int64 }
end

type t =
  { store : S.t
  ; retained : Retained_session.t
  ; max_depth : int
  ; authorize_independent : D.record -> (unit, P.Error.t) result
  }

let create ~store ~registry ~max_depth ~read_owned ~authorize_independent =
  if max_depth <= 0
  then Error (P.Error.invalid_request "retained ancestry requires a positive depth limit")
  else
    Ok
      { store
      ; retained = Retained_session.create ~store ~registry ~read_owned
      ; max_depth
      ; authorize_independent
      }
;;

let inspect t ~(reference : D.Reference.t) ~expected_stop_epoch =
  let open Result.Let_syntax in
  let resolve reference =
    D.resolve (S.delegations t.store) reference
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let rec inspect depth visited reference expected =
    let%bind record = resolve reference in
    let parent_id = record.key.parent_session_id in
    if depth >= t.max_depth || Set.mem visited parent_id
    then
      Error
        (P.Error.invalid_request "retained ancestry is cyclic or exceeds its depth limit")
    else if Option.is_some record.revocation || not (D.equal_stage record.stage Linked)
    then Ok Disposition.Retired
    else (
      let observed =
        Retained_session.with_state
          t.retained
          ~session_id:parent_id
          ~authorize:(fun _ -> Ok ())
          ~f:(fun handle parent ->
            let%bind lifecycle =
              S.read_lifecycle t.store handle
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            in
            let lifecycle = S.Lifecycle.Observation.value lifecycle in
            let admitted =
              match record.admission.lifetime with
              | Owned | Invocation_owned _ ->
                Agent_store.Session_archive_record.Status.equal
                  (Agent_store.Session_archive_record.status lifecycle)
                  Active
                && Agent_store.Session_archive_record.Admission.equal
                     (Agent_store.Session_archive_record.admission lifecycle)
                     Automatic
              | Independent _ ->
                (match Agent_store.Session_archive_record.status lifecycle with
                 | Active | Archived -> true
                 | Removed -> false)
            in
            let%bind moderator =
              Agent_session.Moderator_checkpoint.observer parent.State.moderator
            in
            let fingerprint =
              Agent_session.Delegation_authority.fingerprint ?moderator parent
            in
            let matches =
              Int.equal parent.identity.generation record.key.parent_generation
              && P.Id.Prompt_revision.equal
                   parent.spec.prompt_revision_id
                   record.admission.parent_revision_id
              && (match fingerprint with
                  | Ok current -> String.equal current record.admission.authority_sha256
                  | Error _ -> false)
              && Result.is_ok
                   (Agent_session.Delegation_authority.check_invocation_owner
                      record
                      parent)
              && admitted
            in
            if not matches
            then Ok Disposition.Retired
            else (
              let%bind allowed =
                match record.admission.lifetime with
                | Independent _ ->
                  let%map () = t.authorize_independent record in
                  true
                | Owned | Invocation_owned _ ->
                  Ok
                    (Int64.equal parent.stop_epoch expected
                     && P.Session.equal_desired_state parent.lifecycle.desired Running
                     && (not parent.halted)
                     && Option.is_none parent.failure)
              in
              if not allowed
              then Ok Disposition.Retired
              else (
                let%bind ancestry =
                  match record.admission.lifetime, parent.spec.delegation with
                  | Independent _, _ | (Owned | Invocation_owned _), None ->
                    Ok (Disposition.Retained { stop_epoch = parent.stop_epoch })
                  | (Owned | Invocation_owned _), Some ancestor ->
                    inspect
                      (depth + 1)
                      (Set.add visited parent_id)
                      ancestor
                      (Option.value parent.parent_stop_epoch ~default:0L)
                in
                let%bind metadata =
                  S.Handle.metadata_checked handle
                  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                in
                let%bind () =
                  (* Every canonical state transition advances this revision.
                     Generation plus revision closes the observed authority basis;
                     the retained reservation excludes lifecycle/owner replacement. *)
                  if
                    P.Id.Session.equal metadata.session.id parent.identity.session_id
                    && Int.equal metadata.session.generation parent.identity.generation
                    && Int64.equal metadata.session.revision parent.counters.revision
                  then Ok ()
                  else
                    Error
                      (P.Error.create
                         Conflict
                         ~message:"retained parent changed during ancestry observation"
                         ~retryable:true
                         ())
                in
                let%map current = resolve reference in
                if
                  (not (D.Key.equal current.key record.key))
                  || Option.is_some current.revocation
                  || not (D.equal_stage current.stage Linked)
                then Disposition.Retired
                else (
                  match ancestry with
                  | Disposition.Retired -> Retired
                  | Retained _ -> Retained { stop_epoch = parent.stop_epoch }))))
      in
      match observed with
      | Error { P.Error.code = Session_not_found; _ } -> Ok Disposition.Retired
      | Error _ | Ok _ -> observed)
  in
  inspect
    0
    (Set.singleton (module Session_key) reference.child_session_id)
    reference
    expected_stop_epoch
;;
