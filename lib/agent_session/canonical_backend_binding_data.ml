open! Core

type commit =
  ?observation:Session_commit_observation.t
  -> command_audit:Document_schema.Document.t option
  -> previous:Session_state.t
  -> Session_transition.t
  -> (unit, Agent_protocol.Error.t) result

module Recovery_admission = struct
  type t =
    | Fits
    | Capacity_exhausted
end

type t =
  { domain : int
  ; writer : Agent_store.Commit_writer.t
  ; commit : commit
  ; recovery_note_owner : Agent_protocol.Session_ref.t option
  ; local_recovery_host : Operator_authorization.Local_recovery_host.t option
  ; check_recovery_admission :
      command_audit:Document_schema.Document.t option
      -> previous:Session_state.t
      -> Session_transition.t
      -> (Recovery_admission.t, Agent_protocol.Error.t) result
  }

let check_domain_exn t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "canonical persistence binding belongs to another domain"
;;

let create
      ~writer
      ~commit
      ~recovery_note_owner
      ~local_recovery_host
      ~check_recovery_admission
  =
  { domain = (Stdlib.Domain.self () :> int)
  ; writer
  ; commit
  ; recovery_note_owner
  ; local_recovery_host
  ; check_recovery_admission
  }
;;

let check_recovery_admission t ~command_audit ~previous candidate =
  check_domain_exn t;
  t.check_recovery_admission ~command_audit ~previous candidate
;;

let session_owner t =
  check_domain_exn t;
  t.recovery_note_owner
;;

let recovery_note_owner = session_owner

type recovery_window_host =
  | Daemon
  | Embedded of { local_owner_authenticated : bool }

let recovery_window_host t ~actor =
  check_domain_exn t;
  match t.local_recovery_host with
  | None -> Daemon
  | Some Transient -> Embedded { local_owner_authenticated = false }
  | Some (Durable host) ->
    let local_owner_authenticated =
      Agent_store.Commit_writer.is_available t.writer
      && Option.exists t.recovery_note_owner ~f:(fun owner ->
        Operator_authorization.current_durable_local
          actor
          ~host
          ~server_id:(Agent_protocol.Session_ref.server_id owner))
    in
    Embedded { local_owner_authenticated }
;;

let validate_exn t ~session_id ~commit =
  check_domain_exn t;
  if not (phys_equal t.commit commit)
  then invalid_arg "canonical persistence binding has another physical commit closure";
  if
    not
      (Agent_protocol.Id.Session.equal
         session_id
         (Agent_store.Commit_writer.session_id t.writer))
  then invalid_arg "canonical persistence binding has another session writer";
  Option.iter t.recovery_note_owner ~f:(fun owner ->
    if
      not
        (Agent_protocol.Id.Session.equal
           (Agent_protocol.Session_ref.session_id owner)
           session_id)
    then invalid_arg "canonical persistence has another recovery note owner")
;;

let requires_recovery t =
  check_domain_exn t;
  Agent_store.Commit_writer.requires_recovery t.writer
;;

let is_available t =
  check_domain_exn t;
  Agent_store.Commit_writer.is_available t.writer
;;
