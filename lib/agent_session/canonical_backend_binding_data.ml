open! Core

type commit =
  ?observation:Session_commit_observation.t
  -> command_audit:Document_schema.Document.t option
  -> previous:Session_state.t
  -> Session_transition.t
  -> (unit, Agent_protocol.Error.t) result

type t =
  { domain : int
  ; writer : Agent_store.Commit_writer.t
  ; commit : commit
  }

let check_domain_exn t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "canonical persistence binding belongs to another domain"
;;

let create ~writer ~commit = { domain = (Stdlib.Domain.self () :> int); writer; commit }

let validate_exn t ~session_id ~commit =
  check_domain_exn t;
  if not (phys_equal t.commit commit)
  then invalid_arg "canonical persistence binding has another physical commit closure";
  if
    not
      (Agent_protocol.Id.Session.equal
         session_id
         (Agent_store.Commit_writer.session_id t.writer))
  then invalid_arg "canonical persistence binding has another session writer"
;;

let requires_recovery t =
  check_domain_exn t;
  Agent_store.Commit_writer.requires_recovery t.writer
;;
