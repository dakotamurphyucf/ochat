open! Core

type t =
  { persist :
      principal:Agent_protocol.Principal.t
      -> host_id:Agent_protocol.Id.Server.t
      -> additions:Agent_protocol.Session_organization.Values.t
      -> commit:(unit -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result
  }

let create persist = { persist }

let persist t ~principal ~host_id ~additions ~commit =
  t.persist ~principal ~host_id ~additions ~commit
;;
