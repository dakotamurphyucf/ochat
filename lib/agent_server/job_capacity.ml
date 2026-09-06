open! Core

module Key = struct
  type t =
    { principal : string
    ; prompt : string
    ; workspace_conflict_domain : string
    ; session : string
    ; kind : Agent_protocol.Job.kind
    ; nested_depth : int
    }

  let create
        ~principal_id
        ~prompt
        ~workspace_conflict_domain
        ~session_id
        ~kind
        ~nested_depth
    =
    { principal =
        Option.value_map
          principal_id
          ~default:"<anonymous>"
          ~f:Agent_protocol.Id.Principal.to_string
    ; prompt
    ; workspace_conflict_domain
    ; session = Agent_protocol.Id.Session.to_string session_id
    ; kind
    ; nested_depth
    }
  ;;
end

type counts =
  { mutable global : int
  ; mutable principals : (string, int, String.comparator_witness) Map.t
  ; mutable prompts : (string, int, String.comparator_witness) Map.t
  ; mutable workspaces : (string, int, String.comparator_witness) Map.t
  ; mutable sessions : (string, int, String.comparator_witness) Map.t
  ; mutable kinds : (Agent_protocol.Job.kind, int) Map.Poly.t
  }

type t =
  { limits : Config.Server.job_limits
  ; mutex : Eio.Mutex.t
  ; counts : counts
  }

type lease =
  { owner : t
  ; key : Key.t
  ; mutable released : bool
  }

let create ~limits =
  { limits
  ; mutex = Eio.Mutex.create ()
  ; counts =
      { global = 0
      ; principals = String.Map.empty
      ; prompts = String.Map.empty
      ; workspaces = String.Map.empty
      ; sessions = String.Map.empty
      ; kinds = Map.Poly.empty
      }
  }
;;

let count map key = Map.find map key |> Option.value ~default:0

let increment map key =
  Map.update map key ~f:(function
    | None -> 1
    | Some value -> value + 1)
;;

let decrement map key =
  match Map.find map key with
  | None | Some 0 | Some 1 -> Map.remove map key
  | Some value -> Map.set map ~key ~data:(value - 1)
;;

let saturated t key =
  let limits = t.limits in
  let counts = t.counts in
  counts.global >= limits.daemon_total
  || count counts.principals key.Key.principal >= limits.per_principal
  || count counts.prompts key.prompt >= limits.per_prompt
  || count counts.workspaces key.workspace_conflict_domain >= limits.per_workspace
  || count counts.sessions key.session >= limits.per_session
  || count counts.kinds key.kind >= limits.per_kind
;;

let reserve t key =
  let counts = t.counts in
  counts.global <- counts.global + 1;
  counts.principals <- increment counts.principals key.Key.principal;
  counts.prompts <- increment counts.prompts key.prompt;
  counts.workspaces <- increment counts.workspaces key.workspace_conflict_domain;
  counts.sessions <- increment counts.sessions key.session;
  counts.kinds <- increment counts.kinds key.kind
;;

let depth_error maximum actual =
  Agent_protocol.Error.create
    Resource_limit
    ~message:(sprintf "job nesting depth %d exceeds configured maximum %d" actual maximum)
    ~retryable:false
    ()
;;

let try_acquire t key =
  if key.Key.nested_depth < 0 || key.nested_depth > t.limits.max_nested_depth
  then Error (depth_error t.limits.max_nested_depth key.nested_depth)
  else
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if saturated t key
      then Ok None
      else (
        reserve t key;
        Ok (Some { owner = t; key; released = false })))
;;

let release lease =
  Eio.Mutex.use_rw ~protect:true lease.owner.mutex (fun () ->
    if not lease.released
    then (
      let counts = lease.owner.counts in
      counts.global <- Int.max 0 (counts.global - 1);
      counts.principals <- decrement counts.principals lease.key.Key.principal;
      counts.prompts <- decrement counts.prompts lease.key.prompt;
      counts.workspaces <- decrement counts.workspaces lease.key.workspace_conflict_domain;
      counts.sessions <- decrement counts.sessions lease.key.session;
      counts.kinds <- decrement counts.kinds lease.key.kind;
      lease.released <- true))
;;
