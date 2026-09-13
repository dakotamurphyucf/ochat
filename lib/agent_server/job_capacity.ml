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
  [@@deriving equal]

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
  ; mutable reservations : (Agent_protocol.Id.Job.t, reservation) Map.Poly.t
  }

and lease =
  { owner : t
  ; key : Key.t
  ; mutable released : bool
  ; reservation_id : Agent_protocol.Id.Job.t option
  }

and reservation =
  { lease : lease
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; mutable phase : reservation_phase
  }

and reservation_phase =
  | Staged
  | Ready
  | Claimed

let create ~limits =
  { limits
  ; mutex = Eio.Mutex.create ()
  ; reservations = Map.Poly.empty
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
        Ok (Some { owner = t; key; released = false; reservation_id = None })))
;;

let release_locked lease =
  if not lease.released
  then (
    let counts = lease.owner.counts in
    counts.global <- Int.max 0 (counts.global - 1);
    counts.principals <- decrement counts.principals lease.key.Key.principal;
    counts.prompts <- decrement counts.prompts lease.key.prompt;
    counts.workspaces <- decrement counts.workspaces lease.key.workspace_conflict_domain;
    counts.sessions <- decrement counts.sessions lease.key.session;
    counts.kinds <- decrement counts.kinds lease.key.kind;
    lease.released <- true;
    Option.iter lease.reservation_id ~f:(fun id ->
      match Map.find lease.owner.reservations id with
      | Some reservation when phys_equal reservation.lease lease ->
        lease.owner.reservations <- Map.remove lease.owner.reservations id
      | _ -> ()))
;;

let release lease =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true lease.owner.mutex (fun () -> release_locked lease))
;;

let valid_key (key : Key.t) (job : Agent_protocol.Job.t) =
  String.equal key.session (Agent_protocol.Id.Session.to_string job.session_id)
  && Agent_protocol.Job.equal_kind key.kind job.kind
  && Option.for_all job.launch ~f:(fun launch ->
    Int.equal key.nested_depth launch.nested_depth)
;;

let reserve_job t key ~(job : Agent_protocol.Job.t) =
  if not (valid_key key job)
  then
    Error
      (Agent_protocol.Error.invalid_request "job capacity owner differs from reservation")
  else if key.nested_depth < 0 || key.nested_depth > t.limits.max_nested_depth
  then Error (depth_error t.limits.max_nested_depth key.nested_depth)
  else (
    match job.status, job.attempt with
    | Queued, 0 ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match Map.mem t.reservations job.id, saturated t key with
        | true, _ ->
          Error
            (Agent_protocol.Error.create
               Conflict
               ~message:"job already has a capacity reservation"
               ~retryable:false
               ())
        | false, true -> Ok None
        | false, false ->
          reserve t key;
          let reservation =
            { lease = { owner = t; key; released = false; reservation_id = Some job.id }
            ; session_id = job.session_id
            ; generation = job.generation
            ; phase = Staged
            }
          in
          t.reservations <- Map.set t.reservations ~key:job.id ~data:reservation;
          Ok (Some reservation))
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "only a new queued job can reserve admission capacity"))
;;

let publish reservation =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true reservation.lease.owner.mutex (fun () ->
      match reservation.phase, reservation.lease.released with
      | Staged, false -> reservation.phase <- Ready
      | _ -> ()))
;;

let abort reservation =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true reservation.lease.owner.mutex (fun () ->
      match reservation.phase with
      | Staged -> release_locked reservation.lease
      | Ready | Claimed -> ()))
;;

let try_acquire_job t key ~(job : Agent_protocol.Job.t) =
  if
    not
      (match job.status with
       | Queued -> true
       | _ -> false)
  then
    Error
      (Agent_protocol.Error.invalid_request "scheduler capacity requires a queued job")
  else if not (valid_key key job)
  then
    Error
      (Agent_protocol.Error.invalid_request
         "job capacity owner differs from scheduler key")
  else if key.nested_depth < 0 || key.nested_depth > t.limits.max_nested_depth
  then Error (depth_error t.limits.max_nested_depth key.nested_depth)
  else
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match Map.find t.reservations job.id with
      | Some reservation
        when (not (Key.equal reservation.lease.key key))
             || not (Int.equal reservation.generation job.generation) ->
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"job does not match its reserved capacity"
             ~retryable:false
             ())
      | Some reservation ->
        (match reservation.phase with
         | Staged | Claimed -> Ok None
         | Ready ->
           reservation.phase <- Claimed;
           Ok (Some reservation.lease))
      | None ->
        (match saturated t key with
         | true -> Ok None
         | false ->
           reserve t key;
           let lease =
             { owner = t; key; released = false; reservation_id = Some job.id }
           in
           t.reservations
           <- Map.set
                t.reservations
                ~key:job.id
                ~data:
                  { lease
                  ; session_id = job.session_id
                  ; generation = job.generation
                  ; phase = Claimed
                  };
           Ok (Some lease)))
;;

let retire_job t (job : Agent_protocol.Job.t) =
  match job.status with
  | Queued | Running | Waiting_permission _ -> ()
  | Waiting_completion _ | Succeeded | Failed _ | Cancelled | Interrupted _ ->
    Eio.Cancel.protect (fun () ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match Map.find t.reservations job.id with
        | Some reservation
          when Agent_protocol.Id.Session.equal reservation.session_id job.session_id
               && Int.equal reservation.generation job.generation
               && Agent_protocol.Job.equal_kind reservation.lease.key.kind job.kind ->
          (match reservation.phase with
           | Staged | Ready -> release_locked reservation.lease
           | Claimed -> ())
        | _ -> ()))
;;

let retire_unclaimed t ~matches =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Map.data t.reservations
      |> List.iter ~f:(fun reservation ->
        match matches reservation, reservation.phase with
        | true, (Staged | Ready) -> release_locked reservation.lease
        | _ -> ())))
;;

let retire_previous_generations t ~session_id ~generation =
  retire_unclaimed t ~matches:(fun reservation ->
    Agent_protocol.Id.Session.equal reservation.session_id session_id
    && reservation.generation < generation)
;;

let close_session t ~session_id =
  retire_unclaimed t ~matches:(fun reservation ->
    Agent_protocol.Id.Session.equal reservation.session_id session_id)
;;
