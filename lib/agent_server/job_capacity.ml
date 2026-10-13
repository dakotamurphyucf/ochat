open! Core
module P = Agent_protocol

module Principal_bucket = struct
  module T = struct
    type t =
      | Anonymous
      | Principal of P.Id.Principal.t
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Workload_bucket = struct
  module T = struct
    type t =
      | Conversation_prompt of string
      | Developer_project of Chatml_source.Id.t
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Owner_bucket = struct
  module T = struct
    type t =
      | Conversation_session of P.Id.Session.t
      | Developer_run of
          { host_id : P.Id.Server.t
          ; run_id : P.Id.Developer_run.t
          ; principal_id : P.Id.Principal.t
          }
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Job_kind = struct
  module T = struct
    type t = P.Job.kind [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Conversation_job_id = struct
  module T = struct
    type t = P.Id.Job.t [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Key = struct
  type scope =
    | Conversation of P.Id.Session.t
    | Developer of P.Execution_owner.developer
  [@@deriving equal]

  type t =
    { principal : Principal_bucket.t
    ; workload : Workload_bucket.t
    ; workspace_conflict_domain : string
    ; owner_bucket : Owner_bucket.t
    ; scope : scope
    ; kind : P.Job.kind
    ; nested_depth : int
    }
  [@@deriving equal]

  let developer_bucket (owner : P.Execution_owner.developer) =
    Owner_bucket.Developer_run
      { host_id = owner.host_id
      ; run_id = owner.run_id
      ; principal_id = owner.principal_id
      }
  ;;

  let create
        ~principal_id
        ~prompt
        ~workspace_conflict_domain
        ~session_id
        ~kind
        ~nested_depth
    =
    { principal =
        Option.value_map principal_id ~default:Principal_bucket.Anonymous ~f:(fun id ->
          Principal_bucket.Principal id)
    ; workload = Conversation_prompt prompt
    ; workspace_conflict_domain
    ; owner_bucket = Conversation_session session_id
    ; scope = Conversation session_id
    ; kind
    ; nested_depth
    }
  ;;

  let developer ~owner ~project_source ~workspace_conflict_domain ~kind ~nested_depth =
    let open Result.Let_syntax in
    let%bind () = P.Execution_owner.validate (P.Execution_owner.Developer owner) in
    let%map () =
      if String.is_empty workspace_conflict_domain
      then Error (P.Error.invalid_request "developer workspace domain is empty")
      else Ok ()
    in
    { principal = Principal owner.principal_id
    ; workload = Developer_project project_source
    ; workspace_conflict_domain
    ; owner_bucket = developer_bucket owner
    ; scope = Developer owner
    ; kind
    ; nested_depth
    }
  ;;

  let matches_job t job =
    P.Job.equal_kind t.kind (P.Host_job.kind job)
    && (match P.Host_job.to_conversation job with
        | Some conversation ->
          Option.for_all conversation.launch ~f:(fun launch ->
            Int.equal t.nested_depth launch.nested_depth)
        | None -> Int.equal t.nested_depth (P.Host_job.nested_depth job))
    &&
    match t.scope, P.Host_job.owner job with
    | Conversation session_id, P.Execution_owner.Conversation owner ->
      P.Id.Session.equal session_id owner.session_id
    | Developer expected, P.Execution_owner.Developer actual ->
      P.Execution_owner.equal_developer expected actual
    | Conversation _, Developer _ | Developer _, Conversation _ -> false
  ;;
end

type counts =
  { mutable global : int
  ; mutable principals :
      (Principal_bucket.t, int, Principal_bucket.comparator_witness) Map.t
  ; mutable workloads : (Workload_bucket.t, int, Workload_bucket.comparator_witness) Map.t
  ; mutable workspaces : (string, int, String.comparator_witness) Map.t
  ; mutable owners : (Owner_bucket.t, int, Owner_bucket.comparator_witness) Map.t
  ; mutable kinds : (Job_kind.t, int, Job_kind.comparator_witness) Map.t
  }

type t =
  { limits : Config.Server.job_limits
  ; mutex : Eio.Mutex.t
  ; counts : counts
  ; mutable reservations :
      (P.Host_job.Identity.t, reservation, P.Host_job.Identity.comparator_witness) Map.t
  ; mutable conversation_ids :
      ( Conversation_job_id.t
        , P.Host_job.Identity.t
        , Conversation_job_id.comparator_witness )
        Map.t
  }

and lease =
  { owner : t
  ; key : Key.t
  ; mutable released : bool
  ; reservation_id : P.Host_job.Identity.t option
  }

and reservation =
  { lease : lease
  ; identity : P.Host_job.Identity.t
  ; queued_attempt : int
  ; mutable phase : reservation_phase
  }

and reservation_phase =
  | Staged
  | Ready
  | Claimed

let create ~limits =
  { limits
  ; mutex = Eio.Mutex.create ()
  ; reservations = Map.empty (module P.Host_job.Identity)
  ; conversation_ids = Map.empty (module Conversation_job_id)
  ; counts =
      { global = 0
      ; principals = Map.empty (module Principal_bucket)
      ; workloads = Map.empty (module Workload_bucket)
      ; workspaces = String.Map.empty
      ; owners = Map.empty (module Owner_bucket)
      ; kinds = Map.empty (module Job_kind)
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
  || count counts.workloads key.workload >= limits.per_prompt
  || count counts.workspaces key.workspace_conflict_domain >= limits.per_workspace
  || count counts.owners key.owner_bucket >= limits.per_session
  || count counts.kinds key.kind >= limits.per_kind
;;

let reserve t key =
  let counts = t.counts in
  counts.global <- counts.global + 1;
  counts.principals <- increment counts.principals key.Key.principal;
  counts.workloads <- increment counts.workloads key.workload;
  counts.workspaces <- increment counts.workspaces key.workspace_conflict_domain;
  counts.owners <- increment counts.owners key.owner_bucket;
  counts.kinds <- increment counts.kinds key.kind
;;

let depth_error maximum actual =
  P.Error.create
    Resource_limit
    ~message:(sprintf "job nesting depth %d exceeds configured maximum %d" actual maximum)
    ~retryable:false
    ()
;;

let validate_depth t key =
  if key.Key.nested_depth < 0 || key.nested_depth > t.limits.max_nested_depth
  then Error (depth_error t.limits.max_nested_depth key.nested_depth)
  else Ok ()
;;

let try_acquire t key =
  let open Result.Let_syntax in
  let%bind () = validate_depth t key in
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
    counts.workloads <- decrement counts.workloads lease.key.workload;
    counts.workspaces <- decrement counts.workspaces lease.key.workspace_conflict_domain;
    counts.owners <- decrement counts.owners lease.key.owner_bucket;
    counts.kinds <- decrement counts.kinds lease.key.kind;
    lease.released <- true;
    Option.iter lease.reservation_id ~f:(fun id ->
      match Map.find lease.owner.reservations id with
      | Some reservation when phys_equal reservation.lease lease ->
        lease.owner.reservations <- Map.remove lease.owner.reservations id;
        (match P.Host_job.Identity.owner id with
         | Conversation _ ->
           lease.owner.conversation_ids
           <- Map.remove lease.owner.conversation_ids (P.Host_job.Identity.job_id id)
         | Developer _ -> ())
      | None | Some _ -> ()))
;;

let release lease =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true lease.owner.mutex (fun () -> release_locked lease))
;;

let owner_error message = Error (P.Error.invalid_request message)

let conversation_identity_conflict t identity =
  match P.Host_job.Identity.owner identity with
  | Developer _ -> false
  | Conversation _ ->
    (match Map.find t.conversation_ids (P.Host_job.Identity.job_id identity) with
     | Some existing -> not (P.Host_job.Identity.equal existing identity)
     | None -> false)
;;

let add_reservation t reservation =
  t.reservations <- Map.set t.reservations ~key:reservation.identity ~data:reservation;
  match P.Host_job.Identity.owner reservation.identity with
  | Developer _ -> ()
  | Conversation _ ->
    t.conversation_ids
    <- Map.set
         t.conversation_ids
         ~key:(P.Host_job.Identity.job_id reservation.identity)
         ~data:reservation.identity
;;

let reserve_host_job t key ~job =
  let open Result.Let_syntax in
  let%bind () =
    if Key.matches_job key job
    then Ok ()
    else owner_error "job capacity owner differs from reservation"
  in
  let%bind () = validate_depth t key in
  match P.Host_job.lifecycle job with
  | New_queued ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let identity = P.Host_job.identity job in
      match
        ( Map.mem t.reservations identity || conversation_identity_conflict t identity
        , saturated t key )
      with
      | true, _ ->
        Error
          (P.Error.create
             Conflict
             ~message:"job already has a capacity reservation"
             ~retryable:false
             ())
      | false, true -> Ok None
      | false, false ->
        reserve t key;
        let lease =
          { owner = t; key; released = false; reservation_id = Some identity }
        in
        let reservation =
          { lease; identity; queued_attempt = P.Host_job.attempt job; phase = Staged }
        in
        add_reservation t reservation;
        Ok (Some reservation))
  | Queued_retry | Claimed_or_waiting | Terminal ->
    owner_error "only a new queued job can reserve admission capacity"
;;

let reserve_job t key ~job =
  Result.bind (P.Host_job.of_conversation job) ~f:(fun job -> reserve_host_job t key ~job)
;;

let publish reservation =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true reservation.lease.owner.mutex (fun () ->
      match reservation.phase, reservation.lease.released with
      | Staged, false -> reservation.phase <- Ready
      | Staged, true | Ready, _ | Claimed, _ -> ()))
;;

let abort reservation =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true reservation.lease.owner.mutex (fun () ->
      match reservation.phase with
      | Staged -> release_locked reservation.lease
      | Ready | Claimed -> ()))
;;

let try_acquire_host_job t key ~job =
  let open Result.Let_syntax in
  let%bind () =
    match P.Host_job.lifecycle job with
    | New_queued | Queued_retry -> Ok ()
    | Claimed_or_waiting | Terminal ->
      owner_error "scheduler capacity requires a queued job"
  in
  let%bind () =
    if Key.matches_job key job
    then Ok ()
    else owner_error "job capacity owner differs from scheduler key"
  in
  let%bind () = validate_depth t key in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let identity = P.Host_job.identity job in
    match Map.find t.reservations identity with
    | Some reservation when not (Key.equal reservation.lease.key key) ->
      Error
        (P.Error.create
           Conflict
           ~message:"job does not match its reserved capacity"
           ~retryable:false
           ())
    | Some reservation ->
      let attempt = P.Host_job.attempt job in
      (match reservation.phase with
       | Claimed when attempt > reservation.queued_attempt -> Ok None
       | (Staged | Ready | Claimed)
         when not (Int.equal attempt reservation.queued_attempt) ->
         Error
           (P.Error.create
              Conflict
              ~message:"job attempt does not match its reserved capacity"
              ~retryable:false
              ())
       | Staged | Claimed -> Ok None
       | Ready ->
         reservation.phase <- Claimed;
         Ok (Some reservation.lease))
    | None ->
      if conversation_identity_conflict t identity
      then
        Error
          (P.Error.create
             Conflict
             ~message:"conversation job ID has a different capacity owner"
             ~retryable:false
             ())
      else if saturated t key
      then Ok None
      else (
        reserve t key;
        let lease =
          { owner = t; key; released = false; reservation_id = Some identity }
        in
        let reservation =
          { lease; identity; queued_attempt = P.Host_job.attempt job; phase = Claimed }
        in
        add_reservation t reservation;
        Ok (Some lease)))
;;

let try_acquire_job t key ~job =
  Result.bind (P.Host_job.of_conversation job) ~f:(fun job ->
    try_acquire_host_job t key ~job)
;;

let retire_host_job t job =
  let retire =
    match P.Host_job.status job with
    | Queued | Running | Waiting_permission _ -> false
    | Waiting_completion _ | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
  in
  if retire
  then
    Eio.Cancel.protect (fun () ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match Map.find t.reservations (P.Host_job.identity job) with
        | Some reservation
          when Int.equal reservation.queued_attempt (P.Host_job.attempt job)
               && P.Job.equal_kind reservation.lease.key.kind (P.Host_job.kind job) ->
          (match reservation.phase with
           | Staged | Ready -> release_locked reservation.lease
           | Claimed -> ())
        | None | Some _ -> ()))
;;

let retire_job t job =
  match P.Host_job.of_conversation job with
  | Ok job -> retire_host_job t job
  | Error _ -> ()
;;

let retire_unclaimed t ~matches =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Map.data t.reservations
      |> List.iter ~f:(fun reservation ->
        if matches reservation
        then (
          match reservation.phase with
          | Staged | Ready -> release_locked reservation.lease
          | Claimed -> ()))))
;;

let retire_previous_generations t ~session_id ~generation =
  retire_unclaimed t ~matches:(fun reservation ->
    match P.Host_job.Identity.owner reservation.identity with
    | Conversation owner ->
      P.Id.Session.equal owner.session_id session_id && owner.generation < generation
    | Developer _ -> false)
;;

let same_developer_run
      (left : P.Execution_owner.developer)
      (right : P.Execution_owner.developer)
  =
  P.Id.Server.equal left.host_id right.host_id
  && P.Id.Developer_run.equal left.run_id right.run_id
  && P.Id.Principal.equal left.principal_id right.principal_id
;;

let retire_previous_epochs t ~current_owner =
  retire_unclaimed t ~matches:(fun reservation ->
    match P.Host_job.Identity.owner reservation.identity with
    | Conversation _ -> false
    | Developer owner ->
      same_developer_run owner current_owner && owner.epoch < current_owner.epoch)
;;

let close_session t ~session_id =
  retire_unclaimed t ~matches:(fun reservation ->
    match P.Host_job.Identity.owner reservation.identity with
    | Conversation owner -> P.Id.Session.equal owner.session_id session_id
    | Developer _ -> false)
;;

let close_developer_run t ~owner =
  retire_unclaimed t ~matches:(fun reservation ->
    match P.Host_job.Identity.owner reservation.identity with
    | Conversation _ -> false
    | Developer reserved -> same_developer_run owner reserved)
;;
