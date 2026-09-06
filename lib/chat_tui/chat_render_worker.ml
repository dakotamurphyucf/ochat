open Core
module Job = Chat_message_render_job
module Runtime = Chat_render_worker_runtime

let runtime_initialization_mutex = Stdlib.Mutex.create ()

type submit_result =
  | Queued
  | Already_pending
  | Rejected

type direction =
  | Preferred
  | Neutral
  | Opposite

type ranked_job =
  { job : Job.t
  ; distance : int
  ; direction : direction
  }

type retry_result =
  | Retried
  | Exhausted
  | Stale

type work =
  { mutable job : Job.t
  ; config : Runtime.Config.t
  ; resize_generation : int
  ; batch_id : int
  ; mutable distance : int
  ; mutable direction : direction
  ; sequence : int
  ; mutable retries : int
  ; valid : bool Atomic.t
  }

type worker_command =
  | Render of work
  | Stop

type completion =
  | Rendered of work * Job.result
  | Failed of work * exn
  | Stop_completion

type queues =
  { mutable visible : work list
  ; mutable prefetch : work list
  ; mutable background : work list
  }

type metrics =
  { submitted : int Atomic.t
  ; coalesced : int Atomic.t
  ; dropped : int Atomic.t
  ; completed : int Atomic.t
  ; stale : int Atomic.t
  ; synchronous_fallback : int Atomic.t
  ; failed : int Atomic.t
  ; render_count : int Atomic.t
  ; render_latency_ns : int Atomic.t
  ; max_queue_depth : int Atomic.t
  }

type t =
  { queue_capacity : int
  ; worker_count : int
  ; work_stream : worker_command Eio.Stream.t
  ; foreground_stream : worker_command Eio.Stream.t option
  ; completions : completion Eio.Stream.t
  ; changed : Eio.Condition.t
  ; latest : (string, work) Hashtbl.t
  ; queues : queues
  ; staged : work list Atomic.t
  ; in_flight : work list Atomic.t
  ; outstanding : work list Atomic.t
  ; mutable config : Runtime.Config.t
  ; mutable next_submission_generation : int
  ; mutable next_sequence : int
  ; active : bool Atomic.t
  ; metrics : metrics
  ; force_synchronous : (string, Job.Key.t) Hashtbl.t
  }

let slot ~resize_generation ~batch_id (job : Job.t) =
  String.concat
    ~sep:"\x1f"
    [ Int.to_string resize_generation
    ; Int.to_string batch_id
    ; Int.to_string job.key.width
    ; Projected_message.Id.to_string job.key.row_id
    ; Int.to_string job.key.row_revision
    ]
;;

let family_slot ~resize_generation ~batch_id (job : Job.t) =
  String.concat
    ~sep:"\x1f"
    [ Int.to_string resize_generation
    ; Int.to_string batch_id
    ; Int.to_string job.key.width
    ; Projected_message.Id.to_string job.key.row_id
    ]
;;

let same_job (a : Job.t) (b : Job.t) = Job.Key.equal a.key b.key

let queued_count t =
  List.length t.queues.visible
  + List.length t.queues.prefetch
  + List.length t.queues.background
  + List.length (Atomic.get t.staged)
;;

let atomic_incr value = ignore (Atomic.fetch_and_add value 1 : int)

let rec update_max value candidate =
  let current = Atomic.get value in
  if candidate > current
  then
    if not (Atomic.compare_and_set value current candidate)
    then update_max value candidate
;;

let rec update_atomic atom f =
  let current = Atomic.get atom in
  if not (Atomic.compare_and_set atom current (f current)) then update_atomic atom f
;;

let remove_outstanding t work =
  update_atomic t.outstanding (fun current ->
    List.filter current ~f:(fun candidate -> not (phys_equal candidate work)))
;;

let record_queue_depth t = update_max t.metrics.max_queue_depth (queued_count t)

let work_slot work =
  slot ~resize_generation:work.resize_generation ~batch_id:work.batch_id work.job
;;

let work_family_slot work =
  family_slot ~resize_generation:work.resize_generation ~batch_id:work.batch_id work.job
;;

let remove_family jobs target =
  List.filter jobs ~f:(fun work -> not (String.equal (work_family_slot work) target))
;;

let remove_queued_family t target =
  t.queues.visible <- remove_family t.queues.visible target;
  t.queues.prefetch <- remove_family t.queues.prefetch target;
  t.queues.background <- remove_family t.queues.background target
;;

let invalidate_work t work =
  if Atomic.exchange work.valid false
  then (
    atomic_incr t.metrics.dropped;
    remove_outstanding t work)
;;

let invalidate_job t job =
  let invalidate work = if same_job work.job job then invalidate_work t work in
  List.iter (Atomic.get t.outstanding) ~f:invalidate
;;

let rec take_oldest = function
  | [] -> None
  | [ item ] -> Some (item, [])
  | item :: rest ->
    Option.map (take_oldest rest) ~f:(fun (oldest, remaining) ->
      oldest, item :: remaining)
;;

let forget_if_latest t work =
  invalidate_work t work;
  let target = work_slot work in
  match Hashtbl.find t.latest target with
  | Some latest when phys_equal latest work -> Hashtbl.remove t.latest target
  | None | Some _ -> ()
;;

let drop_from t get set =
  match take_oldest (get t.queues) with
  | None -> false
  | Some (work, remaining) ->
    set t.queues remaining;
    forget_if_latest t work;
    true
;;

let make_room t priority =
  let background () =
    drop_from t (fun q -> q.background) (fun q value -> q.background <- value)
  in
  let prefetch () =
    drop_from t (fun q -> q.prefetch) (fun q value -> q.prefetch <- value)
  in
  let visible () = drop_from t (fun q -> q.visible) (fun q value -> q.visible <- value) in
  match priority with
  | Job.Priority.Background -> false
  | Prefetch -> background () || prefetch ()
  | Visible -> background () || prefetch () || visible ()
;;

let direction_rank = function
  | Preferred -> 0
  | Neutral -> 1
  | Opposite -> 2
;;

let compare_work left right =
  match Int.compare left.distance right.distance with
  | 0 ->
    (match
       Int.compare (direction_rank left.direction) (direction_rank right.direction)
     with
     | 0 -> Int.compare left.sequence right.sequence
     | order -> order)
  | order -> order
;;

let insert work queue = List.sort (work :: queue) ~compare:compare_work

let queue t work =
  update_atomic t.outstanding (fun current -> work :: current);
  match work.job.priority with
  | Visible -> t.queues.visible <- insert work t.queues.visible
  | Prefetch -> t.queues.prefetch <- insert work t.queues.prefetch
  | Background -> t.queues.background <- insert work t.queues.background
;;

let pop t =
  let pop_from get set =
    match get t.queues with
    | work :: remaining ->
      set t.queues remaining;
      Some work
    | [] -> None
  in
  match pop_from (fun q -> q.visible) (fun q value -> q.visible <- value) with
  | Some _ as work -> work
  | None ->
    (match pop_from (fun q -> q.prefetch) (fun q value -> q.prefetch <- value) with
     | Some _ as work -> work
     | None -> pop_from (fun q -> q.background) (fun q value -> q.background <- value))
;;

let pop_foreground t =
  let rec take acc = function
    | [] -> None
    | work :: rest ->
      if Poly.(work.job.priority = Job.Priority.Visible)
      then Some (work, List.rev_append acc rest)
      else take (work :: acc) rest
  in
  match take [] t.queues.visible with
  | None -> None
  | Some (work, remaining) ->
    t.queues.visible <- remaining;
    Some work
;;

let pop_general t =
  let pop_visible () =
    let rec take acc = function
      | [] -> None
      | work :: rest ->
        if
          Option.is_none t.foreground_stream
          || not Poly.(work.job.priority = Job.Priority.Visible)
        then Some (work, List.rev_append acc rest)
        else take (work :: acc) rest
    in
    match take [] t.queues.visible with
    | None -> None
    | Some (work, remaining) ->
      t.queues.visible <- remaining;
      Some work
  in
  match pop_visible () with
  | Some _ as work -> work
  | None ->
    (match t.queues.prefetch with
     | work :: remaining ->
       t.queues.prefetch <- remaining;
       Some work
     | [] ->
       (match t.queues.background with
        | work :: remaining ->
          t.queues.background <- remaining;
          Some work
        | [] -> None))
;;

let is_latest t work =
  Hashtbl.find t.latest (work_slot work) |> Option.exists ~f:(phys_equal work)
;;

let remove_outstanding_from outstanding work =
  update_atomic outstanding (fun current ->
    List.filter current ~f:(fun candidate -> not (phys_equal candidate work)))
;;

let rec dispatch_with t ~stream ~pop =
  if not (Atomic.get t.active)
  then ()
  else (
    match pop t with
    | Some work ->
      if is_latest t work
      then (
        update_atomic t.staged (fun current -> work :: current);
        Eio.Stream.add stream (Render work);
        update_atomic t.staged (fun current ->
          List.filter current ~f:(fun candidate -> not (phys_equal candidate work))))
      else Atomic.set work.valid false;
      dispatch_with t ~stream ~pop
    | None ->
      Eio.Condition.await_no_mutex t.changed;
      dispatch_with t ~stream ~pop)
;;

let runtime_renderer ~code_cache_capacity =
  let runtime = ref None in
  fun is_cancelled config job ->
    let current =
      match !runtime with
      | Some current
        when Int.equal
               (Runtime.theme_generation current)
               (Runtime.Config.theme_generation config)
             && Int.equal
                  (Runtime.grammar_generation current)
                  (Runtime.Config.grammar_generation config) -> current
      | None | Some _ ->
        let current =
          Stdlib.Mutex.lock runtime_initialization_mutex;
          Exn.protect
            ~f:(fun () ->
              Runtime.create ~config ~code_cache_capacity () |> Or_error.ok_exn)
            ~finally:(fun () -> Stdlib.Mutex.unlock runtime_initialization_mutex)
        in
        runtime := Some current;
        current
    in
    Runtime.render current ~is_cancelled job
;;

let run_worker
      ~domain_mgr
      ~render
      ~publish
      ~active
      ~metrics
      ~in_flight
      ~outstanding
      ~now
      work_stream
  =
  let render = render () in
  let rec loop () =
    match Eio.Stream.take work_stream with
    | Stop -> ()
    | Render work ->
      update_atomic in_flight (fun current -> work :: current);
      if Atomic.get work.valid
      then (
        let started_at = now () in
        match
          Eio.Domain_manager.run domain_mgr (fun () ->
            render (fun () -> not (Atomic.get work.valid)) work.config work.job)
        with
        | result ->
          let elapsed_ns =
            Mtime.span started_at (now ())
            |> Mtime.Span.to_float_ns
            |> Float.iround_down_exn
          in
          atomic_incr metrics.render_count;
          ignore (Atomic.fetch_and_add metrics.render_latency_ns elapsed_ns : int);
          if Atomic.get active then publish (Rendered (work, result))
        | exception Exit -> ()
        | exception exn ->
          if Atomic.get active then publish (Failed (work, exn));
          Eio.Fiber.check ());
      update_atomic in_flight (fun current ->
        List.filter current ~f:(fun candidate -> not (phys_equal candidate work)));
      remove_outstanding_from outstanding work;
      loop ()
  in
  loop ()
;;

let create_internal
      ~sw
      ~domain_mgr
      ~config
      ~worker_count
      ~queue_capacity
      ~render
      ~on_result
      ~on_error
      ~now
      ()
  =
  let completions = Eio.Stream.create 0 in
  let create_state () =
    if worker_count <= 0 then invalid_arg "Chat_render_worker.create: no workers";
    if queue_capacity <= 0 then invalid_arg "Chat_render_worker.create: empty queue";
    let foreground_stream =
      if worker_count >= 2 then Some (Eio.Stream.create 0) else None
    in
    { queue_capacity
    ; worker_count
    ; work_stream = Eio.Stream.create 0
    ; foreground_stream
    ; completions
    ; changed = Eio.Condition.create ()
    ; latest = Hashtbl.create (module String)
    ; queues = { visible = []; prefetch = []; background = [] }
    ; staged = Atomic.make []
    ; in_flight = Atomic.make []
    ; outstanding = Atomic.make []
    ; config
    ; next_submission_generation = 0
    ; next_sequence = 0
    ; active = Atomic.make true
    ; metrics =
        { submitted = Atomic.make 0
        ; coalesced = Atomic.make 0
        ; dropped = Atomic.make 0
        ; completed = Atomic.make 0
        ; stale = Atomic.make 0
        ; synchronous_fallback = Atomic.make 0
        ; failed = Atomic.make 0
        ; render_count = Atomic.make 0
        ; render_latency_ns = Atomic.make 0
        ; max_queue_depth = Atomic.make 0
        }
    ; force_synchronous = Hashtbl.create (module String)
    }
  in
  let t = create_state () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      match Eio.Stream.take completions with
      | Rendered (work, result) ->
        if Atomic.get t.active && Atomic.get work.valid
        then (
          atomic_incr t.metrics.completed;
          on_result result);
        loop ()
      | Failed (work, error) ->
        if Atomic.get t.active && Atomic.get work.valid then on_error work.job error;
        loop ()
      | Stop_completion -> `Stop_daemon
    in
    loop ());
  Eio.Fiber.fork_daemon ~sw (fun () ->
    dispatch_with t ~stream:t.work_stream ~pop:pop_general;
    `Stop_daemon);
  Option.iter t.foreground_stream ~f:(fun foreground_stream ->
    Eio.Fiber.fork_daemon ~sw (fun () ->
      dispatch_with t ~stream:foreground_stream ~pop:pop_foreground;
      `Stop_daemon);
    Eio.Fiber.fork_daemon ~sw (fun () ->
      run_worker
        ~domain_mgr
        ~render
        ~publish:(Eio.Stream.add completions)
        ~active:t.active
        ~metrics:t.metrics
        ~in_flight:t.in_flight
        ~outstanding:t.outstanding
        ~now
        foreground_stream;
      `Stop_daemon));
  let general_worker_count =
    worker_count - Option.value_map t.foreground_stream ~default:0 ~f:(fun _ -> 1)
  in
  for _ = 1 to general_worker_count do
    Eio.Fiber.fork_daemon ~sw (fun () ->
      run_worker
        ~domain_mgr
        ~render
        ~publish:(Eio.Stream.add completions)
        ~active:t.active
        ~metrics:t.metrics
        ~in_flight:t.in_flight
        ~outstanding:t.outstanding
        ~now
        t.work_stream;
      `Stop_daemon)
  done;
  t
;;

let create
      ~sw
      ~domain_mgr
      ~config
      ~worker_count
      ~queue_capacity
      ~code_cache_capacity
      ~on_result
      ~on_error
      ~now
      ()
  =
  create_internal
    ~sw
    ~domain_mgr
    ~config
    ~worker_count
    ~queue_capacity
    ~render:(fun () -> runtime_renderer ~code_cache_capacity)
    ~on_result
    ~on_error
    ~now
    ()
;;

let submit_ranked t ~resize_generation ~batch_id (ranked : ranked_job) =
  let job = ranked.job in
  atomic_incr t.metrics.submitted;
  let target = slot ~resize_generation ~batch_id job in
  let family = family_slot ~resize_generation ~batch_id job in
  if not (Atomic.get t.active)
  then (
    atomic_incr t.metrics.dropped;
    Rejected)
  else (
    match Hashtbl.find t.latest target with
    | Some current when same_job current.job job ->
      atomic_incr t.metrics.coalesced;
      Already_pending
    | None | Some _ ->
      Hashtbl.to_alist t.latest
      |> List.iter ~f:(fun (candidate_slot, candidate) ->
        if String.equal (work_family_slot candidate) family
        then (
          invalidate_work t candidate;
          Hashtbl.remove t.latest candidate_slot));
      remove_queued_family t family;
      let queue_is_full = queued_count t >= t.queue_capacity in
      let outstanding_is_full =
        Hashtbl.length t.latest >= t.queue_capacity + t.worker_count
      in
      if (queue_is_full || outstanding_is_full) && not (make_room t job.priority)
      then (
        atomic_incr t.metrics.dropped;
        Rejected)
      else (
        let job = { job with submission_generation = t.next_submission_generation } in
        t.next_submission_generation <- t.next_submission_generation + 1;
        let sequence = t.next_sequence in
        t.next_sequence <- t.next_sequence + 1;
        let work =
          { job
          ; config = t.config
          ; resize_generation
          ; batch_id
          ; distance = Int.max 0 ranked.distance
          ; direction = ranked.direction
          ; sequence
          ; retries = 0
          ; valid = Atomic.make true
          }
        in
        Hashtbl.set t.latest ~key:target ~data:work;
        queue t work;
        record_queue_depth t;
        Eio.Condition.broadcast t.changed;
        Queued))
;;

let submit t job =
  submit_ranked
    t
    ~resize_generation:0
    ~batch_id:0
    ({ job; distance = Int.max_value; direction = Neutral } : ranked_job)
;;

let submit_batch t ~resize_generation ~batch_id (jobs : ranked_job list) =
  List.map jobs ~f:(fun ranked -> submit_ranked t ~resize_generation ~batch_id ranked)
;;

let reprioritize_batch t ~resize_generation ~batch_id (jobs : ranked_job list) =
  let by_slot = Hashtbl.create (module String) in
  List.iter jobs ~f:(fun ranked ->
    Hashtbl.set by_slot ~key:(slot ~resize_generation ~batch_id ranked.job) ~data:ranked);
  let update work =
    if
      Int.equal work.resize_generation resize_generation
      && Int.equal work.batch_id batch_id
    then
      Option.iter
        (Hashtbl.find by_slot (work_slot work))
        ~f:(fun ranked ->
          work.job <- { work.job with priority = ranked.job.priority };
          work.distance <- Int.max 0 ranked.distance;
          work.direction <- ranked.direction)
  in
  List.iter t.queues.visible ~f:update;
  List.iter t.queues.prefetch ~f:update;
  List.iter t.queues.background ~f:update;
  let queued = t.queues.visible @ t.queues.prefetch @ t.queues.background in
  t.queues.visible <- [];
  t.queues.prefetch <- [];
  t.queues.background <- [];
  List.iter queued ~f:(fun work ->
    match work.job.priority with
    | Visible -> t.queues.visible <- insert work t.queues.visible
    | Prefetch -> t.queues.prefetch <- insert work t.queues.prefetch
    | Background -> t.queues.background <- insert work t.queues.background);
  Eio.Condition.broadcast t.changed
;;

let cancel_generation t ~resize_generation =
  let cancelled =
    List.filter (Atomic.get t.outstanding) ~f:(fun work ->
      Int.equal work.resize_generation resize_generation)
  in
  List.iter cancelled ~f:(invalidate_work t);
  let keep work = not (Int.equal work.resize_generation resize_generation) in
  t.queues.visible <- List.filter t.queues.visible ~f:keep;
  t.queues.prefetch <- List.filter t.queues.prefetch ~f:keep;
  t.queues.background <- List.filter t.queues.background ~f:keep;
  Hashtbl.filter_inplace t.latest ~f:(fun work ->
    not (Int.equal work.resize_generation resize_generation));
  Eio.Condition.broadcast t.changed
;;

let available_slots t =
  Int.max
    0
    (Int.min
       (t.queue_capacity - queued_count t)
       (t.queue_capacity + t.worker_count - Hashtbl.length t.latest))
;;

let accepts_result t result =
  match
    Hashtbl.to_alist t.latest
    |> List.find ~f:(fun (_, work) -> Job.result_matches result work.job)
  with
  | Some (target, _) ->
    Hashtbl.remove t.latest target;
    true
  | None ->
    atomic_incr t.metrics.stale;
    false
;;

let reject t job =
  match
    Hashtbl.to_alist t.latest |> List.find ~f:(fun (_, latest) -> same_job latest.job job)
  with
  | Some (target, _) ->
    Hashtbl.remove t.latest target;
    true
  | None -> false
;;

let retry_failed t job =
  match
    Hashtbl.to_alist t.latest |> List.find ~f:(fun (_, latest) -> same_job latest.job job)
  with
  | None -> Stale
  | Some (target, work) ->
    if work.retries >= 1
    then (
      Hashtbl.remove t.latest target;
      invalidate_work t work;
      Exhausted)
    else (
      work.retries <- work.retries + 1;
      queue t work;
      record_queue_depth t;
      Eio.Condition.broadcast t.changed;
      Retried)
;;

let force_synchronous t job =
  Hashtbl.set
    t.force_synchronous
    ~key:(slot ~resize_generation:0 ~batch_id:0 job)
    ~data:job.key
;;

let should_render_synchronously t job =
  let target = slot ~resize_generation:0 ~batch_id:0 job in
  let present =
    Hashtbl.find t.force_synchronous target |> Option.exists ~f:(Job.Key.equal job.key)
  in
  Hashtbl.remove t.force_synchronous target;
  present
;;

let retain_slots jobs =
  List.map jobs ~f:(slot ~resize_generation:0 ~batch_id:0) |> String.Hash_set.of_list
;;

let invalidate_outside t ~priority keep =
  let invalidate work =
    if Poly.equal work.job.priority priority && not (Hash_set.mem keep (work_slot work))
    then forget_if_latest t work
  in
  List.iter (Atomic.get t.outstanding) ~f:invalidate;
  let filtered =
    Hashtbl.filter_keys t.latest ~f:(fun target ->
      match Hashtbl.find t.latest target with
      | None -> true
      | Some work ->
        not (Poly.equal work.job.priority priority && not (Hash_set.mem keep target)))
  in
  Hashtbl.clear t.latest;
  Hashtbl.iteri filtered ~f:(fun ~key ~data -> Hashtbl.set t.latest ~key ~data)
;;

let submit_visible_viewport t jobs =
  let keep = retain_slots jobs in
  invalidate_outside t ~priority:Job.Priority.Visible keep;
  t.queues.visible <- List.filter t.queues.visible ~f:(fun work -> Atomic.get work.valid);
  List.iter jobs ~f:(fun job ->
    ignore (submit t { job with priority = Job.Priority.Visible } : submit_result))
;;

let submit_prefetch_viewport t jobs =
  let keep = retain_slots jobs in
  invalidate_outside t ~priority:Job.Priority.Prefetch keep;
  List.iter t.queues.prefetch ~f:(forget_if_latest t);
  t.queues.prefetch <- [];
  List.iter jobs ~f:(fun job ->
    ignore (submit t { job with priority = Job.Priority.Prefetch } : submit_result))
;;

let update_config t config =
  t.config <- config;
  List.iter (Atomic.get t.outstanding) ~f:(invalidate_work t);
  t.queues.visible <- [];
  t.queues.prefetch <- [];
  t.queues.background <- [];
  Hashtbl.clear t.latest;
  Hashtbl.clear t.force_synchronous
;;

let close t =
  if Atomic.exchange t.active false
  then (
    List.iter (Atomic.get t.outstanding) ~f:(invalidate_work t);
    t.queues.visible <- [];
    t.queues.prefetch <- [];
    t.queues.background <- [];
    Hashtbl.clear t.latest;
    Hashtbl.clear t.force_synchronous;
    Eio.Condition.broadcast t.changed)
;;

let theme_generation t = Runtime.Config.theme_generation t.config
let grammar_generation t = Runtime.Config.grammar_generation t.config
let record_synchronous_fallback t = atomic_incr t.metrics.synchronous_fallback
let record_failure t = atomic_incr t.metrics.failed

let metrics_json t =
  let number value = `Number (Int.to_string (Atomic.get value)) in
  let render_count = Atomic.get t.metrics.render_count in
  let render_latency_ns = Atomic.get t.metrics.render_latency_ns in
  let average_render_latency_ms =
    if render_count = 0
    then 0.
    else Float.of_int render_latency_ns /. Float.of_int render_count /. 1_000_000.
  in
  `Object
    [ "submitted", number t.metrics.submitted
    ; "coalesced", number t.metrics.coalesced
    ; "dropped", number t.metrics.dropped
    ; "completed", number t.metrics.completed
    ; "stale", number t.metrics.stale
    ; "synchronous_fallback", number t.metrics.synchronous_fallback
    ; "failed", number t.metrics.failed
    ; "queue_depth", `Number (Int.to_string (queued_count t))
    ; "max_queue_depth", number t.metrics.max_queue_depth
    ; "render_count", `Number (Int.to_string render_count)
    ; ( "render_latency_ms_total"
      , `Number (Float.to_string (Float.of_int render_latency_ns /. 1_000_000.)) )
    ; "render_latency_ms_average", `Number (Float.to_string average_render_latency_ms)
    ]
;;

module For_testing = struct
  type stats =
    { queued : int
    ; pending : int
    ; queue_capacity : int
    ; worker_count : int
    }

  let stats t =
    { queued = queued_count t
    ; pending = Hashtbl.length t.latest
    ; queue_capacity = t.queue_capacity
    ; worker_count = t.worker_count
    }
  ;;

  let queued_jobs t =
    List.concat
      [ List.map t.queues.visible ~f:(fun work -> work.job)
      ; List.map t.queues.prefetch ~f:(fun work -> work.job)
      ; List.map t.queues.background ~f:(fun work -> work.job)
      ]
  ;;

  let create_detached ~config ~queue_capacity ~worker_count =
    if worker_count <= 0 then invalid_arg "Chat_render_worker.create: no workers";
    if queue_capacity <= 0 then invalid_arg "Chat_render_worker.create: empty queue";
    { queue_capacity
    ; worker_count
    ; work_stream = Eio.Stream.create 0
    ; foreground_stream = None
    ; completions = Eio.Stream.create 0
    ; changed = Eio.Condition.create ()
    ; latest = Hashtbl.create (module String)
    ; queues = { visible = []; prefetch = []; background = [] }
    ; staged = Atomic.make []
    ; in_flight = Atomic.make []
    ; outstanding = Atomic.make []
    ; config
    ; next_submission_generation = 0
    ; next_sequence = 0
    ; active = Atomic.make true
    ; metrics =
        { submitted = Atomic.make 0
        ; coalesced = Atomic.make 0
        ; dropped = Atomic.make 0
        ; completed = Atomic.make 0
        ; stale = Atomic.make 0
        ; synchronous_fallback = Atomic.make 0
        ; failed = Atomic.make 0
        ; render_count = Atomic.make 0
        ; render_latency_ns = Atomic.make 0
        ; max_queue_depth = Atomic.make 0
        }
    ; force_synchronous = Hashtbl.create (module String)
    }
  ;;

  let create
        ~sw
        ~domain_mgr
        ~config
        ~worker_count
        ~queue_capacity
        ~render
        ~on_result
        ~on_error
        ~now
        ()
    =
    create_internal
      ~sw
      ~domain_mgr
      ~config
      ~worker_count
      ~queue_capacity
      ~render
      ~on_result
      ~on_error
      ~now
      ()
  ;;

  let runtime_renderer = runtime_renderer
  let metrics_json = metrics_json
end
