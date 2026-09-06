open Core
module Job = Chat_tui.Chat_message_render_job
module Worker = Chat_tui.Chat_render_worker
module Runtime = Chat_tui.Chat_render_worker_runtime

let config generation =
  Runtime.Config.create
    ~custom_grammars:[]
    ~theme_generation:0
    ~grammar_generation:generation
;;

let job
      ?(index = 0)
      ?row_id
      ?(row_revision = 1)
      ?(width = 40)
      ?(priority = Job.Priority.Visible)
      ?(text = Int.to_string index)
      generation
  =
  let row_id =
    Option.value_or_thunk row_id ~default:(fun () ->
      Chat_tui.Projected_message.Id.local
        ~namespace:"render-worker-test"
        ~local_id:(Int.to_string index)
      |> Result.ok_or_failwith)
  in
  Job.create
    ~transcript_generation:1
    ~row_id
    ~row_revision
    ~message_index:index
    ~message_revision:1
    ~width
    ~role:"assistant"
    ~text
    ~tool_output:None
    ~tool_call_outcome:None
    ~theme_generation:0
    ~grammar_generation:generation
    ~geometry_generation:1
    ~request_generation:0
    ~render_generation:0
    ~submission_generation:0
    ~semantic_seed:None
    ~priority
;;

let%test_unit "worker slots use row identity rather than index hints" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:4
      ~worker_count:1
  in
  let row_id =
    Chat_tui.Projected_message.Id.local ~namespace:"render-worker-test" ~local_id:"moved"
    |> Result.ok_or_failwith
  in
  assert (Poly.equal (Worker.submit worker (job ~index:1 ~row_id 1)) Queued);
  assert (
    Poly.equal (Worker.submit worker (job ~index:3 ~row_id ~text:"1" 1)) Already_pending);
  let other =
    Chat_tui.Projected_message.Id.local ~namespace:"render-worker-test" ~local_id:"other"
    |> Result.ok_or_failwith
  in
  assert (Poly.equal (Worker.submit worker (job ~index:3 ~row_id:other 1)) Queued);
  let queued = Worker.For_testing.queued_jobs worker in
  [%test_eq: int] (List.length queued) 2
;;

let%test_unit "worker preserves preparation generation and assigns submission identity" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:2
      ~worker_count:1
  in
  let original = { (job 1) with request_generation = 41 } in
  assert (Poly.equal (Worker.submit worker original) Queued);
  let queued = Worker.For_testing.queued_jobs worker |> List.hd_exn in
  [%test_eq: int] queued.request_generation 41;
  [%test_eq: int] queued.submission_generation 0
;;

let%test_unit "new row revision supersedes old detached work" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:4
      ~worker_count:1
  in
  let row_id =
    Chat_tui.Projected_message.Id.local
      ~namespace:"render-worker-test"
      ~local_id:"revision"
    |> Result.ok_or_failwith
  in
  let old = job ~row_id ~row_revision:0 1 in
  let current = job ~row_id ~row_revision:1 1 in
  assert (Poly.equal (Worker.submit worker old) Queued);
  assert (Poly.equal (Worker.submit worker current) Queued);
  assert (
    not
      (Worker.accepts_result
         worker
         (Job.result old ~image:(Notty.I.string Notty.A.empty old.key.text))));
  let queued = Worker.For_testing.queued_jobs worker in
  [%test_eq: int] (List.length queued) 1;
  [%test_eq: int] (List.hd_exn queued).key.row_revision 1
;;

let detached_result job =
  Job.result job ~image:(Notty.I.string Notty.A.empty job.Job.key.text)
;;

let result_index result = result.Job.key.message_index

module Barrier = struct
  type t =
    { mutex : Stdlib.Mutex.t
    ; condition : Stdlib.Condition.t
    ; mutable open_ : bool
    }

  let create () =
    { mutex = Stdlib.Mutex.create ()
    ; condition = Stdlib.Condition.create ()
    ; open_ = false
    }
  ;;

  let await t =
    Stdlib.Mutex.lock t.mutex;
    while not t.open_ do
      Stdlib.Condition.wait t.condition t.mutex
    done;
    Stdlib.Mutex.unlock t.mutex
  ;;

  let open_ t =
    Stdlib.Mutex.lock t.mutex;
    t.open_ <- true;
    Stdlib.Condition.broadcast t.condition;
    Stdlib.Mutex.unlock t.mutex
  ;;
end

let await_atomic ~clock value =
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 5.) (fun () ->
    while not (Atomic.get value) do
      Eio.Fiber.yield ()
    done)
;;

let%test_unit "domain service is bounded and coalesces duplicate requests" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let entered = Atomic.make false in
  let release = Barrier.create () in
  let results = Eio.Stream.create 8 in
  let render () =
    fun _is_cancelled _config (job : Job.t) ->
    if Int.equal job.Job.key.message_index 0
    then (
      Atomic.set entered true;
      Barrier.await release);
    detached_result job
  in
  let worker =
    Worker.For_testing.create
      ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config:(config 1)
      ~worker_count:1
      ~queue_capacity:2
      ~render
      ~on_result:(Eio.Stream.add results)
      ~on_error:(fun _ error -> raise error)
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ()
  in
  assert (Poly.equal (Worker.submit worker (job 1)) Queued);
  assert (Poly.equal (Worker.submit worker (job 1)) Already_pending);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) entered;
  ignore
    (Worker.submit worker (job ~index:1 ~priority:Background 1) : Worker.submit_result);
  ignore (Worker.submit worker (job ~index:2 ~priority:Prefetch 1) : Worker.submit_result);
  ignore (Worker.submit worker (job ~index:3 1) : Worker.submit_result);
  let stats = Worker.For_testing.stats worker in
  assert (stats.queued <= stats.queue_capacity);
  assert (stats.pending <= stats.queue_capacity + stats.worker_count);
  Barrier.open_ release;
  let first = Eio.Stream.take results in
  assert (Worker.accepts_result worker first);
  assert (not (Worker.accepts_result worker first));
  Worker.close worker;
  assert (Poly.equal (Worker.submit worker (job ~index:9 1)) Rejected)
;;

let%test_unit "config changes discard queued old-generation work" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let entered = Atomic.make false in
  let finished = Atomic.make false in
  let release = Barrier.create () in
  let results = Eio.Stream.create 4 in
  let render () =
    fun _is_cancelled _config (job : Job.t) ->
    Atomic.set entered true;
    Barrier.await release;
    Atomic.set finished true;
    detached_result job
  in
  let worker =
    Worker.For_testing.create
      ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config:(config 1)
      ~worker_count:1
      ~queue_capacity:2
      ~render
      ~on_result:(Eio.Stream.add results)
      ~on_error:(fun _ error -> raise error)
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ()
  in
  ignore (Worker.submit worker (job 1) : Worker.submit_result);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) entered;
  ignore (Worker.submit worker (job ~index:1 1) : Worker.submit_result);
  Worker.update_config worker (config 2);
  let stats = Worker.For_testing.stats worker in
  [%test_eq: int] stats.queued 0;
  [%test_eq: int] stats.pending 0;
  Barrier.open_ release;
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) finished;
  Eio.Fiber.yield ();
  assert (Option.is_none (Eio.Stream.take_nonblocking results));
  Worker.close worker
;;

let%test_unit "priority admission stays bounded and close rejects work" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:2
      ~worker_count:1
  in
  assert (Poly.equal (Worker.submit worker (job ~index:0 ~priority:Background 1)) Queued);
  assert (Poly.equal (Worker.submit worker (job ~index:1 ~priority:Prefetch 1)) Queued);
  assert (Poly.equal (Worker.submit worker (job ~index:2 ~priority:Background 1)) Rejected);
  assert (Poly.equal (Worker.submit worker (job ~index:3 1)) Queued);
  let stats = Worker.For_testing.stats worker in
  [%test_eq: int] stats.queued 2;
  assert (stats.pending <= stats.queue_capacity + stats.worker_count);
  Worker.close worker;
  assert (Poly.equal (Worker.submit worker (job ~index:4 1)) Rejected);
  let stats = Worker.For_testing.stats worker in
  [%test_eq: int] stats.queued 0;
  [%test_eq: int] stats.pending 0
;;

let%test_unit "new prefetch viewport replaces old queued prefetch work" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:4
      ~worker_count:1
  in
  Worker.submit_prefetch_viewport
    worker
    [ job ~index:1 ~priority:Prefetch 1; job ~index:2 ~priority:Prefetch 1 ];
  let first = Worker.For_testing.stats worker in
  [%test_eq: int] first.queued 2;
  [%test_eq: int] first.pending 2;
  Worker.submit_prefetch_viewport worker [ job ~index:8 ~priority:Prefetch 1 ];
  let second = Worker.For_testing.stats worker in
  [%test_eq: int] second.queued 1;
  [%test_eq: int] second.pending 1
;;

let%test_unit "new visible viewport replaces obsolete queued visible work" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:4
      ~worker_count:1
  in
  Worker.submit_visible_viewport worker [ job ~index:1 1; job ~index:2 1 ];
  let first = Worker.For_testing.stats worker in
  [%test_eq: int] first.queued 2;
  Worker.submit_visible_viewport worker [ job ~index:9 1 ];
  let second = Worker.For_testing.stats worker in
  [%test_eq: int] second.queued 1;
  [%test_eq: int] second.pending 1
;;

let%test_unit "queued dispatch order is visible then prefetch then background" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:4
      ~worker_count:1
  in
  assert (Poly.equal (Worker.submit worker (job ~index:1 ~priority:Background 1)) Queued);
  assert (Poly.equal (Worker.submit worker (job ~index:2 ~priority:Prefetch 1)) Queued);
  assert (Poly.equal (Worker.submit worker (job ~index:3 1)) Queued);
  let indices =
    Worker.For_testing.queued_jobs worker
    |> List.map ~f:(fun job -> job.Job.key.message_index)
  in
  [%test_eq: int list] indices [ 3; 2; 1 ]
;;

let%test_unit "progressive batches keep widths independent and rank deterministically" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:8
      ~worker_count:2
  in
  let row_id =
    Chat_tui.Projected_message.Id.local
      ~namespace:"render-worker-test"
      ~local_id:"two-widths"
    |> Result.ok_or_failwith
  in
  let narrow = job ~row_id ~width:40 ~priority:Prefetch 1 in
  let wide = job ~row_id ~width:72 ~priority:Prefetch 1 in
  let ranked job distance direction : Worker.ranked_job = { job; distance; direction } in
  let submitted =
    Worker.submit_batch
      worker
      ~resize_generation:4
      ~batch_id:2
      [ ranked narrow 8 Opposite; ranked wide 2 Preferred ]
  in
  assert (List.for_all submitted ~f:(Poly.equal Worker.Queued));
  let queued = Worker.For_testing.queued_jobs worker in
  [%test_eq: int list] (List.map queued ~f:(fun job -> job.key.width)) [ 72; 40 ];
  let narrow_result =
    detached_result (List.find_exn queued ~f:(fun job -> job.key.width = 40))
  in
  let wide_result =
    detached_result (List.find_exn queued ~f:(fun job -> job.key.width = 72))
  in
  assert (Worker.accepts_result worker narrow_result);
  assert (Worker.accepts_result worker wide_result)
;;

let%test_unit "resize generation cancellation preserves unrelated visible work" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:8
      ~worker_count:2
  in
  let resize_jobs =
    List.init 4 ~f:(fun index ->
      { Worker.job = job ~index ~priority:Background 1
      ; distance = index
      ; direction = Worker.Neutral
      })
  in
  ignore
    (Worker.submit_batch worker ~resize_generation:8 ~batch_id:0 resize_jobs
     : Worker.submit_result list);
  assert (Poly.equal (Worker.submit worker (job ~index:9 1)) Queued);
  Worker.cancel_generation worker ~resize_generation:8;
  let queued = Worker.For_testing.queued_jobs worker in
  [%test_eq: int list] (List.map queued ~f:(fun job -> job.key.message_index)) [ 9 ];
  assert (Worker.accepts_result worker (detached_result (List.hd_exn queued)))
;;

let%test_unit "equivalent rows in separate generations cancel independently" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:8
      ~worker_count:2
  in
  let shared = job ~priority:Background 1 in
  let ranked : Worker.ranked_job = { job = shared; distance = 0; direction = Neutral } in
  ignore
    (Worker.submit_batch worker ~resize_generation:8 ~batch_id:0 [ ranked ]
     : Worker.submit_result list);
  ignore
    (Worker.submit_batch worker ~resize_generation:9 ~batch_id:0 [ ranked ]
     : Worker.submit_result list);
  Worker.cancel_generation worker ~resize_generation:8;
  let queued = Worker.For_testing.queued_jobs worker in
  [%test_eq: int] (List.length queued) 1;
  assert (Worker.accepts_result worker (detached_result (List.hd_exn queued)))
;;

let%test_unit "reprioritization changes queued tier and distance order" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:8
      ~worker_count:2
  in
  let first = job ~index:1 ~priority:Background 1 in
  let second = job ~index:2 ~priority:Background 1 in
  let ranked job distance direction : Worker.ranked_job = { job; distance; direction } in
  ignore
    (Worker.submit_batch
       worker
       ~resize_generation:3
       ~batch_id:1
       [ ranked first 1 Preferred; ranked second 2 Opposite ]
     : Worker.submit_result list);
  Worker.reprioritize_batch
    worker
    ~resize_generation:3
    ~batch_id:1
    [ ranked { second with priority = Prefetch } 0 Preferred; ranked first 5 Opposite ];
  let queued = Worker.For_testing.queued_jobs worker in
  [%test_eq: int list] (List.map queued ~f:(fun job -> job.key.message_index)) [ 2; 1 ];
  assert (Poly.((List.hd_exn queued).priority = Job.Priority.Prefetch))
;;

let%test_unit "progressive admission stays bounded for ten thousand rows" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:32
      ~worker_count:2
  in
  for index = 0 to 9_999 do
    ignore
      (Worker.submit_batch
         worker
         ~resize_generation:1
         ~batch_id:(index / 16)
         [ { Worker.job = job ~index ~priority:Background 1
           ; distance = index
           ; direction = Worker.Neutral
           }
         ]
       : Worker.submit_result list);
    let stats = Worker.For_testing.stats worker in
    assert (stats.queued <= stats.queue_capacity);
    assert (stats.pending <= stats.queue_capacity + stats.worker_count)
  done
;;

let%test_unit "reserved foreground lane starts visible work during background rendering" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let background_entered = Atomic.make false in
  let visible_entered = Atomic.make false in
  let release = Barrier.create () in
  let render () =
    fun _is_cancelled _config (job : Job.t) ->
    if Poly.(job.priority = Job.Priority.Background)
    then (
      Atomic.set background_entered true;
      Barrier.await release)
    else Atomic.set visible_entered true;
    detached_result job
  in
  let worker =
    Worker.For_testing.create
      ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config:(config 1)
      ~worker_count:2
      ~queue_capacity:4
      ~render
      ~on_result:(fun _ -> ())
      ~on_error:(fun _ error -> raise error)
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ()
  in
  ignore
    (Worker.submit_batch
       worker
       ~resize_generation:3
       ~batch_id:0
       [ { Worker.job = job ~priority:Background 1
         ; distance = 0
         ; direction = Worker.Neutral
         }
       ]
     : Worker.submit_result list);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) background_entered;
  assert (Poly.equal (Worker.submit worker (job ~index:9 1)) Queued);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) visible_entered;
  Barrier.open_ release;
  Worker.close worker
;;

let%test_unit "reserved foreground lane includes visible target-width work" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let background_entered = Atomic.make false in
  let visible_entered = Atomic.make false in
  let release = Barrier.create () in
  let render () =
    fun _is_cancelled _config (job : Job.t) ->
    if Poly.(job.priority = Job.Priority.Background)
    then (
      Atomic.set background_entered true;
      Barrier.await release)
    else Atomic.set visible_entered true;
    detached_result job
  in
  let worker =
    Worker.For_testing.create
      ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config:(config 1)
      ~worker_count:2
      ~queue_capacity:4
      ~render
      ~on_result:(fun _ -> ())
      ~on_error:(fun _ error -> raise error)
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ()
  in
  ignore
    (Worker.submit_batch
       worker
       ~resize_generation:7
       ~batch_id:0
       [ { Worker.job = job ~priority:Background 1
         ; distance = 0
         ; direction = Worker.Neutral
         }
       ]
     : Worker.submit_result list);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) background_entered;
  ignore
    (Worker.submit_batch
       worker
       ~resize_generation:7
       ~batch_id:1
       [ { Worker.job = job ~index:9 ~priority:Visible 1
         ; distance = 0
         ; direction = Worker.Neutral
         }
       ]
     : Worker.submit_result list);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) visible_entered;
  Barrier.open_ release;
  Worker.close worker
;;

let%test_unit "superseded in-flight work is cancelled and only newest result is current" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let entered = Atomic.make false in
  let release = Barrier.create () in
  let cancelled = Atomic.make false in
  let results = Eio.Stream.create 4 in
  let render () =
    fun is_cancelled _config (job : Job.t) ->
    if String.equal job.key.text "old"
    then (
      Atomic.set entered true;
      Barrier.await release;
      Atomic.set cancelled (is_cancelled ());
      if is_cancelled () then raise Exit);
    detached_result job
  in
  let worker =
    Worker.For_testing.create
      ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config:(config 1)
      ~worker_count:1
      ~queue_capacity:2
      ~render
      ~on_result:(Eio.Stream.add results)
      ~on_error:(fun _ error -> raise error)
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ()
  in
  assert (Poly.equal (Worker.submit worker (job ~text:"old" 1)) Queued);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) entered;
  Exn.protect
    ~f:(fun () -> assert (Poly.equal (Worker.submit worker (job ~text:"new" 1)) Queued))
    ~finally:(fun () -> Barrier.open_ release);
  let new_result = Eio.Stream.take results in
  assert (Atomic.get cancelled);
  assert (Worker.accepts_result worker new_result);
  assert (Option.is_none (Eio.Stream.take_nonblocking results));
  Worker.close worker
;;

let%test_unit "close cancels active work and suppresses late callbacks" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let entered = Atomic.make false in
  let release = Barrier.create () in
  let cancelled = Atomic.make false in
  let callback_count = Atomic.make 0 in
  let render () =
    fun is_cancelled _config (job : Job.t) ->
    Atomic.set entered true;
    Barrier.await release;
    Atomic.set cancelled (is_cancelled ());
    detached_result job
  in
  let worker =
    Worker.For_testing.create
      ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config:(config 1)
      ~worker_count:1
      ~queue_capacity:2
      ~render
      ~on_result:(fun _ -> ignore (Atomic.fetch_and_add callback_count 1 : int))
      ~on_error:(fun _ _ -> ignore (Atomic.fetch_and_add callback_count 1 : int))
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ()
  in
  assert (Poly.equal (Worker.submit worker (job 1)) Queued);
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) entered;
  Worker.close worker;
  Worker.close worker;
  Barrier.open_ release;
  await_atomic ~clock:(Eio.Stdenv.mono_clock env) cancelled;
  assert (Atomic.get cancelled);
  [%test_eq: int] (Atomic.get callback_count) 0;
  assert (Poly.equal (Worker.submit worker (job ~index:1 1)) Rejected)
;;

let%expect_test "metrics report lifecycle, bounds, staleness, and fallback" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:1
      ~worker_count:1
  in
  let first = job 1 in
  assert (Poly.equal (Worker.submit worker first) Queued);
  assert (Poly.equal (Worker.submit worker first) Already_pending);
  assert (Poly.equal (Worker.submit worker (job ~index:1 ~priority:Background 1)) Rejected);
  Worker.record_synchronous_fallback worker;
  ignore (Worker.accepts_result worker (detached_result first) : bool);
  ignore (Worker.accepts_result worker (detached_result (job ~index:9 1)) : bool);
  print_endline (Jsonaf.to_string (Worker.For_testing.metrics_json worker));
  [%expect
    {|
    {"submitted":3,"coalesced":1,"dropped":1,"completed":0,"stale":1,"synchronous_fallback":1,"failed":0,"queue_depth":1,"max_queue_depth":1,"render_count":0,"render_latency_ms_total":0.,"render_latency_ms_average":0.}
    |}]
;;

let%test_unit "failed work retries once and stale failures do nothing" =
  let worker =
    Worker.For_testing.create_detached
      ~config:(config 1)
      ~queue_capacity:2
      ~worker_count:1
  in
  let failed = job 1 in
  assert (Poly.equal (Worker.submit worker failed) Queued);
  let submitted = Worker.For_testing.queued_jobs worker |> List.hd_exn in
  assert (Poly.equal (Worker.retry_failed worker submitted) Retried);
  assert (Poly.equal (Worker.retry_failed worker submitted) Exhausted);
  assert (Poly.equal (Worker.retry_failed worker submitted) Stale);
  let stats = Worker.For_testing.stats worker in
  [%test_eq: int] stats.pending 0
;;
