open! Core
module P = Agent_protocol
module Root = Developer_root_owner
module Runner = Developer_background_job_runner
module Model_runner = Developer_model_job_runner
module Capacity_decision = Developer_job_worker.Capacity_decision

module Offer_key = struct
  module T = struct
    type t =
      { identity : P.Host_job.Identity.t
      ; attempt : int
      }
    [@@deriving compare, sexp_of]
  end

  include T
end

type call =
  { mutable cancel : Eio.Cancel.t option
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  }

type attachment =
  { root : Root.run
  ; mutable runner : Runner.t option
  ; mutable model_runner : Model_runner.t option
  ; generation : int
  ; mutable alive : bool
  ; mutable wake : bool
  ; mutable cursor : int
  ; mutable offers : (Offer_key.t * call) list
  }

type batch =
  { attachment : attachment
  ; owner : P.Execution_owner.t
  ; candidates : Root.Job_candidate.t list
  }

type offer_result =
  | Skipped
  | Unavailable
  | Attempted

type scan =
  { attachment : attachment
  ; call : call
  }

type t =
  { sw : Eio.Switch.t
  ; now : unit -> float
  ; registry : Root.Registry.t
  ; mutable closed : bool
  ; mutable attachments : attachment list
  ; mutable retiring : attachment list
  ; mutable scan : scan option
  ; mutable batch : batch option
  ; mutable retry : batch option
  ; mutable next_generation : int
  ; mutable next_root : int
  ; mutable next_scan_at : float
  }

let batch_limit = 64
let fallback_seconds = 0.25
let invalid message = P.Error.invalid_request message
let conflict message = P.Error.create P.Error.Conflict ~message ~retryable:true ()

let stopped () =
  P.Error.create
    P.Error.Server_shutting_down
    ~message:"developer scheduler is stopped"
    ~retryable:true
    ()
;;

let new_call () =
  let finished, finish = Eio.Promise.create () in
  { cancel = None; finished; finish }
;;

let finish_call call =
  call.cancel <- None;
  Eio.Promise.resolve call.finish ()
;;

let cancel_call call =
  Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit)
;;

let live_roots t = Root.Registry.active_for_scheduler t.registry
let root_live roots root = List.mem roots root ~equal:phys_equal

let attachment_live t attachment =
  if t.closed || not attachment.alive
  then Ok false
  else Result.map (live_roots t) ~f:(fun roots -> root_live roots attachment.root)
;;

module Availability = struct
  type t =
    | Runnable
    | Malformed_supported
    | Artifact_installer_unavailable
    | Executor_unavailable
  [@@deriving equal, sexp_of]
end

module Batch = struct
  type t = batch

  let owner t = t.owner
  let candidates t = t.candidates

  let availability t candidate =
    if not (List.mem t.candidates candidate ~equal:phys_equal)
    then Error (invalid "candidate is not in this developer scheduler batch")
    else (
      match P.Developer_job.kind (Root.Job_candidate.job candidate) with
      | P.Job.Async_tool ->
        (match t.attachment.runner with
         | Some runner ->
           Result.map (Runner.availability runner candidate) ~f:(function
             | Runner.Availability.Runnable -> Availability.Runnable
             | Malformed_supported -> Malformed_supported
             | Agent_artifact_installer_unavailable -> Artifact_installer_unavailable
             | Issuer_source_installer_unavailable -> Executor_unavailable
             | Unsupported_kind -> Executor_unavailable)
         | None -> Ok Availability.Executor_unavailable)
      | Model_call ->
        (match t.attachment.model_runner with
         | Some runner ->
           Result.map (Model_runner.availability runner candidate) ~f:(function
             | Model_runner.Availability.Runnable -> Availability.Runnable
             | Malformed_supported -> Malformed_supported
             | Unsupported_kind -> Executor_unavailable)
         | None -> Ok Availability.Executor_unavailable)
      | Nested_agent | Scheduled_event | Shell_process | Compaction ->
        Ok Availability.Executor_unavailable)
  ;;
end

let create ~sw ~clock ~registry ~capacity =
  if not (Root.Registry.uses_capacity registry capacity)
  then invalid_arg "developer scheduler uses another capacity ledger";
  { sw
  ; now = (fun () -> Eio.Time.now clock)
  ; registry
  ; closed = false
  ; attachments = []
  ; retiring = []
  ; scan = None
  ; batch = None
  ; retry = None
  ; next_generation = 0
  ; next_root = 0
  ; next_scan_at = Eio.Time.now clock
  }
;;

let find_attachment t root =
  List.find t.attachments ~f:(fun attachment -> phys_equal attachment.root root)
;;

let attach_new t ~root ~roots ~runner ~model_runner =
  if (not (List.is_empty t.retiring)) || List.length t.attachments >= List.length roots
  then Error (conflict "developer scheduler attachments are at their bound")
  else (
    t.next_generation <- t.next_generation + 1;
    t.attachments
    <- t.attachments
       @ [ { root
           ; runner
           ; model_runner
           ; generation = t.next_generation
           ; alive = true
           ; wake = true
           ; cursor = 0
           ; offers = []
           }
         ];
    Ok ())
;;

let attach t ~root ~runner =
  if t.closed
  then Error (stopped ())
  else if not (Runner.issued_by_root runner root)
  then Error (invalid "developer runner belongs to another physical root")
  else
    let open Result.Let_syntax in
    let%bind roots = live_roots t in
    if not (root_live roots root)
    then Error (invalid "developer root is not active in this scheduler registry")
    else (
      match find_attachment t root with
      | Some attachment ->
        (match attachment.runner with
         | Some installed when phys_equal installed runner -> Ok ()
         | Some _ -> Error (conflict "developer root already has another Tool runner")
         | None ->
           attachment.runner <- Some runner;
           attachment.wake <- true;
           Ok ())
      | None -> attach_new t ~root ~roots ~runner:(Some runner) ~model_runner:None)
;;

let attach_model t ~root ~runner =
  if t.closed
  then Error (stopped ())
  else if not (Model_runner.issued_by_root runner root)
  then Error (invalid "developer Model runner belongs to another physical root")
  else
    let open Result.Let_syntax in
    let%bind roots = live_roots t in
    if not (root_live roots root)
    then Error (invalid "developer root is not active in this scheduler registry")
    else (
      match find_attachment t root with
      | Some attachment ->
        (match attachment.model_runner with
         | Some installed when phys_equal installed runner -> Ok ()
         | Some _ -> Error (conflict "developer root already has another Model runner")
         | None ->
           attachment.model_runner <- Some runner;
           attachment.wake <- true;
           Ok ())
      | None -> attach_new t ~root ~roots ~runner:None ~model_runner:(Some runner))
;;

let wake t ~root =
  if not t.closed
  then Option.iter (find_attachment t root) ~f:(fun attachment -> attachment.wake <- true)
;;

let current t (batch : batch) =
  let attachment = batch.attachment in
  if
    not
      (List.exists t.attachments ~f:(fun active ->
         phys_equal active attachment && Int.equal active.generation attachment.generation))
  then Ok false
  else attachment_live t attachment
;;

let selected_window attachment candidates =
  let total = List.length candidates in
  if total = 0
  then [], 0
  else (
    let offset = if attachment.cursor >= total then 0 else attachment.cursor in
    let selected = List.drop candidates offset |> Fn.flip List.take batch_limit in
    let next = offset + List.length selected in
    selected, if next >= total then 0 else next)
;;

let scan_key : attachment Eio.Fiber.key = Eio.Fiber.create_key ()
let offer_key : attachment Eio.Fiber.key = Eio.Fiber.create_key ()

let scanner_current t scan =
  match t.scan with
  | Some active when phys_equal active scan -> attachment_live t scan.attachment
  | None | Some _ -> Ok false
;;

let start_scan t attachment ~fallback_due =
  Eio.Switch.check t.sw;
  let call = new_call () in
  let scan = { attachment; call } in
  t.scan <- Some scan;
  attachment.wake <- false;
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    Exn.protect
      ~finally:(fun () ->
        if fallback_due then t.next_scan_at <- t.now () +. fallback_seconds;
        (match t.scan with
         | Some active when phys_equal active scan -> t.scan <- None
         | None | Some _ -> ());
        finish_call call)
      ~f:(fun () ->
        try
          Eio.Cancel.sub (fun cancel ->
            call.cancel <- Some cancel;
            Eio.Fiber.with_binding scan_key attachment (fun () ->
              match
                Root.Job_candidate.read_current
                  attachment.root
                  ~checkpoint:Eio.Fiber.check
              with
              | Error _ -> ()
              | Ok all ->
                (match scanner_current t scan with
                 | Error _ | Ok false -> ()
                 | Ok true ->
                   let candidates, next = selected_window attachment all in
                   attachment.cursor <- next;
                   (match candidates with
                    | [] -> ()
                    | first :: _ ->
                      let owner = Root.Job_candidate.host_job first |> P.Host_job.owner in
                      if
                        List.for_all candidates ~f:(fun candidate ->
                          P.Execution_owner.equal
                            owner
                            (Root.Job_candidate.host_job candidate |> P.Host_job.owner))
                      then t.batch <- Some { attachment; owner; candidates }
                      else invalid_arg "one physical Root returned different job owners"))))
        with
        | Eio.Cancel.Cancelled _ -> ()))
;;

let choose_attachment t ~fallback_due =
  let available =
    List.filter t.attachments ~f:(fun attachment ->
      match attachment_live t attachment with
      | Ok true -> true
      | Ok false | Error _ -> false)
  in
  match
    if fallback_due
    then None
    else List.find available ~f:(fun attachment -> attachment.wake)
  with
  | Some attachment -> Some attachment
  | None ->
    let count = List.length available in
    if count = 0
    then None
    else (
      let index = t.next_root % count in
      t.next_root <- (index + 1) % count;
      List.nth available index)
;;

let poll t =
  let fallback_due = Float.(t.now () >= t.next_scan_at) in
  if
    (not t.closed)
    && Option.is_none t.scan
    && Option.is_none t.batch
    && (fallback_due || List.exists t.attachments ~f:(fun attachment -> attachment.wake))
  then
    Option.iter (choose_attachment t ~fallback_due) ~f:(fun attachment ->
      start_scan t attachment ~fallback_due)
;;

let take_batch t =
  let batch = t.batch in
  t.batch <- None;
  batch
;;

let retry_batch t = t.retry

let clear_retry t batch =
  match t.retry with
  | Some retry when phys_equal retry batch -> t.retry <- None
  | None | Some _ -> ()
;;

let rotate_after batch candidate =
  let before, rest =
    List.split_while batch.candidates ~f:(fun current ->
      not (phys_equal current candidate))
  in
  match rest with
  | current :: after when phys_equal current candidate ->
    { batch with candidates = after @ before @ [ current ] }
  | [] | _ -> invalid_arg "developer retry candidate is absent from its batch"
;;

let retry_pending t ~root =
  Option.value_map t.retry ~default:false ~f:(fun batch ->
    phys_equal batch.attachment.root root && batch.attachment.alive)
;;

let key candidate =
  let host = Root.Job_candidate.host_job candidate in
  { Offer_key.identity = P.Host_job.identity host; attempt = P.Host_job.attempt host }
;;

let offer t (batch : batch) candidate =
  let open Result.Let_syntax in
  let%bind is_current = current t batch in
  if not is_current
  then (
    clear_retry t batch;
    Ok Skipped)
  else if not (List.mem batch.candidates candidate ~equal:phys_equal)
  then Error (invalid "candidate is not in this developer scheduler batch")
  else (
    let attachment = batch.attachment in
    let candidate_key = key candidate in
    if
      List.exists attachment.offers ~f:(fun (existing, _) ->
        Offer_key.compare existing candidate_key = 0)
      || List.length attachment.offers >= Root.Registry.max_active_workers t.registry
    then Ok Skipped
    else (
      Eio.Switch.check t.sw;
      let call = new_call () in
      let decided, resolve_decided = Eio.Promise.create () in
      let decision_sent = ref false in
      let signal_decision value =
        if not !decision_sent
        then (
          decision_sent := true;
          Eio.Promise.resolve resolve_decided value)
      in
      attachment.offers <- (candidate_key, call) :: attachment.offers;
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        Exn.protect
          ~finally:(fun () ->
            signal_decision Capacity_decision.Rejected;
            attachment.offers
            <- List.filter attachment.offers ~f:(fun (_, current) ->
                 not (phys_equal current call));
            finish_call call)
          ~f:(fun () ->
            try
              Eio.Cancel.sub (fun cancel ->
                call.cancel <- Some cancel;
                Eio.Fiber.with_binding offer_key attachment (fun () ->
                  match current t batch with
                  | Error _ | Ok false -> ()
                  | Ok true ->
                    (match P.Developer_job.kind (Root.Job_candidate.job candidate) with
                     | P.Job.Async_tool ->
                       Option.iter attachment.runner ~f:(fun runner ->
                         ignore
                           (Runner.run_candidate
                              runner
                              ~on_capacity_decision:signal_decision
                              candidate
                            : (P.Developer_job.t option, P.Error.t) result))
                     | Model_call ->
                       Option.iter attachment.model_runner ~f:(fun runner ->
                         ignore
                           (Model_runner.run_candidate
                              runner
                              ~on_capacity_decision:signal_decision
                              candidate
                            : (P.Developer_job.t option, P.Error.t) result))
                     | Nested_agent | Scheduled_event | Shell_process | Compaction -> ())))
            with
            | Eio.Cancel.Cancelled _ -> ()));
      match Eio.Promise.await decided with
      | Acquired | Rejected ->
        clear_retry t batch;
        Ok Attempted
      | Unavailable ->
        (match current t batch with
         | Ok true ->
           t.retry <- Some (rotate_after batch candidate);
           Ok Unavailable
         | Error _ | Ok false ->
           clear_retry t batch;
           Ok Skipped)))
;;

let same_task attachment key =
  Option.value_map (Eio.Fiber.get key) ~default:false ~f:(phys_equal attachment)
;;

let await_attachment attachment =
  List.iter attachment.offers ~f:(fun (_, call) -> Eio.Promise.await call.finished)
;;

let retire t ~root =
  let attachment =
    match find_attachment t root with
    | Some attachment -> Some attachment
    | None -> List.find t.retiring ~f:(fun attachment -> phys_equal attachment.root root)
  in
  match attachment with
  | None -> Ok ()
  | Some attachment ->
    if same_task attachment scan_key || same_task attachment offer_key
    then Error (conflict "developer scheduler task cannot retire its own root")
    else (
      attachment.alive <- false;
      t.attachments
      <- List.filter t.attachments ~f:(fun active -> not (phys_equal active attachment));
      if not (List.mem t.retiring attachment ~equal:phys_equal)
      then t.retiring <- attachment :: t.retiring;
      (match t.batch with
       | Some batch when phys_equal batch.attachment attachment -> t.batch <- None
       | None | Some _ -> ());
      (match t.retry with
       | Some batch when phys_equal batch.attachment attachment -> t.retry <- None
       | None | Some _ -> ());
      (match t.scan with
       | Some scan when phys_equal scan.attachment attachment -> cancel_call scan.call
       | None | Some _ -> ());
      List.iter attachment.offers ~f:(fun (_, call) -> cancel_call call);
      Eio.Cancel.protect (fun () ->
        (match t.scan with
         | Some scan when phys_equal scan.attachment attachment ->
           Eio.Promise.await scan.call.finished
         | None | Some _ -> ());
        await_attachment attachment;
        t.retiring
        <- List.filter t.retiring ~f:(fun retired -> not (phys_equal retired attachment)));
      Ok ())
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    t.batch <- None;
    t.retry <- None;
    List.iter t.attachments ~f:(fun attachment ->
      attachment.alive <- false;
      if not (List.mem t.retiring attachment ~equal:phys_equal)
      then t.retiring <- attachment :: t.retiring;
      List.iter attachment.offers ~f:(fun (_, call) -> cancel_call call));
    t.attachments <- [];
    Option.iter t.scan ~f:(fun scan -> cancel_call scan.call))
;;

let await_idle t =
  if not t.closed
  then Error (invalid "close developer scheduler before awaiting scans")
  else if
    Option.is_some (Eio.Fiber.get scan_key) || Option.is_some (Eio.Fiber.get offer_key)
  then Error (conflict "developer scheduler task cannot join itself")
  else (
    Eio.Cancel.protect (fun () ->
      Option.iter t.scan ~f:(fun scan -> Eio.Promise.await scan.call.finished);
      List.iter t.retiring ~f:await_attachment;
      t.retiring <- []);
    Ok ())
;;
