open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol
module O = Inference.Observation
module Inference_entry = Developer_inference_row
module Command_row = Developer_command_row
module Command_rows = Developer_command_rows

module Member = struct
  type t =
    | Invocation of P.Id.Invocation.t
    | Job of P.Id.Job.t
  [@@deriving compare, hash, sexp_of]
end

type epoch =
  { number : int
  ; capture : Chatml_source.Digest.t
  ; phase : [ `Active | `Interrupted | `Closed ]
  ; revision : int64
  }

module Invocation_entry = struct
  type t =
    { revision : int
    ; value : P.Host_invocation.t
    }

  let revision t = t.revision
  let value t = t.value
end

type t =
  { host_id : P.Id.Server.t
  ; run_id : P.Id.Developer_run.t
  ; principal_id : P.Id.Principal.t
  ; revision : int64
  ; epochs : epoch list
  ; invocations : Invocation_entry.t list
  ; jobs : P.Developer_job.t list
  ; last_inference_ordinal : int64
  ; inference_entries : Inference_entry.t list
  ; inference_bytes : int
  ; command_rows : Command_rows.t
  }

let kind = "store.developer_run"
let version = 2
let ( let* ) result f = Result.bind result ~f
let corrupt reason = Error (Store_error.Corrupt reason)
let limit reason = Error (Store_error.Document (D.Error.Limit_exceeded reason))
let invalid field reason = F.invalid field reason |> F.store
let host_id t = t.host_id
let run_id t = t.run_id
let principal_id t = t.principal_id
let revision t = t.revision
let epochs t = t.epochs
let epoch_number t = t.number
let epoch_capture t = t.capture
let epoch_phase t = t.phase
let epoch_revision (t : epoch) = t.revision
let inference_entries t = t.inference_entries
let inference_count t = List.length t.inference_entries
let inference_bytes t = t.inference_bytes
let command_rows t = Command_rows.rows t.command_rows
let find_command t key = Command_rows.find t.command_rows key
let command_count t = Command_rows.count t.command_rows
let command_bytes t = Command_rows.encoded_bytes t.command_rows
let last_inference_ordinal t = t.last_inference_ordinal

let next_inference_ordinal t =
  if Int64.(t.last_inference_ordinal < zero || t.last_inference_ordinal >= max_value)
  then None
  else Some Int64.(t.last_inference_ordinal + one)
;;

let inference_scope t ~(owner : P.Execution_owner.developer) ~job_id ~ordinal ~relation =
  if
    P.Id.Server.equal t.host_id owner.host_id
    && P.Id.Developer_run.equal t.run_id owner.run_id
    && P.Id.Principal.equal t.principal_id owner.principal_id
  then Inference_entry.scope ~owner ~job_id ~ordinal ~relation
  else Error (Store_error.Corrupt "foreign inference scope owner")
;;

let find_invocation t id =
  List.find t.invocations ~f:(fun entry ->
    P.Id.Invocation.equal (P.Host_invocation.context entry.value).id id)
;;

let invocation_count t = List.length t.invocations

let find_job t id =
  List.find t.jobs ~f:(fun job -> P.Id.Job.equal (P.Developer_job.id job) id)
;;

let jobs t = t.jobs
let job_count t = List.length t.jobs

let valid_external_model_launch_shape launch =
  match P.Developer_job.Launch.parent launch with
  | Run_root ->
    (match P.Developer_job.Launch.parent_job launch with
     | None -> Int.equal (P.Developer_job.Launch.nested_depth launch) 0
     | Some (_, attempt) -> attempt > 0 && P.Developer_job.Launch.nested_depth launch > 0)
  | Invocation _ -> true
;;

let next_revision revision =
  if Int64.(revision <= zero || revision >= max_value - one)
  then corrupt "developer run document revision exhausted"
  else Ok Int64.(revision + one)
;;

let create ~(owner : P.Execution_owner.developer) ~capture_fingerprint =
  { host_id = owner.host_id
  ; run_id = owner.run_id
  ; principal_id = owner.principal_id
  ; revision = 1L
  ; epochs =
      [ { number = owner.epoch
        ; capture = capture_fingerprint
        ; phase = `Active
        ; revision = 1L
        }
      ]
  ; invocations = []
  ; jobs = []
  ; last_inference_ordinal = 0L
  ; inference_entries = []
  ; inference_bytes = 0
  ; command_rows = Command_rows.empty
  }
;;

let same_root t (owner : P.Execution_owner.developer) =
  P.Id.Server.equal t.host_id owner.host_id
  && P.Id.Developer_run.equal t.run_id owner.run_id
  && P.Id.Principal.equal t.principal_id owner.principal_id
;;

let matching_epoch t (owner : P.Execution_owner.developer) =
  if not (same_root t owner)
  then None
  else List.find t.epochs ~f:(fun epoch -> Int.equal epoch.number owner.epoch)
;;

let validate_command_owner t row =
  let owner = Command_row.owner row in
  match matching_epoch t owner with
  | Some epoch
    when Chatml_source.Digest.equal epoch.capture (Command_row.capture_fingerprint row) ->
    Ok ()
  | None | Some _ -> corrupt "developer command has foreign owner, epoch or capture"
;;

let append_command t row ~commands ~poll =
  let* command_rows =
    Command_rows.append
      t.command_rows
      row
      ~commands
      ~poll
      ~check_owner:(validate_command_owner t)
  in
  let* revision = next_revision t.revision in
  Ok { t with command_rows; revision }
;;

let change_command t ~expected ~change ~limits ~commands ~poll =
  let* command_rows =
    Command_rows.change
      t.command_rows
      ~expected
      ~change
      ~limits
      ~commands
      ~poll
      ~check_owner:(validate_command_owner t)
    |> Result.map_error ~f:(function
      | Command_row.Error.Conflict -> Store_error.Corrupt "developer command row changed"
      | Store error -> error)
  in
  let* revision = next_revision t.revision in
  Ok { t with command_rows; revision }
;;

let command_recovery t ~limits ~commands ~poll =
  let* original =
    Command_rows.of_rows
      (command_rows t)
      ~commands
      ~poll
      ~check_owner:(validate_command_owner t)
  in
  let* recovered, count = Command_rows.interrupted original ~limits ~commands ~poll in
  if Int64.(t.revision >= max_value - of_int count)
  then limit "developer command recovery exhausts document revision"
  else
    Ok { t with command_rows = recovered; revision = Int64.(t.revision + of_int count) }
;;

let validate_invocation_owner t invocation =
  let open Result.Let_syntax in
  let%bind _ =
    P.Host_invocation.developer_to_json invocation
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid developer invocation")
  in
  match (P.Host_invocation.context invocation).owner with
  | P.Execution_owner.Conversation _ -> corrupt "conversation invocation in developer run"
  | Developer owner ->
    if Option.is_some (matching_epoch t owner)
    then Ok ()
    else corrupt "developer invocation has foreign run or epoch"
;;

let validate_job_owner t job =
  let owner = P.Developer_job.owner job in
  match matching_epoch t owner with
  | Some epoch
    when Chatml_source.Digest.equal
           epoch.capture
           (P.Developer_job.capture_fingerprint job) -> Ok ()
  | Some _ | None -> corrupt "developer job has foreign run, epoch or capture"
;;

let unique_ids ~compare ids =
  Int.equal (List.length ids) (List.length (List.dedup_and_sort ids ~compare))
;;

let validate_relations t ~poll =
  let open Result.Let_syntax in
  let invocations = Hashtbl.create (module P.Id.Invocation) in
  let jobs = Hashtbl.create (module P.Id.Job) in
  List.iter t.invocations ~f:(fun entry ->
    poll ();
    Hashtbl.set
      invocations
      ~key:(P.Host_invocation.context entry.value).id
      ~data:entry.value);
  List.iter t.jobs ~f:(fun job ->
    poll ();
    Hashtbl.set jobs ~key:(P.Developer_job.id job) ~data:job);
  let same_epoch left right = P.Execution_owner.equal_developer left right in
  let invocation_parent child id =
    match Hashtbl.find invocations id with
    | None -> corrupt "developer invocation parent is missing"
    | Some parent ->
      (match (P.Host_invocation.context parent).owner with
       | P.Execution_owner.Conversation _ ->
         corrupt "developer invocation parent is foreign"
       | Developer owner ->
         if same_epoch child owner
         then Ok (Member.Invocation id)
         else corrupt "developer invocation parent has another epoch")
  in
  let job_parent child id ~attempt =
    match Hashtbl.find jobs id with
    | None -> corrupt "developer job parent is missing"
    | Some parent ->
      let latest_attempt = (P.Developer_job.state parent).attempt in
      if not (same_epoch child (P.Developer_job.owner parent))
      then corrupt "developer job parent has another epoch"
      else if attempt <= 0 || attempt > latest_attempt
      then corrupt "developer job parent attempt is outside recorded history"
      else Ok (Member.Job id)
  in
  let members =
    List.map t.invocations ~f:(fun entry ->
      poll ();
      Member.Invocation (P.Host_invocation.context entry.value).id)
    @ List.map t.jobs ~f:(fun job ->
      poll ();
      Member.Job (P.Developer_job.id job))
  in
  let count = List.length members in
  let indexes = Hashtbl.create (module Member) in
  List.iteri members ~f:(fun index member ->
    poll ();
    Hashtbl.set indexes ~key:member ~data:index);
  let parents = Array.create ~len:count [] in
  let indegree = Array.create ~len:count 0 in
  let add_edges child edges =
    let child_index = Hashtbl.find_exn indexes child in
    List.iter edges ~f:(fun parent ->
      poll ();
      let parent_index = Hashtbl.find_exn indexes parent in
      parents.(child_index) <- parent_index :: parents.(child_index);
      indegree.(parent_index) <- indegree.(parent_index) + 1)
  in
  let%bind () =
    List.fold_result t.invocations ~init:() ~f:(fun () entry ->
      poll ();
      let context = P.Host_invocation.context entry.value in
      let%bind owner =
        match context.owner with
        | P.Execution_owner.Conversation _ -> corrupt "foreign developer invocation"
        | Developer owner -> Ok owner
      in
      let%bind invocation_edge =
        match context.parent_invocation with
        | None -> Ok []
        | Some id -> Result.map (invocation_parent owner id) ~f:List.return
      in
      let%map job_edge =
        match context.parent_job with
        | None -> Ok []
        | Some id -> Result.map (job_parent owner id ~attempt:1) ~f:List.return
      in
      add_edges (Member.Invocation context.id) (invocation_edge @ job_edge))
  in
  let%bind () =
    List.fold_result t.jobs ~init:() ~f:(fun () job ->
      poll ();
      let owner = P.Developer_job.owner job in
      let launch = P.Developer_job.launch job in
      let%bind () =
        if
          P.Job.equal_kind (P.Developer_job.kind job) P.Job.Model_call
          && not (valid_external_model_launch_shape launch)
        then corrupt "external model launch has invalid ancestry or depth"
        else Ok ()
      in
      let%bind () =
        match
          ( P.Developer_job.kind job
          , P.Developer_job.Launch.parent launch
          , P.Developer_job.Launch.parent_job launch )
        with
        | P.Job.Model_call, Run_root, Some (parent_id, _) ->
          (match Hashtbl.find jobs parent_id with
           | Some parent ->
             let depth =
               P.Developer_job.Launch.nested_depth (P.Developer_job.launch parent)
             in
             if
               P.Job.equal_kind (P.Developer_job.kind parent) P.Job.Model_call
               && depth < Int.max_value
               && Int.equal (P.Developer_job.Launch.nested_depth launch) (depth + 1)
               && Chatml_source.Digest.equal
                    (P.Developer_job.capture_fingerprint parent)
                    (P.Developer_job.capture_fingerprint job)
             then Ok ()
             else corrupt "external model claimed parent kind, capture or depth differs"
           | None -> corrupt "external model claimed parent is missing")
        | P.Job.Model_call, (Run_root | Invocation _), None
        | P.Job.Model_call, Invocation _, Some _
        | ( ( P.Job.Nested_agent
            | P.Job.Scheduled_event
            | P.Job.Async_tool
            | P.Job.Shell_process
            | P.Job.Compaction )
          , _
          , _ ) -> Ok ()
      in
      let%bind invocation_edge =
        match P.Developer_job.Launch.parent launch with
        | Run_root -> Ok []
        | Invocation id ->
          let%bind parent = invocation_parent owner id in
          let invocation = Hashtbl.find_exn invocations id in
          let context = P.Host_invocation.context invocation in
          let%bind () =
            match P.Developer_job.kind job, P.Host_invocation.status invocation with
            | P.Job.Model_call, (Dispatching | Published _ | Resolved _) -> Ok ()
            | _, P.Invocation.Resolved _ -> Ok ()
            | P.Job.Model_call, Admitted ->
              corrupt "external model job has an unadmitted invocation parent"
            | _, (Admitted | Dispatching | Published _) ->
              corrupt "developer job has a nonterminal invocation parent"
          in
          let%map () =
            match P.Developer_job.Launch.parent_job launch, context.parent_job with
            | None, None -> Ok ()
            | Some (job_id, _), Some parent_id when P.Id.Job.equal job_id parent_id ->
              Ok ()
            | None, Some _ | Some _, None | Some _, Some _ ->
              corrupt "developer job and invocation disagree on parent job"
          in
          [ parent ]
      in
      let%map job_edge =
        match P.Developer_job.Launch.parent_job launch with
        | None -> Ok []
        | Some (id, attempt) -> Result.map (job_parent owner id ~attempt) ~f:List.return
      in
      add_edges (Member.Job (P.Developer_job.id job)) (invocation_edge @ job_edge))
  in
  let ready = Queue.create () in
  Array.iteri indegree ~f:(fun index degree ->
    poll ();
    if Int.equal degree 0 then Queue.enqueue ready index);
  let visited = ref 0 in
  while not (Queue.is_empty ready) do
    poll ();
    let child = Queue.dequeue_exn ready in
    incr visited;
    List.iter parents.(child) ~f:(fun parent ->
      poll ();
      indegree.(parent) <- indegree.(parent) - 1;
      if Int.equal indegree.(parent) 0 then Queue.enqueue ready parent)
  done;
  if Int.equal !visited count
  then Ok ()
  else corrupt "developer run member ancestry contains a cycle"
;;

let validate_inference_relations
      t
      ~poll
      ~limits
      ~max_inference_attempts
      ~max_inference_bytes
  =
  let open Result.Let_syntax in
  if
    List.length t.inference_entries > max_inference_attempts
    || Int64.(t.last_inference_ordinal < zero)
  then corrupt "developer inference row count or ordinal exceeds limit"
  else (
    let prior_by_scope = Hashtbl.create (module Transcript.Scope.Key) in
    let epoch_by_number = Hashtbl.create (module Int) in
    let job_by_id = Hashtbl.create (module P.Id.Job) in
    List.iter t.epochs ~f:(fun epoch ->
      poll ();
      Hashtbl.set epoch_by_number ~key:epoch.number ~data:epoch);
    List.iter t.jobs ~f:(fun job ->
      poll ();
      Hashtbl.set job_by_id ~key:(P.Developer_job.id job) ~data:job);
    let rec check last total recovered_total active_count recovered = function
      | [] ->
        if
          not
            (Int64.equal last t.last_inference_ordinal
             && Int.equal total t.inference_bytes)
        then corrupt "developer inference ordinal or byte count disagrees with rows"
        else if recovered_total > max_inference_bytes
        then limit "developer inference recovery reserve exceeds aggregate limit"
        else if Int64.(t.revision >= max_value - of_int active_count)
        then limit "developer inference recovery exhausts document revision"
        else
          Ok
            { t with
              revision = Int64.(t.revision + of_int active_count)
            ; inference_entries = List.rev recovered
            ; inference_bytes = recovered_total
            }
      | entry :: rest ->
        poll ();
        let ordinal = Inference_entry.ordinal entry in
        let owner = Inference_entry.owner entry in
        let job_id = Inference_entry.job_id entry in
        let attempt = Inference_entry.job_attempt entry in
        let record = Inference_entry.record entry in
        let scope = O.Attempt_record.scope record in
        let%bind () =
          if Int64.(ordinal <= last) || Int64.(ordinal <= zero)
          then corrupt "developer inference ordinals are not strictly increasing"
          else Ok ()
        in
        let%bind () =
          match Hashtbl.find epoch_by_number owner.epoch with
          | Some epoch
            when Chatml_source.Digest.equal
                   epoch.capture
                   (Inference_entry.capture_fingerprint entry) -> Ok ()
          | Some _ | None -> corrupt "developer inference has foreign epoch or capture"
        in
        let%bind () =
          match Hashtbl.find job_by_id job_id with
          | Some job
            when P.Job.equal_kind (P.Developer_job.kind job) P.Job.Model_call
                 && P.Execution_owner.equal_developer (P.Developer_job.owner job) owner
                 && Chatml_source.Digest.equal
                      (P.Developer_job.capture_fingerprint job)
                      (Inference_entry.capture_fingerprint entry)
                 && attempt > 0
                 && attempt <= (P.Developer_job.state job).attempt -> Ok ()
          | Some _ | None ->
            corrupt "developer inference has no matching historical model attempt"
        in
        let%bind recreated =
          Inference_entry.create
            ~ordinal
            ~owner
            ~capture_fingerprint:(Inference_entry.capture_fingerprint entry)
            ~job_id
            ~job_attempt:attempt
            ~context_id:(Inference_entry.context_id entry)
            ~record
            ~limits
        in
        let%bind recovery_entry, was_active =
          let delivery =
            match O.Attempt_record.state record with
            | Prepared -> Some Inference.Event.Terminal.Definitely_not_submitted
            | Running -> Some Inference.Event.Terminal.Possibly_submitted
            | Terminal _ | Interrupted _ -> None
          in
          match delivery with
          | None -> Ok (entry, false)
          | Some delivery ->
            (match
               Developer_inference_transition.interrupted
                 record
                 ~reason:O.Attempt_record.Host_interrupted
                 ~delivery
             with
             | Ok (Developer_inference_transition.Changed after) ->
               Result.map (Inference_entry.with_record entry after ~limits) ~f:(fun row ->
                 row, true)
             | Error Developer_inference_transition.Limit_exceeded ->
               limit "developer inference recovery row exceeds limit"
             | Ok Developer_inference_transition.Unchanged
             | Error Developer_inference_transition.Conflict ->
               corrupt "developer inference recovery form is invalid")
        in
        let%bind () =
          if Inference_entry.encoded_bytes recreated > max_inference_bytes - total
          then corrupt "developer inference aggregate bytes exceed limit"
          else Ok ()
        in
        let%bind () =
          match scope.relation with
          | Transcript.Scope.Root -> Ok ()
          | Transcript.Scope.Nested parent ->
            (match Hashtbl.find prior_by_scope parent.scope with
             | Some prior
               when P.Execution_owner.equal_developer (Inference_entry.owner prior) owner
                    && P.Id.Job.equal (Inference_entry.job_id prior) job_id
                    && Int.equal (Inference_entry.job_attempt prior) attempt
                    && Int64.(Inference_entry.ordinal prior < ordinal) -> Ok ()
             | Some _ | None -> corrupt "developer inference parent is missing or foreign")
        in
        let key = Transcript.Scope.key scope in
        let%bind () =
          if Hashtbl.mem prior_by_scope key
          then corrupt "duplicate developer inference scope"
          else Ok ()
        in
        Hashtbl.set prior_by_scope ~key ~data:entry;
        let recovered_bytes = Inference_entry.encoded_bytes recovery_entry in
        let%bind () =
          if recovered_bytes > max_inference_bytes - recovered_total
          then limit "developer inference recovery reserve exceeds aggregate limit"
          else Ok ()
        in
        let active_count = active_count + if was_active then 1 else 0 in
        check
          ordinal
          (total + Inference_entry.encoded_bytes recreated)
          (recovered_total + recovered_bytes)
          active_count
          (recovery_entry :: recovered)
          rest
    in
    check 0L 0 0 0 [] t.inference_entries)
;;

let append_inference
      t
      ~poll
      ~limits
      ~ordinal
      ~owner
      ~capture_fingerprint
      ~job_id
      ~job_attempt
      ~context_id
      ~record
      ~max_inference_attempts
      ~max_inference_bytes
  =
  let open Result.Let_syntax in
  let%bind () =
    if List.length t.inference_entries >= max_inference_attempts
    then corrupt "developer inference row quota exceeded"
    else Ok ()
  in
  let%bind () =
    match next_inference_ordinal t with
    | Some next when Int64.equal next ordinal -> Ok ()
    | Some _ | None -> corrupt "developer inference next ordinal conflicts"
  in
  let%bind entry =
    Inference_entry.create
      ~ordinal
      ~owner
      ~capture_fingerprint
      ~job_id
      ~job_attempt
      ~context_id
      ~record
      ~limits
  in
  let%bind () =
    if Inference_entry.encoded_bytes entry > max_inference_bytes - inference_bytes t
    then corrupt "developer inference aggregate bytes exceed limit"
    else Ok ()
  in
  let%bind revision = next_revision t.revision in
  let next =
    { t with
      revision
    ; last_inference_ordinal = ordinal
    ; inference_entries = t.inference_entries @ [ entry ]
    ; inference_bytes = t.inference_bytes + Inference_entry.encoded_bytes entry
    }
  in
  let%map _recovery =
    validate_inference_relations
      next
      ~poll
      ~limits
      ~max_inference_attempts
      ~max_inference_bytes
  in
  next
;;

let set_inference_record t ~poll ~limits ~ordinal ~expected ~after ~max_inference_bytes =
  let open Result.Let_syntax in
  let%bind prior =
    match
      List.find t.inference_entries ~f:(fun entry ->
        Int64.equal (Inference_entry.ordinal entry) ordinal)
    with
    | None -> corrupt "developer inference row is missing"
    | Some row -> Ok row
  in
  let%bind () =
    if
      Inference_entry.same_identity prior expected
      && Jsonaf.exactly_equal
           (O.Attempt_record.to_json (Inference_entry.record prior))
           (O.Attempt_record.to_json (Inference_entry.record expected))
    then Ok ()
    else corrupt "developer inference expected row conflicts"
  in
  let%bind updated = Inference_entry.with_record prior after ~limits in
  let%bind () =
    if
      Inference_entry.encoded_bytes updated
      > max_inference_bytes - (inference_bytes t - Inference_entry.encoded_bytes prior)
    then corrupt "developer inference aggregate bytes exceed limit"
    else Ok ()
  in
  let%map revision = next_revision t.revision in
  let inference_entries =
    List.map t.inference_entries ~f:(fun entry ->
      poll ();
      if Int64.equal (Inference_entry.ordinal entry) ordinal then updated else entry)
  in
  { t with
    revision
  ; inference_entries
  ; inference_bytes =
      t.inference_bytes
      - Inference_entry.encoded_bytes prior
      + Inference_entry.encoded_bytes updated
  }
;;

let set_invocation t ~poll ~id ~expected_revision ~after ~max_invocations =
  let open Result.Let_syntax in
  let%bind () = validate_invocation_owner t after in
  let%bind () =
    if P.Id.Invocation.equal (P.Host_invocation.context after).id id
    then Ok ()
    else corrupt "developer invocation ID differs from record"
  in
  let prior = find_invocation t id in
  let%bind entry_revision =
    match prior, expected_revision with
    | None, None ->
      if List.length t.invocations >= max_invocations
      then corrupt "developer invocation count exceeds limit"
      else Ok 1
    | Some entry, Some expected when Int.equal entry.revision expected ->
      if expected <= 0 || expected >= Int.max_value - 1
      then corrupt "developer invocation revision exhausted"
      else Ok (expected + 1)
    | None, Some _ | Some _, None | Some _, Some _ ->
      corrupt "developer invocation revision conflicts"
  in
  let%map revision = next_revision t.revision in
  let invocations =
    match prior with
    | None ->
      t.invocations @ [ { Invocation_entry.revision = entry_revision; value = after } ]
    | Some _ ->
      List.map t.invocations ~f:(fun entry ->
        poll ();
        if P.Id.Invocation.equal (P.Host_invocation.context entry.value).id id
        then { Invocation_entry.revision = entry_revision; value = after }
        else entry)
  in
  { t with revision; invocations }
;;

let append_selected
      t
      ~poll
      ~limits
      ~selected
      ~max_jobs
      ~max_selected
      ~max_job_payload_bytes
  =
  let open Result.Let_syntax in
  let%bind () =
    if
      List.length selected > max_selected
      || List.length selected > max_jobs - List.length t.jobs
    then corrupt "developer selected job count exceeds limit"
    else Ok ()
  in
  poll ();
  let all = t.jobs @ selected in
  let%bind () =
    if unique_ids ~compare:P.Id.Job.compare (List.map all ~f:P.Developer_job.id)
    then Ok ()
    else corrupt "duplicate developer job ID"
  in
  let%bind () =
    List.fold_result selected ~init:() ~f:(fun () job ->
      poll ();
      validate_job_owner t job)
  in
  let%bind _payload_bytes =
    List.fold_result all ~init:0 ~f:(fun accumulated job ->
      poll ();
      let%bind length =
        D.Json.validate_and_measure_with_checkpoint
          ~limits
          ~checkpoint:poll
          (P.Developer_job.payload job)
        |> F.store
      in
      if length > max_job_payload_bytes - accumulated
      then corrupt "developer job payload bytes exceed limit"
      else Ok (accumulated + length))
  in
  let%map revision = next_revision t.revision in
  { t with revision; jobs = all }
;;

let set_job_state t ~poll ~id ~expected_attempt ~after =
  let open Result.Let_syntax in
  let%bind prior =
    match find_job t id with
    | None -> corrupt "developer job is missing"
    | Some prior -> Ok prior
  in
  let%bind () =
    if Int.equal (P.Developer_job.state prior).attempt expected_attempt
    then Ok ()
    else corrupt "developer job attempt conflicts"
  in
  let%bind updated =
    P.Developer_job.with_state prior after
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid developer job state")
  in
  let%map revision = next_revision t.revision in
  let jobs =
    List.map t.jobs ~f:(fun job ->
      poll ();
      if P.Id.Job.equal (P.Developer_job.id job) id then updated else job)
  in
  { t with revision; jobs }
;;

let append_epoch t ~owner ~capture_fingerprint ~max_epochs =
  let open Result.Let_syntax in
  if not (same_root t owner)
  then corrupt "developer run epoch belongs to another root or principal"
  else if List.length t.epochs >= max_epochs
  then corrupt "developer run epoch limit exceeded"
  else (
    match List.last t.epochs with
    | None -> corrupt "developer run has no epoch"
    | Some last when last.number = Int.max_value ->
      corrupt "developer run epoch number exhausted"
    | Some last when owner.epoch <> last.number + 1 ->
      corrupt "developer run epoch is not the next epoch"
    | Some last ->
      let%bind revision = next_revision t.revision in
      let%map old_revision = next_revision last.revision in
      let prior =
        List.map t.epochs ~f:(fun value ->
          if value.number = last.number
          then
            { value with
              phase =
                (match value.phase with
                 | `Closed -> `Closed
                 | `Active | `Interrupted -> `Interrupted)
            ; revision = old_revision
            }
          else value)
      in
      { t with
        revision
      ; epochs =
          prior
          @ [ { number = owner.epoch
              ; capture = capture_fingerprint
              ; phase = `Active
              ; revision = 1L
              }
            ]
      })
;;

let set_phase t ~epoch ~phase =
  let open Result.Let_syntax in
  let%bind revision = next_revision t.revision in
  let found = ref false in
  let%map epochs =
    List.map t.epochs ~f:(fun value ->
      if value.number <> epoch
      then Ok value
      else (
        found := true;
        let%map epoch_revision = next_revision value.revision in
        { value with phase; revision = epoch_revision }))
    |> Result.all
    |> Result.bind ~f:(fun epochs ->
      if !found then Ok epochs else corrupt "developer run epoch is missing")
  in
  { t with revision; epochs }
;;

let phase_json = function
  | `Active -> `String "active"
  | `Interrupted -> `String "interrupted"
  | `Closed -> `String "closed"
;;

let phase_of_json = function
  | `String "active" -> Ok `Active
  | `String "interrupted" -> Ok `Interrupted
  | `String "closed" -> Ok `Closed
  | _ -> F.invalid "phase" "unsupported developer run phase"
;;

let exact_fields json names =
  match json with
  | `Object fields ->
    if
      List.length fields = List.length names
      && List.for_all fields ~f:(fun (name, _) -> List.mem names name ~equal:String.equal)
    then Ok ()
    else invalid "payload" "developer run fields are missing or unexpected"
  | _ -> invalid "payload" "developer run payload is not an object"
;;

let epoch_json epoch =
  `Object
    [ "number", F.decimal_json (Int64.of_int epoch.number)
    ; "capture_fingerprint", `String (Chatml_source.Digest.to_hex epoch.capture)
    ; "phase", phase_json epoch.phase
    ; "revision", F.decimal_json epoch.revision
    ]
;;

let invocation_entry_json (entry : Invocation_entry.t) =
  Result.map (P.Host_invocation.developer_to_json entry.value) ~f:(fun value ->
    `Object [ "revision", F.decimal_json (Int64.of_int entry.revision); "value", value ])
  |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid developer invocation")
;;

let to_payload t ~poll =
  let open Result.Let_syntax in
  let%map invocations =
    List.map t.invocations ~f:(fun entry ->
      poll ();
      invocation_entry_json entry)
    |> Result.all
  in
  `Object
    [ "host_id", P.Id.Server.to_json t.host_id
    ; "run_id", P.Id.Developer_run.to_json t.run_id
    ; "principal_id", P.Id.Principal.to_json t.principal_id
    ; "revision", F.decimal_json t.revision
    ; ( "epochs"
      , `Array
          (List.map t.epochs ~f:(fun epoch ->
             poll ();
             epoch_json epoch)) )
    ; "invocations", `Array invocations
    ; ( "jobs"
      , `Array
          (List.map t.jobs ~f:(fun job ->
             poll ();
             P.Developer_job.to_json job)) )
    ; ( "commands"
      , `Array
          (List.map (command_rows t) ~f:(fun row ->
             poll ();
             Command_row.to_json row)) )
    ; "last_inference_ordinal", F.decimal_json t.last_inference_ordinal
    ; ( "inference_attempts"
      , `Array
          (List.map t.inference_entries ~f:(fun entry ->
             poll ();
             Inference_entry.to_json entry)) )
    ]
;;

let validate
      t
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  let open Result.Let_syntax in
  let%bind () =
    if List.is_empty t.epochs || List.length t.epochs > max_epochs
    then corrupt "developer run epoch count is invalid"
    else Ok ()
  in
  let%bind () =
    if Int64.(t.revision <= zero || t.revision >= max_value)
    then corrupt "developer run revision is invalid"
    else Ok ()
  in
  let rec check previous = function
    | [] -> Ok ()
    | epoch :: rest ->
      poll ();
      let%bind () =
        if
          epoch.number < 0
          || Int64.(epoch.revision <= zero || epoch.revision >= max_value)
          || Option.value_map previous ~default:false ~f:(fun old ->
            old = Int.max_value || epoch.number <> old + 1)
        then corrupt "developer run epoch sequence is invalid"
        else Ok ()
      in
      let%bind () =
        match rest, epoch.phase with
        | _ :: _, `Active -> corrupt "only the latest developer epoch may be active"
        | _ -> Ok ()
      in
      check (Some epoch.number) rest
  in
  let%bind () = check None t.epochs in
  let%bind () =
    if List.length t.jobs > max_jobs || List.length t.invocations > max_invocations
    then corrupt "developer run member count exceeds limit"
    else Ok ()
  in
  let%bind () =
    if
      unique_ids
        ~compare:P.Id.Invocation.compare
        (List.map t.invocations ~f:(fun entry ->
           (P.Host_invocation.context entry.value).id))
      && unique_ids ~compare:P.Id.Job.compare (List.map t.jobs ~f:P.Developer_job.id)
    then Ok ()
    else corrupt "duplicate developer run member ID"
  in
  let%bind () =
    List.fold_result t.invocations ~init:() ~f:(fun () entry ->
      poll ();
      if entry.revision <= 0 || entry.revision >= Int.max_value
      then corrupt "developer invocation revision is invalid"
      else validate_invocation_owner t entry.value)
  in
  let%bind () =
    List.fold_result t.jobs ~init:() ~f:(fun () job ->
      poll ();
      let%bind () = validate_job_owner t job in
      match P.Developer_job.kind job with
      | P.Job.Model_call ->
        P.Developer_model_call.of_json (P.Developer_job.payload job) ~limits
        |> Result.map ~f:(fun _ -> ())
        |> Result.map_error ~f:(fun _ ->
          Store_error.Corrupt "invalid durable developer model call")
      | Nested_agent | Scheduled_event | Async_tool | Shell_process | Compaction -> Ok ())
  in
  let%bind () = validate_relations t ~poll in
  let%bind recovery =
    validate_inference_relations
      t
      ~poll
      ~limits
      ~max_inference_attempts
      ~max_inference_bytes
  in
  let%bind recovery = command_recovery recovery ~limits ~commands ~poll in
  let%map _payload_bytes =
    List.fold_result t.jobs ~init:0 ~f:(fun accumulated job ->
      poll ();
      let%bind length =
        D.Json.validate_and_measure_with_checkpoint
          ~limits
          ~checkpoint:poll
          (P.Developer_job.payload job)
        |> F.store
      in
      if length > max_job_payload_bytes - accumulated
      then corrupt "developer job payload bytes exceed limit"
      else Ok (accumulated + length))
  in
  recovery
;;

module Encode_input = struct
  type t =
    { payload : D.Json.t
    ; recovery_payload : D.Json.t option
    ; limits : D.Limits.t
    }
end

let prepare_encode
      t
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  let open Result.Let_syntax in
  let%bind recovery =
    validate
      t
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  in
  let%bind payload = to_payload t ~poll in
  let%map recovery_payload =
    if Int64.equal t.revision recovery.revision
    then Ok None
    else Result.map (to_payload recovery ~poll) ~f:Option.some
  in
  { Encode_input.payload; recovery_payload; limits }
;;

let finish_encode { Encode_input.payload; recovery_payload; limits } =
  let open Result.Let_syntax in
  let%bind document = D.Document.create ~limits ~kind ~version ~payload |> F.store in
  let%map () =
    match recovery_payload with
    | None -> Ok ()
    | Some payload ->
      D.Document.create ~limits ~kind ~version ~payload
      |> F.store
      |> Result.map ~f:(fun _ -> ())
  in
  D.Document.to_string document
;;

let to_bytes
      t
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  prepare_encode
    t
    ~poll
    ~limits
    ~max_epochs
    ~max_jobs
    ~max_invocations
    ~max_job_payload_bytes
    ~max_inference_attempts
    ~max_inference_bytes
    ~commands
  |> Result.bind ~f:finish_encode
;;

let required json name decode = F.required json name decode |> F.store
let protocol decode json = decode json |> F.protocol

let epoch_of_json json =
  let open Result.Let_syntax in
  let%bind () =
    exact_fields json [ "number"; "capture_fingerprint"; "phase"; "revision" ]
  in
  let%bind number =
    required json "number" F.decimal
    |> Result.bind ~f:(fun value ->
      if Int64.(value > of_int Int.max_value)
      then invalid "number" "developer run epoch exceeds int range"
      else Ok (Int64.to_int_exn value))
  in
  let%bind capture =
    required json "capture_fingerprint" F.digest
    |> Result.bind ~f:(fun hex ->
      Chatml_source.Digest.of_hex hex
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid capture digest"))
  in
  let%bind phase = required json "phase" phase_of_json in
  let%map revision = required json "revision" F.decimal in
  { number; capture; phase; revision }
;;

let invocation_entry_of_json json =
  let open Result.Let_syntax in
  let%bind () = exact_fields json [ "revision"; "value" ] in
  let%bind revision = required json "revision" F.decimal in
  let%bind () =
    if Int64.(revision <= zero || revision >= of_int Int.max_value)
    then corrupt "invalid developer invocation revision"
    else Ok ()
  in
  let%map value = required json "value" (protocol P.Host_invocation.developer_of_json) in
  { Invocation_entry.revision = Int64.to_int_exn revision; value }
;;

let of_decoded_document
      document
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  let open Result.Let_syntax in
  (* Document.decode recorded the exact admitted limits. With the Store's same
     limits this is a constant-time proof, not a second JSON traversal. *)
  let%bind () = D.Document.validate document ~limits |> F.store in
  let%bind () = F.expect document ~kind ~version |> F.store in
  let%bind () =
    exact_fields
      (D.Document.json document)
      [ "format"; "schema_version"; "kind"; "payload" ]
  in
  let payload = D.Document.payload document in
  let%bind () =
    exact_fields
      payload
      [ "host_id"
      ; "run_id"
      ; "principal_id"
      ; "revision"
      ; "epochs"
      ; "invocations"
      ; "jobs"
      ; "commands"
      ; "last_inference_ordinal"
      ; "inference_attempts"
      ]
  in
  let%bind host_id = required payload "host_id" (protocol P.Id.Server.of_json) in
  let%bind run_id = required payload "run_id" (protocol P.Id.Developer_run.of_json) in
  let%bind principal_id =
    required payload "principal_id" (protocol P.Id.Principal.of_json)
  in
  let%bind revision = required payload "revision" F.decimal in
  let%bind epochs_json = required payload "epochs" F.array in
  let%bind () =
    if List.is_empty epochs_json || List.length epochs_json > max_epochs
    then corrupt "developer run epoch count exceeds its bound"
    else Ok ()
  in
  let%bind epochs =
    List.map epochs_json ~f:(fun json ->
      poll ();
      epoch_of_json json)
    |> Result.all
  in
  let%bind invocations = required payload "invocations" F.array in
  let%bind jobs = required payload "jobs" F.array in
  let%bind last_inference_ordinal = required payload "last_inference_ordinal" F.decimal in
  let%bind inference_attempts = required payload "inference_attempts" F.array in
  let%bind () =
    if
      List.length invocations > max_invocations
      || List.length jobs > max_jobs
      || List.length inference_attempts > max_inference_attempts
    then corrupt "developer run member count exceeds limit"
    else Ok ()
  in
  let%bind _payload_bytes =
    List.fold_result jobs ~init:0 ~f:(fun accumulated job_json ->
      poll ();
      let%bind payload = required job_json "payload" (fun value -> Ok value) in
      let%bind length =
        D.Json.validate_and_measure_with_checkpoint ~limits ~checkpoint:poll payload
        |> F.store
      in
      if length > max_job_payload_bytes - accumulated
      then corrupt "developer job payload bytes exceed limit"
      else Ok (accumulated + length))
  in
  let%bind invocations =
    List.map invocations ~f:(fun json ->
      poll ();
      invocation_entry_of_json json)
    |> Result.all
  in
  let%bind jobs =
    List.map jobs ~f:(fun json ->
      poll ();
      protocol P.Developer_job.of_json json |> F.store)
    |> Result.all
  in
  let%bind inference_entries =
    List.map inference_attempts ~f:(fun json ->
      poll ();
      Inference_entry.of_json json ~limits)
    |> Result.all
  in
  let inference_bytes =
    List.sum (module Int) inference_entries ~f:Inference_entry.encoded_bytes
  in
  let%bind command_json = required payload "commands" F.array in
  let%bind () =
    if List.length command_json > Developer_command_limits.max_commands commands
    then corrupt "developer command count exceeds limit"
    else Ok ()
  in
  let%bind command_rows =
    List.map command_json ~f:(fun json ->
      poll ();
      Command_row.of_json json ~limits ~poll)
    |> Result.all
    |> Result.bind ~f:(fun rows ->
      Command_rows.of_rows rows ~commands ~poll ~check_owner:(fun _ -> Ok ()))
  in
  let t =
    { host_id
    ; run_id
    ; principal_id
    ; revision
    ; epochs
    ; invocations
    ; jobs
    ; last_inference_ordinal
    ; inference_entries
    ; inference_bytes
    ; command_rows
    }
  in
  let%bind recovery =
    validate
      t
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  in
  let%map () =
    if Int64.equal recovery.revision t.revision
    then Ok ()
    else (
      let%bind payload = to_payload recovery ~poll in
      D.Document.create_with_checkpoint ~limits ~checkpoint:poll ~kind ~version ~payload
      |> F.store
      |> Result.map ~f:(fun _ -> ()))
  in
  t
;;

let of_bytes_with_document
      bytes
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  let open Result.Let_syntax in
  let%bind document = D.Document.decode ~limits bytes |> F.store in
  let%map t =
    of_decoded_document
      document
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  in
  t, document
;;

let of_bytes
      bytes
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  Result.map
    (of_bytes_with_document
       bytes
       ~poll
       ~limits
       ~max_epochs
       ~max_jobs
       ~max_invocations
       ~max_job_payload_bytes
       ~max_inference_attempts
       ~max_inference_bytes
       ~commands)
    ~f:fst
;;
