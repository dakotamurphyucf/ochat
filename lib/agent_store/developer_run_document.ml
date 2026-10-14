open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

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
  }

let kind = "store.developer_run"
let version = 1
let ( let* ) result f = Result.bind result ~f
let corrupt reason = Error (Store_error.Corrupt reason)
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
    Option.is_none (P.Developer_job.Launch.parent_job launch)
    && Int.equal (P.Developer_job.Launch.nested_depth launch) 0
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
        then corrupt "external model root launch has job ancestry or depth"
        else Ok ()
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
        D.Json.validate_and_measure ~limits (P.Developer_job.payload job) |> F.store
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
    ]
;;

let validate t ~poll ~limits ~max_epochs ~max_jobs ~max_invocations ~max_job_payload_bytes
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
  let%map _payload_bytes =
    List.fold_result t.jobs ~init:0 ~f:(fun accumulated job ->
      poll ();
      let%bind length =
        D.Json.validate_and_measure ~limits (P.Developer_job.payload job) |> F.store
      in
      if length > max_job_payload_bytes - accumulated
      then corrupt "developer job payload bytes exceed limit"
      else Ok (accumulated + length))
  in
  ()
;;

let to_bytes t ~poll ~limits ~max_epochs ~max_jobs ~max_invocations ~max_job_payload_bytes
  =
  let open Result.Let_syntax in
  let%bind () =
    validate t ~poll ~limits ~max_epochs ~max_jobs ~max_invocations ~max_job_payload_bytes
  in
  let%bind payload = to_payload t ~poll in
  let%map document = D.Document.create ~limits ~kind ~version ~payload |> F.store in
  D.Document.to_string document
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

let of_bytes_with_document
      bytes
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
  =
  let open Result.Let_syntax in
  let%bind document = D.Document.decode ~limits bytes |> F.store in
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
      [ "host_id"; "run_id"; "principal_id"; "revision"; "epochs"; "invocations"; "jobs" ]
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
  let%bind () =
    if List.length invocations > max_invocations || List.length jobs > max_jobs
    then corrupt "developer run member count exceeds limit"
    else Ok ()
  in
  let%bind _payload_bytes =
    List.fold_result jobs ~init:0 ~f:(fun accumulated job_json ->
      poll ();
      let%bind payload = required job_json "payload" (fun value -> Ok value) in
      let%bind length = D.Json.validate_and_measure ~limits payload |> F.store in
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
  let t = { host_id; run_id; principal_id; revision; epochs; invocations; jobs } in
  let%map () =
    validate t ~poll ~limits ~max_epochs ~max_jobs ~max_invocations ~max_job_payload_bytes
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
  =
  Result.map
    (of_bytes_with_document
       bytes
       ~poll
       ~limits
       ~max_epochs
       ~max_jobs
       ~max_invocations
       ~max_job_payload_bytes)
    ~f:fst
;;
