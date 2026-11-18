open! Core
module P = Agent_protocol
module Cause = Policy_cause_data
module Data = Policy_canonical_usage_data
module Carrier = Policy_canonical_usage_carrier
module Member = Cause.Member
module Context = Cause.Context
module Native_prefix = Policy_native_ancestry

type t = Carrier.t
type accounting = t

let invalid = Data.invalid

module Standalone_data = Policy_standalone_source

module Boundary = struct
  type t =
    | Event of
        { event : P.Moderator_execution.t
        ; descriptor : Standalone_data.t
        }
    | Invocation of
        { invocation : P.Invocation.t
        ; descriptor : Standalone_data.t
        }
    | Job of
        { job : P.Job.t
        ; descriptor : Standalone_data.t
        }
end

module Resolution = struct
  type t =
    | Unrelated
    | Standalone_unrelated of Standalone_data.t
    | Caused of Cause.t
end

module Birth = struct
  type t =
    { update : Policy_canonical_usage_update.Update.t option
    ; classification : Standalone_data.Classification.t
    }

  let update t = t.update
  let classification t = t.classification
end

(* Event and Job execution-source fields retain only an observer, not an
   installation epoch. Invocation observation observers are deferred sinks,
   preserved as DATA rather than treated as execution-source evidence. Full
   epochs are checked where the retained Cause or descriptor records one. *)
let check_observer (prefix : Native_prefix.t) observer =
  match observer with
  | None -> Ok ()
  | Some observer ->
    if
      Option.exists prefix.current_source ~f:(fun source ->
        P.Invocation.equal_observer source.P.Run_source.observer observer)
    then Ok ()
    else invalid "policy native observer differs from current source"
;;

let job_observer (row : P.Job.t) =
  Option.bind row.launch ~f:(fun launch -> launch.P.Job.moderator_source)
;;

let validate_descriptor_source (prefix : Native_prefix.t) (descriptor : Standalone_data.t)
  =
  let open Result.Let_syntax in
  let%bind () =
    Native_prefix.check_owner
      prefix
      ~session_id:descriptor.session_id
      ~generation:descriptor.generation
  in
  match descriptor.source.installation, prefix.current_source with
  | Some captured, Some current
    when P.Run_source.equal captured current
         && Option.exists
              descriptor.source.observer
              ~f:(P.Invocation.equal_observer current.observer) -> Ok ()
  | None, _ | Some _, None | Some _, Some _ ->
    invalid "standalone native boundary requires its exact current full source"
;;

let require_origin actual expected =
  if Option.exists actual ~f:(P.Inference_origin.equal expected)
  then Ok ()
  else invalid "standalone native origin differs from its actual subject"
;;

let validate_event_origin prefix (event : P.Moderator_execution.t) expected =
  match event.queue_occurrence, event.context.job with
  | Some occurrence, None ->
    require_origin (P.Moderator_execution.Queue_occurrence.origin occurrence) expected
  | None, Some parent ->
    let%bind.Result job = Native_prefix.job prefix (P.Id.Job.to_string parent.job_id) in
    require_origin job.inference_origin expected
  | None, None ->
    (* A delegated receipt has no native origin field. Its actual claim issuer
       froze this fact from the prepared candidate/Run index at SAME ACK. An
       ordinary unclassified Event cannot become a positive exception here. *)
    if Option.is_some event.delegation
    then Ok ()
    else invalid "standalone Event has no admitted delegated or producer boundary"
  | Some _, Some _ -> invalid "standalone Event has contradictory native parents"
;;

let validate_boundary accounting ~(prefix : Native_prefix.t) boundary =
  let open Result.Let_syntax in
  let%bind member, descriptor =
    match boundary with
    | Boundary.Event { event; descriptor } ->
      let%bind actual =
        Native_prefix.event prefix (P.Id.Moderator_execution.to_string event.context.id)
      in
      let%bind () =
        if
          Jsonaf.exactly_equal
            (P.Moderator_execution.to_json event)
            (P.Moderator_execution.to_json actual)
        then Ok ()
        else invalid "standalone Event is not the actual retained native row"
      in
      let%bind () = check_observer prefix (Some event.context.source) in
      let%bind () = validate_event_origin prefix event descriptor.origin in
      let%map member =
        Member.event event.context.id ~generation:event.context.generation
      in
      member, descriptor
    | Boundary.Invocation { invocation; descriptor } ->
      let%bind actual =
        Native_prefix.invocation prefix (P.Id.Invocation.to_string invocation.context.id)
      in
      let%bind () =
        if P.Invocation.equal invocation actual
        then Ok ()
        else invalid "standalone Invocation is not the actual retained native row"
      in
      let%bind () = require_origin invocation.inference_origin descriptor.origin in
      let%map member =
        Member.invocation invocation.context.id ~generation:invocation.context.generation
      in
      member, descriptor
    | Boundary.Job { job; descriptor } ->
      let%bind actual = Native_prefix.job prefix (P.Id.Job.to_string job.id) in
      let%bind () =
        if Jsonaf.exactly_equal (P.Job.to_json job) (P.Job.to_json actual)
        then Ok ()
        else invalid "standalone Job is not the actual retained native row"
      in
      let%bind () = check_observer prefix (job_observer job) in
      let%bind () = require_origin job.inference_origin descriptor.origin in
      let%map member = Native_prefix.member_of_job job in
      member, descriptor
  in
  let%bind () = validate_descriptor_source prefix descriptor in
  let%bind () =
    match descriptor.subject with
    | Member actual when Member.equal actual member -> Ok ()
    | Member _ | Root_operation _ -> invalid "standalone native boundary subject differs"
  in
  let%bind () =
    match accounting with
    | None -> Ok ()
    | Some accounting ->
      if Option.is_none (Data.find_member (Carrier.value accounting) member)
      then Ok ()
      else invalid "standalone boundary contradicts retained caused membership"
  in
  Ok (member, descriptor)
;;

(* Absence is factual unavailability only. A present canonical record must still
   validate its exact kind, Cause chain, terminal occurrence and installation. *)
let find_retained_queue_cause accounting ~(prefix : Native_prefix.t) queued =
  let open Result.Let_syntax in
  match accounting with
  | None -> Ok None
  | Some accounting ->
    let data = Carrier.value accounting in
    let%bind selected =
      match
        List.filter data.memberships ~f:(fun (row : Data.Membership.t) ->
          String.equal
            (Member.id row.member)
            (P.Id.Job.to_string (Native_prefix.Queued_job.id queued))
          && Int.equal
               (Member.generation row.member)
               (Native_prefix.Queued_job.generation queued))
      with
      | [] -> Ok None
      | [ row ] ->
        (match Member.kind row.member with
         | Model_job | Async_job -> Ok (Some row)
         | Event | Invocation -> invalid "retained queue producer membership is not a Job")
      | _ :: _ -> invalid "retained queue producer has no unique canonical Job membership"
    in
    (match selected with
     | None -> Ok None
     | Some selected ->
       let%bind cause =
         Data.find_cause data selected.cause
         |> Result.of_option
              ~error:(P.Error.invalid_request "retained queue producer Cause is absent")
       in
       let%bind member =
         Data.Source_work.retained_queued_parent
           ~memberships:data.memberships
           cause
           queued
       in
       let%map () =
         if
           Option.exists
             prefix.current_source
             ~f:(P.Run_source.equal (Cause.callee_source cause))
         then Ok ()
         else invalid "retained queued policy Cause is not the current installation"
       in
       Some (accounting, cause, member))
;;

let cause_of_retained_queue accounting ~prefix queued =
  let%bind.Result selected = find_retained_queue_cause accounting ~prefix queued in
  Result.of_option
    selected
    ~error:
      (P.Error.invalid_request
         (match accounting with
          | None -> "retained queued policy Job has no accounting history"
          | Some _ -> "retained queue producer has no unique canonical Job membership"))
;;

(* An absent component is not an ordinary-root proof. Inspect the exact named
   native chain first; a delegated seed or child of retained caused work must
   have its own membership. The complete Context is unnecessary for this query. *)
let resolve ?boundary accounting ~(prefix : Native_prefix.t) ~member =
  let open Result.Let_syntax in
  let data = Option.map accounting ~f:Carrier.value in
  let%bind () =
    match data with
    | None -> Ok ()
    | Some data ->
      if P.Id.Session.equal prefix.session_id (P.Session_ref.session_id data.callee)
      then Ok ()
      else invalid "policy source carrier belongs to another actual session"
  in
  let%bind boundary =
    match boundary with
    | None -> Ok None
    | Some boundary ->
      validate_boundary accounting ~prefix boundary |> Result.map ~f:Option.some
  in
  let rec visit seen ~unregistered_child member =
    let%bind () =
      if Int.equal (Member.generation member) prefix.generation
      then Ok ()
      else invalid "policy source member generation changed"
    in
    let key = Member.key member in
    let%bind seen =
      if Set.mem seen key
      then invalid "policy source ancestry contains a cycle"
      else Ok (Set.add seen key)
    in
    match Option.bind data ~f:(fun data -> Data.find_member data member) with
    | Some retained ->
      let%bind actual_data =
        Result.of_option
          data
          ~error:(P.Error.invalid_request "retained policy carrier is absent")
      in
      let%bind cause =
        Option.bind data ~f:(fun data -> Data.find_cause data retained.cause)
        |> Result.of_option
             ~error:(P.Error.invalid_request "policy source Cause is absent")
      in
      let%bind () =
        if
          Option.exists
            prefix.current_source
            ~f:(P.Run_source.equal (Cause.callee_source cause))
        then Ok ()
        else invalid "policy source Cause is not the current installation"
      in
      let%bind actual =
        Data.Source_work.capture_native
          ~memberships:actual_data.memberships
          prefix
          cause
          member
      in
      let%bind () =
        match actual with
        | Some actual when Data.Source_work.matches retained actual -> Ok ()
        | Some _ | None ->
          invalid "policy source member differs from its actual native work"
      in
      if unregistered_child
      then invalid "caused native work lost its birth membership"
      else Ok (Resolution.Caused cause)
    | None ->
      let%bind parent =
        match Member.kind member with
        | Event ->
          let%bind row = Native_prefix.event prefix (Member.id member) in
          let%bind () =
            Native_prefix.check_owner
              prefix
              ~session_id:row.context.session_id
              ~generation:row.context.generation
          in
          let%bind () = check_observer prefix (Some row.context.source) in
          let%bind ancestry = Native_prefix.event_ancestry prefix row in
          (match ancestry with
           | Root -> Ok None
           | Native parent -> Ok (Some parent)
           | Retained_job queued ->
             (match boundary with
              | Some (selected, _) when Member.equal selected member -> Ok None
              | Some _ | None ->
                let%map _, _, parent =
                  cause_of_retained_queue accounting ~prefix queued
                in
                Some parent)
           | Unavailable ->
             (match boundary with
              | Some (selected, _) when Member.equal selected member -> Ok None
              | Some _ | None -> invalid "queued policy producer history is unavailable"))
        | Invocation ->
          let%bind row = Native_prefix.invocation prefix (Member.id member) in
          let%bind () =
            Native_prefix.check_owner
              prefix
              ~session_id:row.context.session_id
              ~generation:row.context.generation
          in
          Native_prefix.invocation_parent prefix row
        | Model_job | Async_job ->
          let%bind row = Native_prefix.job prefix (Member.id member) in
          let%bind actual = Native_prefix.member_of_job row in
          let%bind () =
            if Member.equal member actual
            then Ok ()
            else invalid "policy source member differs from actual native Job kind"
          in
          let%bind () = check_observer prefix (job_observer row) in
          Native_prefix.job_parent prefix row
      in
      (match boundary with
       | Some (selected, descriptor) when Member.equal selected member ->
         Ok (Resolution.Standalone_unrelated descriptor)
       | Some _ | None ->
         let%bind () =
           match Member.kind member with
           | Event ->
             let%bind row = Native_prefix.event prefix (Member.id member) in
             if Option.is_some row.delegation
             then invalid "delegated policy seed has no retained Cause membership"
             else Ok ()
           | Invocation | Model_job | Async_job -> Ok ()
         in
         (match parent with
          | None -> Ok Resolution.Unrelated
          | Some parent -> visit seen ~unregistered_child:true parent))
  in
  visit String.Set.empty ~unregistered_child:false member
;;

let cause accounting ~prefix ~member =
  let%bind.Result resolution = resolve accounting ~prefix ~member in
  match resolution with
  | Resolution.Unrelated -> Ok None
  | Caused cause -> Ok (Some cause)
  | Standalone_unrelated _ ->
    invalid "public source query cannot issue a standalone boundary"
;;

let with_rows (context : Context.t) ~events ~invocations ~jobs =
  Context.create
    ~session_id:context.session_id
    ~generation:context.generation
    ~current_source:context.current_source
    ~events
    ~invocations
    ~jobs
    ~runs:context.runs
    ~accounts:context.accounts
    ~catalog:context.catalog
    ~descendants:context.descendants
;;

let register ?boundary accounting ~prefix ~before ~after ~parent ~member ~origin =
  let open Result.Let_syntax in
  let%bind inherited =
    Option.value_map parent ~default:(Ok Resolution.Unrelated) ~f:(fun member ->
      resolve ?boundary accounting ~prefix ~member)
  in
  match inherited, accounting with
  | Resolution.Unrelated, _ -> Ok { Birth.update = None; classification = Unknown }
  | Standalone_unrelated descriptor, _ ->
    let%bind () = require_origin origin descriptor.origin in
    let%map descriptor =
      Standalone_data.create
        ~session_id:prefix.session_id
        ~generation:prefix.generation
        ~subject:(Member member)
        ~origin:descriptor.origin
        ~source:descriptor.source
    in
    { Birth.update = None; classification = Standalone_unrelated descriptor }
  | Caused cause, Some accounting ->
    (* These lists belong to the same immutable Actor prefix. Complete inventory
       validation is paid only for an actual caused missing-to-present birth. *)
    let%bind before = before () in
    let%bind () = Native_prefix.check_context prefix before in
    let%bind after = after before in
    Policy_canonical_usage_update.register_work
      accounting
      ~before
      ~after
      ~cause:(Cause.key cause)
      ~member
    |> Result.map ~f:(fun update ->
      { Birth.update = Some update; classification = Unknown })
  | Caused _, None -> invalid "caused native work has no actual accounting carrier"
;;

let invocation_birth_internal
      ?boundary
      accounting
      ~prefix
      ~before
      (invocation : P.Invocation.t)
  =
  let open Result.Let_syntax in
  let c = invocation.context in
  let%bind () =
    Native_prefix.check_owner prefix ~session_id:c.session_id ~generation:c.generation
  in
  let%bind () = P.Invocation.validate invocation in
  let%bind parent = Native_prefix.invocation_parent prefix invocation in
  let%bind member = Member.invocation c.id ~generation:c.generation in
  register
    ?boundary
    accounting
    ~prefix
    ~before
    ~parent
    ~member
    ~origin:invocation.inference_origin
    ~after:(fun before ->
      with_rows
        before
        ~events:before.events
        ~invocations:(invocation :: before.invocations)
        ~jobs:before.jobs)
;;

let job_birth ?boundary accounting ~prefix ~before ~parent (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind () =
    Native_prefix.check_owner prefix ~session_id:job.session_id ~generation:job.generation
  in
  let%bind _ = P.Job.of_json (P.Job.to_json job) in
  let%bind () = check_observer prefix (job_observer job) in
  let%bind actual_parent = Native_prefix.job_parent prefix job in
  let%bind () =
    if Option.equal Member.equal parent actual_parent
    then Ok ()
    else invalid "policy Job immediate producer differs from native birth parent"
  in
  let%bind member = Native_prefix.member_of_job job in
  register
    ?boundary
    accounting
    ~prefix
    ~before
    ~parent
    ~member
    ~origin:job.inference_origin
    ~after:(fun before ->
      with_rows
        before
        ~events:before.events
        ~invocations:before.invocations
        ~jobs:(job :: before.jobs))
;;

let model_job_birth_internal ?boundary accounting ~prefix ~before ~parent (job : P.Job.t) =
  if P.Job.equal_kind job.kind Model_call
  then job_birth ?boundary accounting ~prefix ~before ~parent job
  else invalid "Model birth requires actual Model_call Job"
;;

let async_job_birth_internal ?boundary accounting ~prefix ~before (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind () =
    if P.Job.equal_kind job.kind Async_tool
    then Ok ()
    else invalid "Async birth requires actual Async_tool Job"
  in
  let%bind parent = Native_prefix.job_parent prefix job in
  job_birth ?boundary accounting ~prefix ~before ~parent job
;;

let job_event_birth_internal
      ?boundary
      accounting
      ~prefix
      ~before
      (event : P.Moderator_execution.t)
  =
  let open Result.Let_syntax in
  let c = event.context in
  let%bind () =
    Native_prefix.check_owner prefix ~session_id:c.session_id ~generation:c.generation
  in
  let%bind _ = P.Moderator_execution.of_json (P.Moderator_execution.to_json event) in
  let%bind () = check_observer prefix (Some c.source) in
  let%bind parent =
    match c.job with
    | None -> invalid "Job event birth requires its actual Job occurrence"
    | Some owner ->
      let%bind job = Native_prefix.job prefix (P.Id.Job.to_string owner.job_id) in
      let%bind () =
        if Int.equal owner.attempt job.attempt && owner.attempt > 0
        then Ok ()
        else invalid "new policy Job event cannot borrow an obsolete attempt"
      in
      Native_prefix.member_of_job job
  in
  let%bind member = Member.event c.id ~generation:c.generation in
  let%bind job = Native_prefix.job prefix (Member.id parent) in
  register
    ?boundary
    accounting
    ~prefix
    ~before
    ~parent:(Some parent)
    ~member
    ~origin:job.inference_origin
    ~after:(fun before ->
      with_rows
        before
        ~events:(event :: before.events)
        ~invocations:before.invocations
        ~jobs:before.jobs)
;;

let queued_event_birth_internal
      accounting
      ~prefix
      ~before
      (event : P.Moderator_execution.t)
  =
  let open Result.Let_syntax in
  let c = event.context in
  let%bind () =
    Native_prefix.check_owner prefix ~session_id:c.session_id ~generation:c.generation
  in
  let%bind _ = P.Moderator_execution.of_json (P.Moderator_execution.to_json event) in
  let%bind () = check_observer prefix (Some c.source) in
  let%bind occurrence =
    Result.of_option
      event.queue_occurrence
      ~error:
        (P.Error.invalid_request "queued policy birth requires its consumed occurrence")
  in
  let%bind () =
    if
      Option.exists
        prefix.current_source
        ~f:(P.Run_source.equal (P.Moderator_execution.Queue_occurrence.source occurrence))
    then Ok ()
    else invalid "queued policy birth source is not the current installation"
  in
  let%bind ancestry = Native_prefix.event_ancestry prefix event in
  let%bind member = Member.event c.id ~generation:c.generation in
  let after before =
    with_rows
      before
      ~events:(event :: before.Context.events)
      ~invocations:before.invocations
      ~jobs:before.jobs
  in
  match ancestry with
  | Root -> Ok { Birth.update = None; classification = Unknown }
  | Unavailable ->
    (* Preserve truthful unavailable historical callback facts. This is not
       unrelatedness; any later new-work Source query still rejects. *)
    Ok { Birth.update = None; classification = Unknown }
  | Native parent ->
    register
      accounting
      ~prefix
      ~before
      ~after
      ~parent:(Some parent)
      ~member
      ~origin:(P.Moderator_execution.Queue_occurrence.origin occurrence)
  | Retained_job queued ->
    let%bind selected = find_retained_queue_cause accounting ~prefix queued in
    (match selected with
     | None -> Ok { Birth.update = None; classification = Unknown }
     | Some (accounting, cause, _parent) ->
       let%bind before = before () in
       let%bind () = Native_prefix.check_context prefix before in
       let%bind after = after before in
       let%map update =
         Policy_canonical_usage_update.register_work
           accounting
           ~before
           ~after
           ~cause:(Cause.key cause)
           ~member
       in
       { Birth.update = Some update; classification = Unknown })
;;

let queued_event_birth accounting ~prefix ~before event =
  queued_event_birth_internal accounting ~prefix ~before event
  |> Result.map ~f:Birth.update
;;

let invocation_birth accounting ~prefix ~before invocation =
  invocation_birth_internal accounting ~prefix ~before invocation
  |> Result.map ~f:Birth.update
;;

let model_job_birth accounting ~prefix ~before ~parent job =
  model_job_birth_internal accounting ~prefix ~before ~parent job
  |> Result.map ~f:Birth.update
;;

let async_job_birth accounting ~prefix ~before job =
  async_job_birth_internal accounting ~prefix ~before job |> Result.map ~f:Birth.update
;;

let job_event_birth accounting ~prefix ~before event =
  job_event_birth_internal accounting ~prefix ~before event |> Result.map ~f:Birth.update
;;

module Standalone = struct
  module Boundary = Boundary
  module Resolution = Resolution
  module Birth = Birth

  let resolve accounting ~prefix ~member ~boundary =
    resolve ~boundary accounting ~prefix ~member
  ;;

  let invocation_birth accounting ~prefix ~before ~boundary invocation =
    invocation_birth_internal ~boundary accounting ~prefix ~before invocation
  ;;

  let model_job_birth accounting ~prefix ~before ~parent ~boundary job =
    model_job_birth_internal ~boundary accounting ~prefix ~before ~parent job
  ;;

  let async_job_birth accounting ~prefix ~before ~boundary job =
    async_job_birth_internal ~boundary accounting ~prefix ~before job
  ;;

  let job_event_birth accounting ~prefix ~before ~boundary event =
    job_event_birth_internal ~boundary accounting ~prefix ~before event
  ;;
end
