open Core
module P = Agent_protocol

let invalid message = Error (P.Error.create Journal_corrupt ~message ~retryable:false ())

let invocation invocations id =
  match
    List.find invocations ~f:(fun i ->
      P.Id.Invocation.compare i.P.Invocation.context.id id = 0)
  with
  | Some value -> Ok value
  | None -> invalid "extension references an unknown invocation"
;;

let subscription subscriptions id =
  match
    List.find subscriptions ~f:(fun s ->
      P.Id.Subscription.compare s.P.Subscription.context.id id = 0)
  with
  | Some value -> Ok value
  | None -> invalid "extension references an unknown subscription"
;;

let owner ~session_id ~generation actual_session actual_generation =
  if P.Id.Session.compare session_id actual_session = 0 && generation = actual_generation
  then Ok ()
  else invalid "extension reference crosses session ownership or generation"
;;

let invocation_event_owner ~events (invocation : P.Invocation.t) =
  let open Result.Let_syntax in
  match invocation.parent_event with
  | None -> Ok ()
  | Some id ->
    let%bind event =
      match
        List.find events ~f:(fun event ->
          P.Id.Moderator_execution.equal event.P.Moderator_execution.context.id id)
      with
      | Some event -> Ok event
      | None -> invalid "invocation references an unknown moderator event"
    in
    let%bind () =
      owner
        ~session_id:invocation.context.session_id
        ~generation:invocation.context.generation
        event.context.session_id
        event.context.generation
    in
    (match invocation.observation with
     | Some observation
       when P.Invocation.equal_observer event.context.source observation.observer -> Ok ()
     | _ -> invalid "invocation observer differs from its parent event source")
;;

let job jobs id =
  match List.find jobs ~f:(fun j -> P.Id.Job.compare j.P.Job.id id = 0) with
  | Some value -> Ok value
  | None -> invalid "extension references an unknown job"
;;

let work_owner ~jobs ~subscriptions ~session_id ~generation = function
  | P.Invocation.Job id ->
    let open Result.Let_syntax in
    let%bind j = job jobs id in
    owner ~session_id ~generation j.session_id j.generation
  | Subscription id ->
    let open Result.Let_syntax in
    let%bind s = subscription subscriptions id in
    owner ~session_id ~generation s.context.session_id s.context.generation
;;

let completion_for_work ~jobs ~subscriptions = function
  | P.Invocation.Subscription id ->
    let open Result.Let_syntax in
    let%bind s = subscription subscriptions id in
    (match s.result with
     | Some result -> Ok (P.Stored_completion.Inline result)
     | None -> invalid "subscription is not terminal")
  | Job id ->
    let open Result.Let_syntax in
    let%bind j = job jobs id in
    let%bind completion =
      P.Job.terminal_result j
      |> Result.map_error ~f:(fun error ->
        P.Error.create
          Journal_corrupt
          ~message:("invalid job completion: " ^ error.message)
          ~retryable:false
          ())
    in
    (match completion with
     | Some completion -> Ok completion
     | None -> invalid "delivery references nonterminal job")
;;

let delivery_ready ~invocations ~jobs ~events delivery =
  match Notification_readiness.check ~invocations ~jobs ~events delivery with
  | Ok () -> Ok ()
  | Error (Waiting message | Rejected message) -> invalid message
;;

let validate
      ~session_id
      ~generation
      ~invocations
      ~subscriptions
      ~deliveries
      ~jobs
      ~schedules
      ~events
  =
  let open Result.Let_syntax in
  let seen_occurrences = Hash_set.create (module P.History.Id) in
  let%bind () =
    List.fold_result invocations ~init:() ~f:(fun () (i : P.Invocation.t) ->
      let unique seen = function
        | None -> Ok ()
        | Some id ->
          if Hash_set.mem seen id
          then invalid "canonical occurrence is claimed by multiple invocations"
          else (
            Hash_set.add seen id;
            Ok ())
      in
      let%bind () = unique seen_occurrences i.context.call_entry_id in
      unique seen_occurrences i.output_entry_id)
  in
  let seen_subscriptions = Hash_set.create (module P.Id.Subscription) in
  let%bind () =
    List.fold_result subscriptions ~init:() ~f:(fun () (s : P.Subscription.t) ->
      let c = s.context in
      let%bind () = P.Subscription.validate s in
      let%bind () =
        owner ~session_id ~generation:c.generation c.session_id c.generation
      in
      if c.generation > generation || Hash_set.mem seen_subscriptions c.id
      then invalid "subscription generation or uniqueness is invalid"
      else (
        Hash_set.add seen_subscriptions c.id;
        let%bind i = invocation invocations c.invocation_id in
        let%bind () =
          match i.status with
          | P.Invocation.Admitted | Dispatching -> Ok ()
          | Resolved (Pending (Subscription id, _))
          | Published (Pending (Subscription id, _))
            when P.Id.Subscription.compare id c.id = 0 -> Ok ()
          | (Resolved (Fail _ | Cancelled _) | Published (Fail _ | Cancelled _))
            when Option.is_some s.result -> Ok ()
          | _ -> invalid "subscription is not acknowledged by its originating invocation"
        in
        let%bind () =
          owner
            ~session_id:c.session_id
            ~generation:c.generation
            i.context.session_id
            i.context.generation
        in
        let%bind () =
          match s.job_id with
          | None -> Ok ()
          | Some id ->
            work_owner
              ~jobs
              ~subscriptions
              ~session_id:c.session_id
              ~generation:c.generation
              (Job id)
        in
        match s.timer_id with
        | None -> Ok ()
        | Some id ->
          (match
             List.find schedules ~f:(fun timer ->
               P.Id.Schedule.compare timer.P.Schedule.id id = 0)
           with
           | None -> invalid "subscription references an unknown timer"
           | Some timer ->
             let%bind () =
               owner
                 ~session_id:c.session_id
                 ~generation:c.generation
                 timer.session_id
                 timer.generation
             in
             (match timer.ownership with
              | None -> Ok ()
              | Some ownership ->
                (match c.source, ownership.subscription with
                 | Some source, Some (id, epoch)
                   when P.Invocation.equal_observer source ownership.source
                        && P.Id.Subscription.equal id c.id
                        && Int.equal epoch s.epoch -> Ok ()
                 | _ -> invalid "subscription timer has a different source or epoch")))))
  in
  let%bind () =
    List.fold_result invocations ~init:() ~f:(fun () (i : P.Invocation.t) ->
      match i.status with
      | Resolved (Pending (work, _)) | Published (Pending (work, _)) ->
        let%bind () =
          work_owner
            ~jobs
            ~subscriptions
            ~session_id:i.context.session_id
            ~generation:i.context.generation
            work
        in
        (match work with
         | Job id ->
           let%bind job = job jobs id in
           (match job.launch with
            | None -> Ok ()
            | Some { owner = Invocation owner; _ }
              when P.Id.Invocation.equal owner i.context.id -> Ok ()
            | Some _ -> invalid "pending job belongs to another launch owner")
         | Subscription id ->
           let%bind s = subscription subscriptions id in
           if P.Id.Invocation.compare i.context.id s.context.invocation_id = 0
           then Ok ()
           else invalid "pending subscription belongs to another invocation")
      | _ -> Ok ())
  in
  let seen_deliveries = Hash_set.create (module P.Id.Delivery) in
  let seen_work = ref [] in
  let%bind () =
    List.fold_result deliveries ~init:() ~f:(fun () (d : P.Delivery.t) ->
      let c = d.context in
      let%bind () = P.Delivery.validate d in
      let%bind () =
        owner ~session_id ~generation:c.generation c.session_id c.generation
      in
      if c.generation > generation || Hash_set.mem seen_deliveries c.id
      then invalid "delivery generation or uniqueness is invalid"
      else (
        Hash_set.add seen_deliveries c.id;
        let%bind () =
          match c.invocation_id with
          | None -> Ok ()
          | Some id ->
            let%bind i = invocation invocations id in
            owner
              ~session_id:c.session_id
              ~generation:c.generation
              i.context.session_id
              i.context.generation
        in
        let%bind () =
          match c.work with
          | None ->
            if P.Delivery.equal_source c.source Moderator
            then Ok ()
            else invalid "completion adapter needs an owned work reference"
          | Some work ->
            let%bind () =
              work_owner
                ~jobs
                ~subscriptions
                ~session_id:c.session_id
                ~generation:c.generation
                work
            in
            let%bind result = completion_for_work ~jobs ~subscriptions work in
            let%bind matches = P.Stored_completion.matches result c.completion in
            if not matches
            then invalid "delivery result differs from its terminal work"
            else if
              List.exists !seen_work ~f:(fun old ->
                P.Invocation.compare_work old work = 0)
            then invalid "terminal work has multiple delivery owners"
            else (
              seen_work := work :: !seen_work;
              match work with
              | Job _ ->
                if P.Delivery.equal_source c.source External_ingress
                then invalid "external ingress cannot own native job completion"
                else Ok ()
              | Subscription id ->
                let%bind s = subscription subscriptions id in
                if
                  not
                    (Option.value_map
                       c.invocation_id
                       ~default:false
                       ~f:(fun invocation_id ->
                         P.Id.Invocation.compare invocation_id s.context.invocation_id = 0))
                then
                  invalid "subscription delivery must retain its originating invocation"
                else if P.Delivery.equal_source c.source Job_adapter
                then invalid "job adapter cannot own subscription completion"
                else if
                  P.Delivery.equal_source c.source External_ingress
                  && Option.is_none s.context.ingress_capability
                then invalid "subscription has no external ingress capability"
                else Ok ())
        in
        match d.status with
        | Committed _ -> delivery_ready ~invocations ~jobs ~events d
        | Pending | Failed _ -> Ok ()))
  in
  () |> Result.return
;;
