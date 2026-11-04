open! Core
module P = Agent_protocol
module R = Recurrence_rules
module F = Recurrence_definition
module I = Recurrence_identity
module N = Recurrence_nominal
module V = Recurrence_frontier_records
module S = V.State
module C = Recurrence_cursor

let protected_rows (s : S.t) = S.claimed_nominals s
let protected_count (s : S.t) = S.unresolved_count s
let pending_count (s : S.t) = S.pending_count s

let ready_capacity (s : S.t) definition =
  Int.max 0 (Recurrence_policy.max_pending (F.policy definition) - protected_count s)
;;

let invalid message = Error (P.Error.invalid_request message)
let value = V.Stored_nominal.value
let key row = N.key (value row)
let slot row = I.Occurrence.slot (key row)
let at row = N.at (value row)
let candidates (s : S.t) = Option.value_map s.batch ~default:[] ~f:(fun b -> b.candidates)

let find_mark (s : S.t) definition =
  match
    List.find s.marks ~f:(fun m -> I.Definition.equal m.definition (F.key definition))
  with
  | None -> invalid "frontier lacks immutable definition mark"
  | Some m when String.equal m.definition_sha256 (F.digest definition) -> Ok m
  | Some _ -> invalid "frontier definition digest changed"
;;

let update_mark (s : S.t) (mark : V.Mark.t) =
  { s with
    marks =
      List.map s.marks ~f:(fun old ->
        if I.Definition.equal old.definition mark.definition then mark else old)
  }
;;

let trim_history (s : S.t) definition =
  { s with
    history = List.take s.history (Recurrence_policy.max_history (F.policy definition))
  }
;;

let detail definition ~first_slot ~last_slot ~kind ~reason ~nominal =
  V.Detail.
    { definition = F.key definition
    ; first_slot
    ; last_slot
    ; kind
    ; reason
    ; nominal
    ; original_json = None
    }
;;

let omit s definition ~first_slot ~last_slot ~reason ~nominal =
  let open Result.Let_syntax in
  let row = detail definition ~first_slot ~last_slot ~kind:Skipped ~reason ~nominal in
  let%bind count = V.Detail.count row in
  let%bind mark = find_mark s definition in
  let%map omitted = V.add mark.omitted count in
  let s = update_mark s { mark with omitted } in
  trim_history { s with history = row :: s.history } definition
;;

let withdraw_rows s definition ~reason rows =
  List.fold_result rows ~init:s ~f:(fun s nominal ->
    let open Result.Let_syntax in
    let%bind mark = find_mark s definition in
    let%map withdrawn = V.add mark.withdrawn 1L in
    let row =
      detail
        definition
        ~first_slot:(slot nominal)
        ~last_slot:(slot nominal)
        ~kind:Withdrawn
        ~reason
        ~nominal:(Some nominal)
    in
    let s = update_mark s { mark with withdrawn } in
    trim_history { s with history = row :: s.history } definition)
;;

let archive (s : S.t) definition kind json =
  let detail = V.Control_detail.{ kind; json; original_json = None } in
  { s with
    control_history =
      List.take
        (detail :: s.control_history)
        (Recurrence_policy.max_history (F.policy definition))
  }
;;

let retire_alarm (s : S.t) ~definition =
  let s =
    match s.alarm with
    | Some alarm when V.Stored_alarm.has_extensions alarm ->
      archive s definition Retired_alarm (V.Stored_alarm.json alarm)
    | None | Some _ -> s
  in
  { s with alarm = None }
;;

let retire_batch (s : S.t) ~definition =
  let s =
    match s.batch with
    | Some batch when Recurrence_frontier_codec.batch_has_extensions batch ->
      archive s definition Retired_batch (Recurrence_frontier_codec.batch_json batch)
    | None | Some _ -> s
  in
  { s with batch = None }
;;

let withdraw (s : S.t) ~definition ~preserve_lookahead =
  let%map.Result s =
    withdraw_rows s definition ~reason:Control_withdrawal (s.ready @ candidates s)
  in
  let s = retire_alarm s ~definition |> fun s -> retire_batch s ~definition in
  { s with
    ready = []
  ; batch = None
  ; alarm = None
  ; lookahead = (if preserve_lookahead then s.lookahead else None)
  }
;;

let consume_range (s : S.t) ~definition ~first_slot ~last_slot ~last_at ~selected ~omitted
  =
  let open Result.Let_syntax in
  let%bind mark = find_mark s definition in
  let%bind expected = V.Mark.consumed mark in
  let intervals =
    List.map selected ~f:(fun row -> slot row, slot row)
    @ List.map omitted ~f:(fun (a, b, _) -> a, b)
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int64.compare a b)
  in
  let%bind count = V.add Int64.(last_slot - first_slot) 1L in
  let%bind () =
    if
      (not (Int64.equal expected first_slot))
      || Int64.(last_slot < first_slot || last_slot = max_value)
      || List.length selected > 32
    then invalid "consumed recurrence range does not continue high-water"
    else Ok ()
  in
  let%bind next =
    List.fold_result intervals ~init:first_slot ~f:(fun expected (a, b) ->
      if (not (Int64.equal a expected)) || Int64.(b < a || b > last_slot)
      then invalid "selected and omitted ranges do not partition consumption"
      else V.add b 1L)
  in
  let%bind () =
    if Int64.equal next Int64.(first_slot + count)
    then Ok ()
    else invalid "consumed recurrence partition has missing slots"
  in
  let%bind () =
    List.fold_result selected ~init:() ~f:(fun () row ->
      N.validate (value row) ~definition)
  in
  let s = update_mark s { mark with through = Some last_slot; last_at = Some last_at } in
  let%bind s =
    List.fold_result omitted ~init:s ~f:(fun s (first_slot, last_slot, reason) ->
      omit s definition ~first_slot ~last_slot ~reason ~nominal:None)
  in
  let%bind s =
    match Recurrence_policy.misfire (F.policy definition), selected with
    | Coalesce, _ :: _ ->
      let%map s = withdraw_rows s definition ~reason:Superseded s.ready in
      { s with ready = [] }
    | (Skip | Catch_up _), _ | Coalesce, [] -> Ok s
  in
  let ready = s.ready @ selected in
  if List.length ready > ready_capacity s definition
  then invalid "recurrence pending capacity exceeded"
  else Ok { (retire_alarm s ~definition) with ready; issue = None }
;;

let collect_calendar (s : S.t) ~definition ~nominal =
  let open Result.Let_syntax in
  match s.batch with
  | None -> invalid "calendar collection lacks captured cutoff"
  | Some batch ->
    let%bind () = N.validate (value nominal) ~definition in
    let%bind mark = find_mark s definition in
    let%bind expected = V.Mark.consumed mark in
    if
      (not (Int64.equal expected (slot nominal)))
      || Time_ns.(at nominal > batch.cutoff)
      || Int64.equal expected Int64.max_value
      || Option.exists mark.last_at ~f:(fun old -> Time_ns.(old >= at nominal))
    then invalid "calendar nominal does not continue captured batch"
    else (
      let s =
        update_mark s { mark with through = Some expected; last_at = Some (at nominal) }
      in
      let policy = F.policy definition in
      let lateness =
        Int64.(
          (Time_ns.to_int63_ns_since_epoch batch.cutoff |> Int63.to_int64)
          - (Time_ns.to_int63_ns_since_epoch (at nominal) |> Int63.to_int64))
      in
      let add_omission s reason =
        omit s definition ~first_slot:expected ~last_slot:expected ~reason ~nominal:None
      in
      if Int64.(lateness > Recurrence_policy.grace_ns policy)
      then add_omission s Expired
      else (
        match Recurrence_policy.misfire policy with
        | Skip when pending_count s > 0 -> add_omission s Policy_skip
        | (Skip | Coalesce) when ready_capacity s definition = 0 ->
          add_omission s Pending_capacity
        | Skip | Coalesce ->
          let%map s =
            List.fold_result batch.candidates ~init:s ~f:(fun s previous ->
              omit
                s
                definition
                ~first_slot:(slot previous)
                ~last_slot:(slot previous)
                ~reason:Superseded
                ~nominal:(Some previous))
          in
          { s with batch = Some { batch with candidates = [ nominal ] } }
        | Catch_up maximum ->
          let capacity =
            Int.min
              maximum
              (Int.max 0 (Recurrence_policy.max_pending policy - pending_count s))
          in
          if List.length batch.candidates >= capacity
          then add_omission s Pending_capacity
          else
            Ok
              { s with
                batch = Some { batch with candidates = batch.candidates @ [ nominal ] }
              }))
;;

let finish_calendar (s : S.t) ~definition ~lookahead =
  match s.batch with
  | None -> invalid "calendar batch is not collecting"
  | Some batch ->
    let open Result.Let_syntax in
    let%bind s =
      match Recurrence_policy.misfire (F.policy definition), batch.candidates with
      | Coalesce, _ :: _ ->
        let%map s = withdraw_rows s definition ~reason:Superseded s.ready in
        { s with ready = [] }
      | (Skip | Catch_up _), _ | Coalesce, [] -> Ok s
    in
    (* A Ready row may have become protected while this cutoff was scanned.
       Its private Coalesce candidate remains selected custody, not permission.
       Dispose any now-unavailable publication credit only at the proven cutoff. *)
    let retained, omitted =
      List.split_n
        batch.candidates
        (Int.max 0 (ready_capacity s definition - List.length s.ready))
    in
    let%bind s =
      List.fold_result omitted ~init:s ~f:(fun s nominal ->
        omit
          s
          definition
          ~first_slot:(slot nominal)
          ~last_slot:(slot nominal)
          ~reason:Pending_capacity
          ~nominal:(Some nominal))
    in
    let ready = s.ready @ retained in
    if List.length ready > ready_capacity s definition
    then invalid "calendar publication exceeds pending capacity"
    else Ok { (retire_batch s ~definition) with ready; lookahead; issue = None }
;;

let arm (s : S.t) ~definition ~slot ~due_at =
  let open Result.Let_syntax in
  match s.alarm with
  | Some stored
    when Int64.equal slot (Recurrence_alarm.slot (V.Stored_alarm.value stored))
         && Time_ns.equal due_at (Recurrence_alarm.due_at (V.Stored_alarm.value stored))
    -> Ok s
  | Some _ -> invalid "retire previous recurrence alarm before replacement"
  | None ->
    let%bind mark = find_mark s definition in
    let%bind sequence =
      match mark.last_alarm_sequence with
      | None -> Ok 0L
      | Some old -> V.add old 1L
    in
    let%bind key = I.Alarm.create ~definition:(F.key definition) ~sequence in
    let%bind alarm = Recurrence_alarm.create ~key ~slot ~due_at in
    let%map alarm = V.Stored_alarm.create alarm in
    let s = update_mark s { mark with last_alarm_sequence = Some sequence } in
    { s with alarm = Some alarm }
;;

let validate_dispositions (s : S.t) =
  let open Result.Let_syntax in
  List.fold_result s.marks ~init:() ~f:(fun () (mark : V.Mark.t) ->
    let details =
      List.filter s.history ~f:(fun detail ->
        I.Definition.equal detail.definition mark.definition)
    in
    let%bind omitted, withdrawn =
      List.fold_result details ~init:(0L, 0L) ~f:(fun (omitted, withdrawn) detail ->
        let%bind count = V.Detail.count detail in
        match detail.kind with
        | Skipped ->
          Result.map (V.add omitted count) ~f:(fun omitted -> omitted, withdrawn)
        | Withdrawn ->
          Result.map (V.add withdrawn count) ~f:(fun withdrawn -> omitted, withdrawn))
    in
    let%bind () =
      if Int64.(omitted <= mark.omitted && withdrawn <= mark.withdrawn)
      then Ok ()
      else invalid "retained details exceed lifetime disposition totals"
    in
    let pending =
      s.ready @ candidates s
      |> List.filter ~f:(fun row ->
        I.Definition.equal (I.Occurrence.definition (key row)) mark.definition)
    in
    let protected =
      List.filter
        (protected_rows s
         @ List.filter_map (S.execution_rows s) ~f:(fun execution ->
           if Recurrence_execution.is_pending execution
           then None
           else
             Some
               (Recurrence_execution_intent.nominal
                  (Recurrence_execution.intent execution))))
        ~f:(fun row ->
          I.Definition.equal (I.Occurrence.definition (N.key row)) mark.definition)
    in
    let intervals =
      List.map protected ~f:(fun row ->
        let slot = I.Occurrence.slot (N.key row) in
        slot, slot)
      @ List.map details ~f:(fun r -> r.first_slot, r.last_slot)
      @ List.map pending ~f:(fun row -> slot row, slot row)
      |> List.sort ~compare:(fun (a, _) (b, _) -> Int64.compare a b)
    in
    List.fold_result intervals ~init:None ~f:(fun previous (first, last) ->
      if Option.exists previous ~f:(fun old -> Int64.(old >= first))
      then invalid "retained dispositions and pending rows overlap"
      else Ok (Some last))
    |> Result.map ~f:(fun _ -> ()))
;;

let strict_times rows =
  List.fold_result rows ~init:None ~f:(fun previous row ->
    if Option.exists previous ~f:(fun old -> Time_ns.(old >= at row))
    then invalid "nominal rows are not strictly ordered"
    else Ok (Some (at row)))
  |> Result.map ~f:(fun _ -> ())
;;

let validate_current (s : S.t) ~definition:current ~cursor =
  let open Result.Let_syntax in
  let%bind () = Recurrence_frontier_codec.validate s in
  let%bind () = validate_dispositions s in
  let%bind () =
    if I.Definition.equal (C.definition cursor) (F.key current)
    then Ok ()
    else invalid "frontier cursor belongs to another definition"
  in
  let%bind () =
    List.fold_result s.marks ~init:() ~f:(fun () (mark : V.Mark.t) ->
      let rows = s.ready @ candidates s in
      let pending =
        List.count rows ~f:(fun row ->
          I.Definition.equal (I.Occurrence.definition (key row)) mark.definition)
      in
      let%bind total = V.add mark.omitted mark.withdrawn in
      let%bind total = V.add total mark.settled in
      let protected =
        List.count (protected_rows s) ~f:(fun row ->
          I.Definition.equal (I.Occurrence.definition (N.key row)) mark.definition)
      in
      let%bind total = V.add total (Int64.of_int (pending + protected)) in
      let%bind consumed = V.Mark.consumed mark in
      if Int64.equal total consumed
      then Ok ()
      else invalid "frontier high-water differs from disposition totals")
  in
  let%bind mark = find_mark s current in
  let policy = F.policy current in
  let%bind () = strict_times s.ready in
  let%bind () = strict_times (candidates s) in
  let%bind () =
    match s.batch, F.schedule current with
    | None, _ -> Ok ()
    | Some batch, Calendar _ ->
      let capacity =
        match Recurrence_policy.misfire policy with
        | Coalesce -> 1
        | Skip -> if pending_count s = 0 then 1 else 0
        | Catch_up count ->
          Int.min
            count
            (Int.max 0 (Recurrence_policy.max_pending policy - pending_count s))
      in
      if
        List.length batch.candidates <= capacity
        && List.for_all batch.candidates ~f:(fun row -> Time_ns.(at row <= batch.cutoff))
        && Option.is_none s.lookahead
        && Option.is_none s.alarm
      then Ok ()
      else invalid "private calendar batch differs from policy/cutoff"
    | Some _, Interval _ -> invalid "interval frontier retains calendar batch"
  in
  let%bind () =
    match mark.through, F.schedule current with
    | None, _ | Some _, Calendar _ -> Ok ()
    | Some slot, Interval { rule; _ } ->
      let%bind expected =
        if
          R.Rule.Interval.equal_mode (R.Rule.Interval.mode rule) Fixed_delay
          && not (Int64.equal slot 0L)
        then (
          match
            List.find
              (s.ready @ candidates s)
              ~f:(fun row -> Int64.equal (I.Occurrence.slot (key row)) slot)
          with
          | Some row -> Ok (at row)
          | None ->
            (match
               List.find (S.execution_rows s) ~f:(fun execution ->
                 let occurrence =
                   Recurrence_execution_intent.occurrence
                     (Recurrence_execution.intent execution)
                 in
                 I.Definition.equal (I.Occurrence.definition occurrence) (F.key current)
                 && Int64.equal (I.Occurrence.slot occurrence) slot)
             with
             | Some execution ->
               Ok
                 (N.at
                    (Recurrence_execution_intent.nominal
                       (Recurrence_execution.intent execution)))
             | None ->
               (match s.delay_basis with
                | Some basis
                  when Int64.equal
                         slot
                         (I.Occurrence.slot (Recurrence_delay_basis.occurrence basis)) ->
                  Ok (Recurrence_delay_basis.nominal_at basis)
                | None | Some _ ->
                  invalid "fixed-delay high-water lacks exact selected metadata")))
        else Result.map (N.create_interval current ~slot) ~f:N.at
      in
      if Option.equal Time_ns.equal mark.last_at (Some expected)
      then Ok ()
      else invalid "interval high-water time differs from exact selected nominal"
  in
  let%bind () =
    if
      List.length s.ready > ready_capacity s current
      || List.length s.history > Recurrence_policy.max_history policy
      || List.length s.control_history > Recurrence_policy.max_history policy
    then invalid "frontier exceeds captured retention policy"
    else Ok ()
  in
  let all = s.ready @ candidates s @ Option.to_list s.lookahead in
  let%bind () =
    List.fold_result all ~init:() ~f:(fun () row ->
      let%bind () = N.validate (value row) ~definition:current in
      match s.lookahead with
      | Some ahead when V.Stored_nominal.equal ahead row -> Ok ()
      | None | Some _ ->
        if Option.exists mark.through ~f:(fun through -> Int64.(slot row <= through))
        then Ok ()
        else invalid "pending nominal lies above consumed high-water")
  in
  let%bind consumed = V.Mark.consumed mark in
  let%bind () =
    match C.position cursor, F.schedule current with
    | Interval interval, Interval { rule; first_due } ->
      let%bind expected =
        if
          R.Rule.Interval.equal_mode (R.Rule.Interval.mode rule) Fixed_delay
          && not (Int64.equal consumed 0L)
        then (
          match s.delay_basis with
          | Some basis
            when Int64.(
                   I.Occurrence.slot (Recurrence_delay_basis.occurrence basis)
                   = consumed - 1L) ->
            Result.map
              (Recurrence_delay_basis.next_interval basis ~definition:current)
              ~f:R.Cursor.Interval.due_at
          | None | Some _ ->
            invalid "fixed-delay successor lacks exact prior completion basis")
        else
          R.Cursor.Interval.at_slot
            (R.Cursor.Interval.create ~first_due)
            ~rule
            ~slot:consumed
          |> Result.map_error ~f:(fun e ->
            P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t e)))
      in
      if
        Int64.equal consumed (R.Cursor.Interval.slot interval)
        && Time_ns.equal expected (R.Cursor.Interval.due_at interval)
        && Option.is_none s.lookahead
        && Option.is_none s.batch
      then Ok ()
      else invalid "interval cursor differs from exact consumed phase"
    | Calendar { cursor = calendar; next_slot }, Calendar _ ->
      let expected_at =
        Option.value_map s.lookahead ~default:mark.last_at ~f:(fun n -> Some (at n))
      in
      if
        Int64.equal consumed next_slot
        && Option.equal Time_ns.equal expected_at (R.Cursor.Calendar.last_at calendar)
        && Option.for_all s.lookahead ~f:(fun n -> Int64.equal (slot n) next_slot)
      then Ok ()
      else invalid "calendar cursor differs from high-water/lookahead"
    | Awaiting_completion occurrence, Interval { rule; _ } ->
      if
        R.Rule.Interval.equal_mode (R.Rule.Interval.mode rule) Fixed_delay
        && Int64.(consumed > 0L)
        && Int64.equal (I.Occurrence.slot occurrence) Int64.(consumed - 1L)
        && Option.is_none s.batch
        && Option.is_none s.lookahead
      then
        if Int64.equal (I.Occurrence.slot occurrence) 0L
        then Ok ()
        else (
          match s.delay_basis with
          | Some basis
            when I.Occurrence.equal occurrence (Recurrence_delay_basis.occurrence basis)
            ->
            if
              Option.equal
                Time_ns.equal
                mark.last_at
                (Some (Recurrence_delay_basis.nominal_at basis))
            then Ok ()
            else invalid "awaiting terminal nominal differs from exact basis"
          | Some basis
            when Int64.(
                   I.Occurrence.slot (Recurrence_delay_basis.occurrence basis)
                   = I.Occurrence.slot occurrence - 1L) ->
            let%bind next =
              Recurrence_delay_basis.next_interval basis ~definition:current
            in
            if
              Option.equal
                Time_ns.equal
                mark.last_at
                (Some (R.Cursor.Interval.due_at next))
            then Ok ()
            else invalid "awaiting nominal differs from prior completion deadline"
          | None | Some _ -> invalid "later delay selection lacks its exact prior basis")
      else invalid "fixed-delay completion frontier differs from exact selected slot"
    | Exhausted, Interval { rule; first_due } ->
      if Int64.equal consumed 0L || Option.is_some s.lookahead || Option.is_some s.batch
      then invalid "exhausted interval lacks consumed endpoint"
      else (
        let%bind range =
          R.Cursor.Interval.consume_count
            (R.Cursor.Interval.create ~first_due)
            ~rule
            ~count:consumed
          |> Result.map_error ~f:(fun e ->
            P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t e)))
        in
        if Option.is_none range.next
        then Ok ()
        else invalid "interval cursor exhausted before actual endpoint")
    | Exhausted, Calendar _ ->
      if Option.is_none s.lookahead && Option.is_none s.batch
      then Ok ()
      else invalid "exhausted calendar retains search selection"
    | (Interval _ | Calendar _ | Awaiting_completion _), (Interval _ | Calendar _) ->
      invalid "frontier cursor mode differs from source"
  in
  let%bind () =
    match s.alarm with
    | None -> Ok ()
    | Some stored ->
      let alarm = V.Stored_alarm.value stored in
      let identity = Recurrence_alarm.key alarm in
      let nominal =
        match C.position cursor with
        | Interval c -> Some (R.Cursor.Interval.slot c, R.Cursor.Interval.due_at c)
        | Calendar _ -> Option.map s.lookahead ~f:(fun n -> slot n, at n)
        | Awaiting_completion _ | Exhausted -> None
      in
      if
        I.Definition.equal (I.Alarm.definition identity) (F.key current)
        && Option.equal
             Int64.equal
             mark.last_alarm_sequence
             (Some (I.Alarm.sequence identity))
        && Option.exists nominal ~f:(fun (slot, at) ->
          Int64.equal slot (Recurrence_alarm.slot alarm)
          && Time_ns.equal at (Recurrence_alarm.due_at alarm))
      then Ok ()
      else invalid "owned alarm differs from current nominal frontier"
  in
  List.fold_result s.history ~init:() ~f:(fun () (detail : V.Detail.t) ->
    match
      List.find s.marks ~f:(fun mark ->
        I.Definition.equal mark.definition detail.definition)
    with
    | None -> invalid "history references absent source revision"
    | Some mark ->
      if
        Option.exists mark.through ~f:(fun through -> Int64.(detail.last_slot <= through))
      then Ok ()
      else invalid "history range lies above consumed high-water")
;;

let validate_associations (s : S.t) ~definitions ~cursor =
  let open Result.Let_syntax in
  let%bind current =
    match List.last definitions with
    | None -> invalid "frontier has no source definitions"
    | Some f -> Ok f
  in
  let%bind () =
    if List.length definitions = List.length s.marks
    then Ok ()
    else invalid "frontier source inventory differs from entry"
  in
  let%bind () =
    List.fold_result definitions ~init:() ~f:(fun () definition ->
      Result.map (find_mark s definition) ~f:(fun _ -> ()))
  in
  let%bind () =
    List.fold_result s.history ~init:() ~f:(fun () (row : V.Detail.t) ->
      match row.nominal with
      | None -> Ok ()
      | Some nominal ->
        (match
           List.find definitions ~f:(fun f -> I.Definition.equal (F.key f) row.definition)
         with
         | None -> invalid "retained nominal lacks captured definition"
         | Some definition -> N.validate (value nominal) ~definition))
  in
  let%bind () =
    List.fold_result s.protected_children ~init:() ~f:(fun () row ->
      match
        List.find definitions ~f:(fun definition ->
          I.Definition.equal
            (F.key definition)
            (I.Occurrence.definition (Recurrence_child_reservation.occurrence row)))
      with
      | None -> invalid "protected creation lacks captured definition"
      | Some definition -> Recurrence_child_reservation.validate row ~definition)
  in
  let%bind () =
    List.fold_result (S.execution_rows s) ~init:() ~f:(fun () execution ->
      let intent = Recurrence_execution.intent execution in
      let occurrence = Recurrence_execution_intent.occurrence intent in
      let%bind definition =
        Result.of_option
          (List.find definitions ~f:(fun definition ->
             I.Definition.equal (F.key definition) (I.Occurrence.definition occurrence)))
          ~error:(P.Error.invalid_request "execution lacks exact captured definition")
      in
      let reservation =
        List.find s.protected_children ~f:(fun row ->
          I.Occurrence.equal occurrence (Recurrence_child_reservation.occurrence row))
      in
      let%bind () =
        Recurrence_execution_intent.validate intent ~definition ~reservation
      in
      let%bind () =
        match Recurrence_execution_intent.child intent with
        | None ->
          if Option.is_none reservation
          then Ok ()
          else invalid "existing-target execution acquired a child reservation"
        | Some child ->
          if
            List.exists s.child_associations ~f:(Recurrence_child_association.equal child)
          then Ok ()
          else invalid "execution child differs from exact retained association"
      in
      let selected = s.ready @ candidates s @ Option.to_list s.lookahead in
      if List.exists selected ~f:(fun row -> I.Occurrence.equal occurrence (key row))
      then invalid "execution overlaps an unadmitted nominal"
      else Ok ())
  in
  let%bind () =
    List.fold_result definitions ~init:() ~f:(fun () definition ->
      let%bind mark = find_mark s definition in
      let retained =
        List.count (S.execution_rows s) ~f:(fun execution ->
          (not (Recurrence_execution.is_pending execution))
          && I.Definition.equal
               (I.Occurrence.definition
                  (Recurrence_execution_intent.occurrence
                     (Recurrence_execution.intent execution)))
               (F.key definition))
      in
      if Int64.(of_int retained <= mark.settled)
      then Ok ()
      else invalid "retained terminal details exceed lifetime settled count")
  in
  let%bind () =
    if
      List.count (S.execution_rows s) ~f:(fun execution ->
        not (Recurrence_execution.is_pending execution))
      <= Recurrence_policy.max_history (F.policy current)
    then Ok ()
    else invalid "terminal execution detail exceeds captured history policy"
  in
  let%bind () =
    match s.delay_basis with
    | None -> Ok ()
    | Some basis ->
      let%bind () = Recurrence_delay_basis.validate basis ~definition:current in
      let%bind mark = find_mark s current in
      let%bind () =
        if Int64.(mark.settled > 0L)
        then Ok ()
        else invalid "completion basis lacks lifetime settlement"
      in
      let key = Recurrence_delay_basis.occurrence basis in
      let%bind () =
        if
          Option.exists mark.through ~f:(fun through ->
            Int64.(I.Occurrence.slot key <= through))
        then Ok ()
        else invalid "fixed-delay basis lies above consumed high-water"
      in
      (match S.find_execution s key with
       | None -> Ok ()
       | Some execution ->
         let attempt = Recurrence_execution.current_attempt execution in
         if
           Option.equal
             Recurrence_run_origin.equal
             (Recurrence_execution.Attempt.origin attempt)
             (Some (Recurrence_delay_basis.origin basis))
           && Option.equal
                Recurrence_run_reference.equal
                (Recurrence_execution.Attempt.terminal attempt)
                (Some (Recurrence_delay_basis.terminal basis))
           && Time_ns.equal
                (Recurrence_nominal.at
                   (Recurrence_execution_intent.nominal
                      (Recurrence_execution.intent execution)))
                (Recurrence_delay_basis.nominal_at basis)
         then Ok ()
         else invalid "fixed-delay basis differs from retained terminal execution")
  in
  let%bind () =
    List.fold_result s.control_history ~init:() ~f:(fun () detail ->
      match detail.kind with
      | Retired_alarm | Retired_batch -> Ok ()
      | Retired_delay_basis ->
        let%bind basis = Recurrence_delay_basis.of_json detail.json in
        let%bind definition =
          Result.of_option
            (List.find definitions ~f:(fun definition ->
               I.Definition.equal
                 (F.key definition)
                 (I.Occurrence.definition (Recurrence_delay_basis.occurrence basis))))
            ~error:
              (P.Error.invalid_request "archived delay basis lacks retained definition")
        in
        let%bind () = Recurrence_delay_basis.validate basis ~definition in
        let%bind mark = find_mark s definition in
        if
          Int64.(mark.settled > 0L)
          && Option.exists mark.through ~f:(fun through ->
            Int64.(I.Occurrence.slot (Recurrence_delay_basis.occurrence basis) <= through))
        then Ok ()
        else invalid "archived delay basis lacks consumed settlement")
  in
  validate_current s ~definition:current ~cursor
;;
