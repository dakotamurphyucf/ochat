open Core

module Interval = struct
  type t =
    { slot : int64
    ; due_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving equal, sexp_of]

  type due_range =
    { first_slot : int64
    ; last_slot : int64
    ; count : int64
    ; next : t option
    }
  [@@deriving sexp_of]

  let create ~first_due = { slot = 0L; due_at = first_due }
  let slot t = t.slot
  let due_at t = t.due_at

  let at_slot t ~rule ~slot =
    if Int64.(slot < 0L || slot < t.slot)
    then Codec.invalid "nominal slot precedes interval cursor"
    else (
      match Rule.Interval.mode rule with
      | Fixed_delay ->
        if Int64.equal slot t.slot
        then Ok t.due_at
        else Codec.invalid "fixed-delay nominal requires actual completion"
      | Fixed_rate ->
        let count = Int64.(slot - t.slot) in
        let period = Rule.Period.nanoseconds (Rule.Interval.period rule) in
        if Int64.(count > max_value / period)
        then Error (Timing_error.Out_of_range "interval nominal slot")
        else Time_arithmetic.add_ns t.due_at Int64.(count * period))
  ;;

  let consume_count t ~rule ~count =
    match Rule.Interval.mode rule with
    | Fixed_delay -> Codec.invalid "fixed-delay cursor requires actual completion"
    | Fixed_rate ->
      if Int64.(count <= 0L)
      then Codec.invalid "consumed count must be positive"
      else (
        let period = Rule.Period.nanoseconds (Rule.Interval.period rule) in
        let additional = Int64.pred count in
        if Int64.(additional > max_value - t.slot || additional > max_value / period)
        then Error (Timing_error.Out_of_range "interval consumed range")
        else
          let open Result.Let_syntax in
          let%bind last_at =
            Time_arithmetic.add_ns t.due_at Int64.(additional * period)
          in
          let last_slot = Int64.(t.slot + additional) in
          let next =
            if Int64.equal last_slot Int64.max_value
            then None
            else (
              match Time_arithmetic.add_ns last_at period with
              | Error _ -> None
              | Ok due_at -> Some { slot = Int64.succ last_slot; due_at })
          in
          Ok { first_slot = t.slot; last_slot; count; next })
  ;;

  let due_through t ~rule ~now =
    match Rule.Interval.mode rule with
    | Fixed_delay -> Codec.invalid "fixed-delay cursor requires actual completion"
    | Fixed_rate ->
      if Time_ns.compare now t.due_at < 0
      then Ok None
      else (
        let period = Rule.Period.nanoseconds (Rule.Interval.period rule) in
        let difference =
          Int64.(Time_arithmetic.nanoseconds now - Time_arithmetic.nanoseconds t.due_at)
        in
        consume_count t ~rule ~count:Int64.((difference / period) + 1L)
        |> Result.map ~f:Option.some)
  ;;

  let after_completion t ~rule ~completed_at =
    match Rule.Interval.mode rule with
    | Fixed_rate -> Codec.invalid "fixed-rate cursor does not advance from completion"
    | Fixed_delay ->
      if Int64.equal t.slot Int64.max_value
      then Error (Timing_error.Out_of_range "interval slot identity")
      else
        let open Result.Let_syntax in
        let%map due_at =
          Time_arithmetic.add_ns
            completed_at
            (Rule.Period.nanoseconds (Rule.Interval.period rule))
        in
        { slot = Int64.succ t.slot; due_at }
  ;;

  let to_json t =
    `Object
      [ "version", Codec.number 1L
      ; "slot", Codec.number t.slot
      ; "due_at", Time_arithmetic.to_json t.due_at
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      Codec.object_fields json ~required:[ "version"; "slot"; "due_at" ]
    in
    let%bind () = Codec.version fields in
    let%bind slot_json = Codec.field fields "slot" in
    let%bind slot = Codec.int64 slot_json in
    let%bind due_json = Codec.field fields "due_at" in
    let%bind due_at = Time_arithmetic.of_json due_json in
    if Int64.(slot < 0L)
    then Codec.invalid "negative interval slot"
    else Ok { slot; due_at }
  ;;
end

module Calendar = struct
  module Occurrence = struct
    type t =
      { date : Date.t
      ; ofday : Time_ns.Ofday.t
      ; fold : int
      ; at : Time_ns.Alternate_sexp.t
      ; shifted_gap : bool
      ; zone : string
      ; provenance : Zone.Provenance.t
      }
    [@@deriving equal, sexp_of]
  end

  type scan =
    | Fresh of Date.t
    | Scanning of
        { next_date : Date.t
        ; best : Occurrence.t option
        ; provenance : Zone.Provenance.t
        }
  [@@deriving equal, sexp_of]

  type t =
    { last_at : Time_ns.Alternate_sexp.t option
    ; scan : scan
    }
  [@@deriving equal, sexp_of]

  type search =
    | Found of Occurrence.t * t
    | Continue of t
    | End
  [@@deriving sexp_of]

  let create rule = { last_at = None; scan = Fresh (Rule.Calendar.start_date rule) }
  let last_at t = t.last_at

  let restart_date rule last_at =
    match last_at with
    | None -> Rule.Calendar.start_date rule
    | Some at ->
      let utc_date, _ = Time_arithmetic.utc_parts at in
      Date.max (Rule.Calendar.start_date rule) (Date.add_days utc_date (-1))
  ;;

  let selected_candidates rule = function
    | Zone.Resolution.Gap { ends_at } ->
      (match Rule.Calendar.gap rule with
       | Skip -> []
       | At_gap_end -> [ 0, ends_at, true ])
    | Candidates candidates ->
      let indexed = List.mapi candidates ~f:(fun fold at -> fold, at, false) in
      (match Rule.Calendar.repeat rule with
       | Earlier -> List.take indexed 1
       | Later -> List.take (List.rev indexed) 1
       | Both -> indexed)
  ;;

  let last_date =
    Time_arithmetic.utc_parts (Time_ns.of_int63_ns_since_epoch Int63.max_value) |> fst
  ;;

  let past_end rule date =
    Date.compare date last_date > 0
    || Option.value_map (Rule.Calendar.end_date rule) ~default:false ~f:(fun last ->
      Date.compare date last > 0)
  ;;

  let candidate_compare (a : Occurrence.t) (b : Occurrence.t) =
    match Time_ns.compare a.at b.at with
    | 0 ->
      (match Date.compare a.date b.date with
       | 0 -> Int.compare a.fold b.fold
       | comparison -> comparison)
    | comparison -> comparison
  ;;

  let retain_earliest best candidate =
    match best with
    | None -> Some candidate
    | Some previous ->
      if candidate_compare candidate previous < 0 then Some candidate else best
  ;;

  let validate_best ~rule ~zone ~last_at = function
    | None -> Ok ()
    | Some (best : Occurrence.t) ->
      if
        (not (String.equal best.zone (Zone.name zone)))
        || (not (Zone.Provenance.equal best.provenance (Zone.provenance zone)))
        || (not (Rule.Calendar.matches rule best.date))
        || (not (Time_ns.Ofday.equal best.ofday (Rule.Calendar.ofday rule)))
        || Option.value_map last_at ~default:false ~f:(fun at ->
          Time_ns.compare best.at at <= 0)
      then Codec.invalid "cached calendar candidate does not match rule/zone/frontier"
      else
        let open Result.Let_syntax in
        let%bind resolution = Zone.resolve zone ~date:best.date ~ofday:best.ofday in
        if
          List.exists
            (selected_candidates rule resolution)
            ~f:(fun (fold, at, shifted_gap) ->
              Int.equal fold best.fold
              && Time_ns.equal at best.at
              && Bool.equal shifted_gap best.shifted_gap)
        then Ok ()
        else
          Codec.invalid "cached calendar candidate is not an actual admitted occurrence"
  ;;

  let next t ~rule ~zone ~max_steps =
    let open Result.Let_syntax in
    if max_steps <= 0 || max_steps > 4096
    then Codec.invalid "calendar max_steps must be in 1..4096"
    else if not (String.equal (Rule.Calendar.zone rule) (Zone.name zone))
    then Error Timing_error.Zone_mismatch
    else (
      let next_date, best =
        match t.scan with
        | Fresh date -> date, None
        | Scanning { next_date; best; provenance } ->
          if Zone.Provenance.equal provenance (Zone.provenance zone)
          then next_date, best
          else restart_date rule t.last_at, None
      in
      if Date.compare next_date (Rule.Calendar.start_date rule) < 0
      then Codec.invalid "calendar cursor precedes rule start"
      else (
        let%bind _ = Time_arithmetic.day_bounds_ns next_date in
        let%bind () = validate_best ~rule ~zone ~last_at:t.last_at best in
        let emit best =
          Found
            ( best
            , { last_at = Some best.Occurrence.at
              ; scan = Fresh (restart_date rule (Some best.at))
              } )
        in
        let rec search next_date best remaining =
          let ended = past_end rule next_date in
          let%bind safe_to_emit =
            match best with
            | None -> Ok false
            | Some _ when ended -> Ok true
            | Some best ->
              let%map midnight_ns, _ = Time_arithmetic.day_bounds_ns next_date in
              let ofday_ns =
                Time_ns.Ofday.to_span_since_start_of_day (Rule.Calendar.ofday rule)
                |> Time_ns.Span.to_int63_ns
                |> Int63.to_int64
              in
              (* Every candidate (including a gap endpoint) is at least its local
                 scalar minus the maximum admitted offset of one day. Later
                 civil labels therefore cannot precede this lower UTC bound. *)
              let lower_bound = Int64.(midnight_ns + ofday_ns - 86_400_000_000_000L) in
              Int64.(lower_bound >= Time_arithmetic.nanoseconds best.Occurrence.at)
          in
          match best, safe_to_emit with
          | Some best, true -> Ok (emit best)
          | _, _ when ended -> Ok End
          | _, _ when Int.equal remaining 0 ->
            Ok
              (Continue
                 { t with
                   scan = Scanning { next_date; best; provenance = Zone.provenance zone }
                 })
          | _ ->
            let%bind best =
              if not (Rule.Calendar.matches rule next_date)
              then Ok best
              else (
                let%map resolution =
                  Zone.resolve zone ~date:next_date ~ofday:(Rule.Calendar.ofday rule)
                in
                List.fold
                  (selected_candidates rule resolution)
                  ~init:best
                  ~f:(fun best (fold, at, shifted_gap) ->
                    if
                      Option.value_map t.last_at ~default:false ~f:(fun last ->
                        Time_ns.compare at last <= 0)
                    then best
                    else
                      retain_earliest
                        best
                        { Occurrence.date = next_date
                        ; ofday = Rule.Calendar.ofday rule
                        ; fold
                        ; at
                        ; shifted_gap
                        ; zone = Zone.name zone
                        ; provenance = Zone.provenance zone
                        }))
            in
            search (Date.add_days next_date 1) best (remaining - 1)
        in
        search next_date best max_steps))
  ;;

  let provenance_json provenance =
    `Object
      [ "source", `String (Zone.Provenance.source provenance)
      ; "sha256", `String (Zone.Provenance.sha256 provenance)
      ]
  ;;

  let provenance_of_json json =
    let open Result.Let_syntax in
    let%bind fields = Codec.object_fields json ~required:[ "source"; "sha256" ] in
    let%bind source_json = Codec.field fields "source" in
    let%bind source = Codec.string source_json in
    let%bind sha256_json = Codec.field fields "sha256" in
    let%bind sha256 = Codec.string sha256_json in
    Zone.Provenance.create ~source ~sha256
  ;;

  let candidate_json (best : Occurrence.t) =
    `Object
      [ "date", `String (Date.to_string best.date)
      ; "ofday", `String (Time_ns.Ofday.to_string best.ofday)
      ; "fold", Codec.number (Int64.of_int best.fold)
      ; "at", Time_arithmetic.to_json best.at
      ; ("shifted_gap", if best.shifted_gap then `True else `False)
      ; "zone", `String best.zone
      ; "provenance", provenance_json best.provenance
      ]
  ;;

  let candidate_of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      Codec.object_fields
        json
        ~required:[ "date"; "ofday"; "fold"; "at"; "shifted_gap"; "zone"; "provenance" ]
    in
    let%bind date_json = Codec.field fields "date" in
    let%bind date = Codec.date date_json in
    let%bind _ = Time_arithmetic.day_bounds_ns date in
    let%bind ofday_json = Codec.field fields "ofday" in
    let%bind ofday = Codec.ofday ofday_json in
    let%bind fold_json = Codec.field fields "fold" in
    let%bind fold = Codec.int fold_json in
    let%bind at_json = Codec.field fields "at" in
    let%bind at = Time_arithmetic.of_json at_json in
    let%bind gap_json = Codec.field fields "shifted_gap" in
    let%bind shifted_gap =
      match gap_json with
      | `True -> Ok true
      | `False -> Ok false
      | _ -> Codec.invalid "invalid gap flag"
    in
    let%bind zone_json = Codec.field fields "zone" in
    let%bind zone = Codec.string zone_json in
    let%bind () = Zone.validate_name zone in
    let%bind provenance_json = Codec.field fields "provenance" in
    let%bind provenance = provenance_of_json provenance_json in
    if
      fold < 0
      || fold > 4096
      || (shifted_gap && not (Int.equal fold 0))
      || Date.compare date last_date > 0
    then Codec.invalid "invalid cached calendar candidate"
    else Ok { Occurrence.date; ofday; fold; at; shifted_gap; zone; provenance }
  ;;

  let to_json t =
    let scan =
      match t.scan with
      | Fresh date ->
        `Object [ "kind", `String "fresh"; "date", `String (Date.to_string date) ]
      | Scanning { next_date; best; provenance } ->
        `Object
          [ "kind", `String "scanning"
          ; "next_date", `String (Date.to_string next_date)
          ; "best", Option.value_map best ~default:`Null ~f:candidate_json
          ; "provenance", provenance_json provenance
          ]
    in
    `Object
      [ "version", Codec.number 1L
      ; "last_at", Option.value_map t.last_at ~default:`Null ~f:Time_arithmetic.to_json
      ; "scan", scan
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      Codec.object_fields json ~required:[ "version"; "last_at"; "scan" ]
    in
    let%bind () = Codec.version fields in
    let%bind last_json = Codec.field fields "last_at" in
    let%bind last_at = Codec.nullable Time_arithmetic.of_json last_json in
    let%bind scan_json = Codec.field fields "scan" in
    let%bind scan =
      match scan_json with
      | `Object initial ->
        let%bind kind_json = Codec.field initial "kind" in
        let%bind kind = Codec.string kind_json in
        (match kind with
         | "fresh" ->
           let%bind fields = Codec.object_fields scan_json ~required:[ "kind"; "date" ] in
           let%bind date_json = Codec.field fields "date" in
           let%bind date = Codec.date date_json in
           let%map _ = Time_arithmetic.day_bounds_ns date in
           Fresh date
         | "scanning" ->
           let%bind fields =
             Codec.object_fields
               scan_json
               ~required:[ "kind"; "next_date"; "best"; "provenance" ]
           in
           let%bind date_json = Codec.field fields "next_date" in
           let%bind next_date = Codec.date date_json in
           let%bind _ = Time_arithmetic.day_bounds_ns next_date in
           let%bind best_json = Codec.field fields "best" in
           let%bind best = Codec.nullable candidate_of_json best_json in
           let%bind provenance_json = Codec.field fields "provenance" in
           let%bind provenance = provenance_of_json provenance_json in
           if
             Option.value_map best ~default:false ~f:(fun best ->
               Date.compare best.Occurrence.date next_date >= 0
               || (not (Zone.Provenance.equal best.provenance provenance))
               || Option.value_map last_at ~default:false ~f:(fun at ->
                 Time_ns.compare best.at at <= 0))
           then Codec.invalid "invalid retained calendar search frontier"
           else Ok (Scanning { next_date; best; provenance })
         | _ -> Codec.invalid "invalid calendar search kind")
      | _ -> Codec.invalid "expected calendar search object"
    in
    Ok { last_at; scan }
  ;;
end
