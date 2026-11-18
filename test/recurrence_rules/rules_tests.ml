open Core
open Fixtures
module R = Recurrence_rules

let%expect_test
    "inverse resolution matches independent UTC enumeration at discontinuities"
  =
  let cases =
    [ chicago, "2024-03-10", [ "01:30"; "02:30"; "03:30" ]
    ; chicago, "2024-11-03", [ "00:30"; "01:30"; "02:30" ]
    ; lord_howe, "2024-04-07", [ "01:15"; "01:45"; "02:15" ]
    ; lord_howe, "2024-10-06", [ "01:45"; "02:15"; "02:45" ]
    ; midnight, "2024-06-01", [ "00:00"; "00:30"; "01:00" ]
    ; midnight, "2024-10-01", [ "00:00"; "00:30"; "01:00" ]
    ; apia, "2011-12-29", [ "00:00"; "12:00"; "23:00" ]
    ; apia, "2011-12-30", [ "00:00"; "12:00"; "23:00" ]
    ; apia, "2011-12-31", [ "00:00"; "12:00"; "23:00" ]
    ; multiple, "2024-06-01", [ "00:30"; "01:30"; "02:15"; "02:45" ]
    ]
  in
  let checked = ref 0 in
  List.iter cases ~f:(fun (fixture, day, times) ->
    List.iter times ~f:(fun at ->
      let date = date day
      and ofday = ofday at in
      let expected = candidates fixture ~date ~ofday in
      (match R.Zone.resolve fixture.zone ~date ~ofday |> ok with
       | Candidates actual ->
         assert (not (List.is_empty actual));
         assert (List.equal Time_ns.equal expected actual);
         List.iter actual ~f:(fun timestamp ->
           let roundtrip_date, roundtrip_ofday =
             R.Zone.to_local fixture.zone timestamp |> ok
           in
           assert (
             Date.equal date roundtrip_date && Time_ns.Ofday.equal ofday roundtrip_ofday))
       | Gap _ -> assert (List.is_empty expected));
      incr checked));
  printf "checked %d civil labels, including midnight and a skipped day\n" !checked;
  [%expect {| checked 31 civil labels, including midnight and a skipped day |}]
;;

let%expect_test
    "calendar folds, gaps and skipped-day coalescing retain nominal identities"
  =
  List.iter [ R.Rule.Calendar.Earlier; Later; Both ] ~f:(fun repeat ->
    let rule =
      calendar
        ~repeat
        ~end_date:(date "2024-11-03")
        ~fixture:chicago
        ~start:"2024-11-03"
        ~ofday:"01:30"
        Daily
    in
    let rows =
      collect (R.Cursor.Calendar.create rule) rule chicago.zone ~steps:1 ~remaining:10
    in
    print_s
      [%sexp
        (List.map rows ~f:(fun row -> row.R.Cursor.Calendar.Occurrence.fold) : int list)]);
  List.iter [ R.Rule.Calendar.Skip; At_gap_end ] ~f:(fun gap ->
    let rule =
      calendar
        ~gap
        ~end_date:(date "2011-12-31")
        ~fixture:apia
        ~start:"2011-12-30"
        ~ofday:"00:00"
        Daily
    in
    let rows =
      collect (R.Cursor.Calendar.create rule) rule apia.zone ~steps:1 ~remaining:10
    in
    print_s
      [%sexp
        (List.map rows ~f:(fun row ->
           Date.to_string row.R.Cursor.Calendar.Occurrence.date, row.shifted_gap)
         : (string * bool) list)]);
  [%expect
    {|
    (0)
    (1)
    (0 1)
    ((2011-12-31 false))
    ((2011-12-30 true)) |}]
;;

let%expect_test
    "calendar frontier orders interleaved civil dates and validates retained candidates"
  =
  let rule =
    calendar
      ~repeat:Both
      ~end_date:(date "2024-06-02")
      ~fixture:interleaved
      ~start:"2024-06-01"
      ~ofday:"12:00"
      Daily
  in
  let oracle =
    List.concat_map
      [ date "2024-06-01"; date "2024-06-02" ]
      ~f:(fun date ->
        candidates interleaved ~date ~ofday:(ofday "12:00")
        |> List.mapi ~f:(fun fold at -> date, fold, at))
    |> List.sort ~compare:(fun (_, _, a) (_, _, b) -> Time_ns.compare a b)
  in
  assert (Int.equal (List.length oracle) 4);
  List.iter [ 1; 2; 3; 4096 ] ~f:(fun steps ->
    List.iter [ false; true ] ~f:(fun restore ->
      let actual =
        collect
          ~restore
          (R.Cursor.Calendar.create rule)
          rule
          interleaved.zone
          ~steps
          ~remaining:100
      in
      assert (
        List.equal
          (fun (date_a, fold_a, at_a) (date_b, fold_b, at_b) ->
             Date.equal date_a date_b
             && Int.equal fold_a fold_b
             && Time_ns.equal at_a at_b)
          oracle
          (List.map actual ~f:(fun row ->
             row.R.Cursor.Calendar.Occurrence.date, row.fold, row.at)))));
  List.iter oracle ~f:(fun (date, fold, at) ->
    printf "%s/%d -> %s\n" (Date.to_string date) fold (Time_ns.to_string_utc at));
  let after_first =
    match
      R.Cursor.Calendar.next
        (R.Cursor.Calendar.create rule)
        ~rule
        ~zone:interleaved.zone
        ~max_steps:1
      |> ok
    with
    | Found (_, cursor) -> cursor
    | Continue _ | End -> assert false
  in
  let retained =
    match
      R.Cursor.Calendar.next after_first ~rule ~zone:interleaved.zone ~max_steps:1 |> ok
    with
    | Continue cursor -> cursor
    | Found _ | End -> assert false
  in
  let map_field json key f =
    match json with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name key then f value else value))
    | _ -> assert false
  in
  let encoded = R.Cursor.Calendar.to_json retained in
  List.iter
    [ "at", `String "2024-06-02T12:00:00.000000001Z"
    ; "fold", `Number "0"
    ; "date", `String "2024-06-02"
    ; "ofday", `String "12:01:00"
    ; "zone", `String "UTC"
    ]
    ~f:(fun (key, replacement) ->
      let forged =
        map_field encoded "scan" (fun scan ->
          map_field scan "best" (fun best -> map_field best key (fun _ -> replacement)))
      in
      match R.Cursor.Calendar.of_json forged with
      | Error _ -> ()
      | Ok forged ->
        assert (
          Result.is_error
            (R.Cursor.Calendar.next forged ~rule ~zone:interleaved.zone ~max_steps:1)));
  let changed =
    R.Zone.create
      ~name:(R.Zone.name interleaved.zone)
      ~provenance:
        (R.Zone.Provenance.create ~source:"updated-fixture" ~sha256:(String.make 64 'b')
         |> ok)
      ~from:(time "2000-01-01 00:00:00Z")
      ~until:(time "2100-01-01 00:00:00Z")
      ~initial_offset_seconds:0
      ~transitions:[]
    |> ok
  in
  (match R.Cursor.Calendar.next retained ~rule ~zone:changed ~max_steps:1 |> ok with
   | Found (row, _) ->
     assert (Time_ns.equal row.at (time "2024-06-01 12:00:00Z"));
     assert (R.Zone.Provenance.equal row.provenance (R.Zone.provenance changed))
   | Continue _ | End -> assert false);
  [%expect
    {|
    2024-06-01/0 -> 2024-05-31 12:00:00.000000000Z
    2024-06-02/0 -> 2024-06-01 12:00:00.000000000Z
    2024-06-01/1 -> 2024-06-02 12:00:00.000000000Z
    2024-06-02/1 -> 2024-06-03 12:00:00.000000000Z |}]
;;

let%expect_test "calendar cursors roundtrip partial endpoint days and exhausted sentinel" =
  let endpoint_dates =
    [ Time_ns.of_int63_ns_since_epoch Int63.min_value, "12:00"
    ; Time_ns.of_int63_ns_since_epoch Int63.max_value, "00:00"
    ]
  in
  List.iter endpoint_dates ~f:(fun (timestamp, at) ->
    let day = Date.of_string (String.prefix (Time_ns.to_string_utc timestamp) 10) in
    let rule =
      R.Rule.Calendar.create
        ~pattern:Daily
        ~ofday:(ofday at)
        ~zone:(R.Zone.name full_range_utc)
        ~start_date:day
        ()
      |> ok
    in
    let cursor = R.Cursor.Calendar.create rule in
    assert (
      R.Cursor.Calendar.equal
        cursor
        (R.Cursor.Calendar.of_json (R.Cursor.Calendar.to_json cursor) |> ok)));
  let last =
    Date.of_string
      (String.prefix
         (Time_ns.to_string_utc (Time_ns.of_int63_ns_since_epoch Int63.max_value))
         10)
  in
  let rule =
    R.Rule.Calendar.create
      ~pattern:Daily
      ~ofday:(ofday "00:00")
      ~zone:(R.Zone.name full_range_utc)
      ~start_date:last
      ()
    |> ok
  in
  let sentinel day =
    `Object
      [ "version", `Number "1"
      ; "last_at", `Null
      ; "scan", `Object [ "kind", `String "fresh"; "date", `String (Date.to_string day) ]
      ]
  in
  let cursor = R.Cursor.Calendar.of_json (sentinel (Date.add_days last 1)) |> ok in
  assert (
    R.Cursor.Calendar.equal
      cursor
      (R.Cursor.Calendar.of_json (R.Cursor.Calendar.to_json cursor) |> ok));
  (match R.Cursor.Calendar.next cursor ~rule ~zone:full_range_utc ~max_steps:1 |> ok with
   | End -> ()
   | Found _ | Continue _ -> assert false);
  assert (Result.is_error (R.Cursor.Calendar.of_json (sentinel (Date.add_days last 2))));
  print_endline "both partial endpoint days and the one-day exhaustion sentinel roundtrip";
  [%expect {| both partial endpoint days and the one-day exhaustion sentinel roundtrip |}]
;;

let%expect_test
    "bounded civil search resumes independently of chunk size across leap months"
  =
  let rule =
    calendar
      ~end_date:(date "2025-03-31")
      ~fixture:utc
      ~start:"2024-01-01"
      ~ofday:"12:00"
      (Month_days [ 29; 31 ])
  in
  let collect steps =
    collect (R.Cursor.Calendar.create rule) rule utc.zone ~steps ~remaining:1000
  in
  let expected =
    List.init
      (Date.diff (date "2025-03-31") (date "2024-01-01") + 1)
      ~f:(Date.add_days (date "2024-01-01"))
    |> List.filter ~f:(fun day -> List.mem [ 29; 31 ] (Date.day day) ~equal:Int.equal)
  in
  let rows = collect 1 in
  assert (
    List.equal
      Date.equal
      expected
      (List.map rows ~f:(fun row -> row.R.Cursor.Calendar.Occurrence.date)));
  List.iter [ 2; 7; 31; 4096 ] ~f:(fun steps ->
    assert (List.equal R.Cursor.Calendar.Occurrence.equal rows (collect steps)));
  let first =
    R.Cursor.Calendar.next
      (R.Cursor.Calendar.create rule)
      ~rule
      ~zone:utc.zone
      ~max_steps:1
    |> ok
  in
  (match first with
   | Continue cursor ->
     assert (
       R.Cursor.Calendar.equal
         cursor
         (R.Cursor.Calendar.of_json (R.Cursor.Calendar.to_json cursor) |> ok))
   | Found _ | End -> assert false);
  printf
    "%d valid month-day labels; chunking preserves exact outcomes\n"
    (List.length rows);
  [%expect {| 23 valid month-day labels; chunking preserves exact outcomes |}]
;;

let%expect_test
    "fixed-rate due ranges match an iterative oracle and skip years arithmetically"
  =
  let first_due = time "2024-01-01 00:00:00Z" in
  List.iter [ 1L; 7L; 1_000L; 86_400_000L ] ~f:(fun milliseconds ->
    let rule = R.Rule.Interval.create ~mode:Fixed_rate ~period:(period milliseconds) in
    List.iter [ 0; 1; 2; 17; 100 ] ~f:(fun ticks ->
      let now =
        Time_ns.add
          first_due
          (Time_ns.Span.of_int63_ns
             (Int63.of_int64_exn Int64.(milliseconds * 1_000_000L * of_int ticks)))
      in
      let cursor = R.Cursor.Interval.create ~first_due in
      let range =
        R.Cursor.Interval.due_through cursor ~rule ~now |> ok |> Option.value_exn
      in
      let rec count at total =
        if Time_ns.compare at now > 0
        then total
        else
          count
            (Time_ns.add at (Time_ns.Span.of_ms (Int64.to_float milliseconds)))
            (total + 1)
      in
      assert (Int64.equal range.count (Int64.of_int (count first_due 0)));
      assert (Int64.equal range.last_slot (Int64.of_int ticks));
      let next = Option.value_exn range.next in
      assert (Time_ns.compare (R.Cursor.Interval.due_at next) now > 0);
      assert (
        R.Cursor.Interval.equal
          next
          (R.Cursor.Interval.of_json (R.Cursor.Interval.to_json next) |> ok))));
  let rule = R.Rule.Interval.create ~mode:Fixed_rate ~period:(period 1L) in
  let range =
    R.Cursor.Interval.due_through
      (R.Cursor.Interval.create ~first_due)
      ~rule
      ~now:(time "2054-01-01 00:00:00Z")
    |> ok
    |> Option.value_exn
  in
  assert (Int64.(range.count > 900_000_000_000L));
  print_endline "iterative oracle agrees; 30-year gap returns one arithmetic range";
  [%expect {| iterative oracle agrees; 30-year gap returns one arithmetic range |}]
;;

let%expect_test "negative epoch civil conversion agrees with the independent UTC printer" =
  let day = 86_400_000_000_000L in
  let samples =
    [ Int63.to_int64 Int63.min_value
    ; Int64.succ (Int63.to_int64 Int63.min_value)
    ; Int64.pred (Int63.to_int64 Int63.max_value)
    ; Int63.to_int64 Int63.max_value
    ]
    @ List.concat_map [ -3L; -2L; -1L; 0L; 1L; 2L; 3L ] ~f:(fun days ->
      List.map
        [ -1L; 0L; 1L; Int64.(day / 2L); Int64.pred day ]
        ~f:(fun offset -> Int64.((days * day) + offset)))
    |> List.dedup_and_sort ~compare:Int64.compare
  in
  List.iter samples ~f:(fun ns ->
    let timestamp = Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn ns) in
    let encoded = Time_ns.to_string_utc timestamp in
    let expected_date = Date.of_string (String.sub encoded ~pos:0 ~len:10) in
    let expected_ofday =
      Time_ns.Ofday.of_string
        (String.sub encoded ~pos:11 ~len:(String.length encoded - 12))
    in
    let actual_date, actual_ofday = R.Zone.to_local full_range_utc timestamp |> ok in
    assert (Date.equal expected_date actual_date);
    assert (Time_ns.Ofday.equal expected_ofday actual_ofday);
    let cursor = R.Cursor.Interval.create ~first_due:timestamp in
    assert (
      R.Cursor.Interval.equal
        cursor
        (R.Cursor.Interval.of_json (R.Cursor.Interval.to_json cursor) |> ok)));
  print_endline "day-boundary neighborhoods and both timestamp extremes roundtrip exactly";
  [%expect {| day-boundary neighborhoods and both timestamp extremes roundtrip exactly |}]
;;

let%expect_test
    "live elapsed phases and restart wall reanchoring have distinct clock semantics"
  =
  let captured =
    R.Clock.snapshot ~wall:(time "2024-01-01 00:00:00Z") ~elapsed_ns:0L |> ok
  in
  let delay = period 1_000L in
  let anchor = R.Clock.after captured ~delay |> ok in
  let corrected =
    R.Clock.snapshot ~wall:(time "2023-01-01 00:00:00Z") ~elapsed_ns:1_000_000_000L |> ok
  in
  assert (R.Clock.is_due anchor corrected);
  let advanced = R.Clock.advance anchor ~period:delay ~count:1L |> ok in
  assert (not (R.Clock.is_due advanced corrected));
  let rate_rule = R.Rule.Interval.create ~mode:Fixed_rate ~period:delay in
  let first = R.Cursor.Interval.create ~first_due:(R.Clock.deadline anchor) in
  List.iter
    [ 0L, 0L
    ; 999_999_999L, 0L
    ; 1_000_000_000L, 1L
    ; 17_000_000_000L, 17L
    ; 9_000_000_000_000_000L, 9_000_000L
    ]
    ~f:(fun (elapsed_ns, expected_count) ->
      let baseline =
        R.Clock.snapshot ~wall:(time "2024-01-01 00:00:00Z") ~elapsed_ns |> ok
      in
      assert (Int64.equal expected_count (R.Clock.due_count anchor ~period:delay baseline));
      List.iter
        [ "2023-01-01 00:00:00Z"; "2024-01-01 00:00:00Z"; "2054-01-01 00:00:00Z" ]
        ~f:(fun wall ->
          let snapshot = R.Clock.snapshot ~wall:(time wall) ~elapsed_ns |> ok in
          let actual_count = R.Clock.due_count anchor ~period:delay snapshot in
          assert (Int64.equal expected_count actual_count);
          if Int64.(actual_count > 0L)
          then (
            let expected =
              R.Cursor.Interval.consume_count first ~rule:rate_rule ~count:expected_count
              |> ok
            in
            let actual =
              R.Cursor.Interval.consume_count first ~rule:rate_rule ~count:actual_count
              |> ok
            in
            assert (Int64.equal actual.last_slot expected.last_slot);
            assert (Option.equal R.Cursor.Interval.equal actual.next expected.next);
            let advanced =
              R.Clock.advance anchor ~period:delay ~count:actual_count |> ok
            in
            assert (not (R.Clock.is_due advanced snapshot)))));
  let restarted =
    R.Clock.snapshot ~wall:(time "2024-01-01 00:00:00.500Z") ~elapsed_ns:0L |> ok
  in
  let recovered = R.Clock.recover restarted ~deadline:(R.Clock.deadline anchor) |> ok in
  assert (not (R.Clock.is_due recovered restarted));
  let now =
    R.Clock.snapshot ~wall:(time "2024-01-01 00:00:00.500Z") ~elapsed_ns:500_000_000L
    |> ok
  in
  assert (R.Clock.is_due recovered now);
  let rule = R.Rule.Interval.create ~mode:Fixed_delay ~period:delay in
  let cursor =
    R.Cursor.Interval.after_completion
      (R.Cursor.Interval.create ~first_due:(time "2024-01-01 00:00:00Z"))
      ~rule
      ~completed_at:(time "2023-01-01 00:00:00Z")
    |> ok
  in
  assert (Time_ns.equal (R.Cursor.Interval.due_at cursor) (time "2023-01-01 00:00:01Z"));
  print_endline "wall rollback cannot reset live rate; recovery uses a new elapsed origin";
  [%expect {| wall rollback cannot reset live rate; recovery uses a new elapsed origin |}]
;;

let%expect_test "strict codecs, coverage and arithmetic reject invalid domain entry" =
  let rules =
    [ R.Rule.Interval (R.Rule.Interval.create ~mode:Fixed_rate ~period:(period 7L))
    ; Calendar
        (calendar ~fixture:utc ~start:"2024-01-01" ~ofday:"12:00" (Weekdays [ Mon; Fri ]))
    ; Calendar
        (calendar
           ~gap:At_gap_end
           ~repeat:Both
           ~fixture:apia
           ~start:"2011-12-29"
           ~ofday:"00:00"
           Daily)
    ]
  in
  List.iter rules ~f:(fun rule ->
    let json = R.Rule.to_json rule in
    assert (R.Rule.equal rule (R.Rule.of_json json |> ok));
    match json with
    | `Object fields ->
      assert (
        Result.is_error (R.Rule.of_json (`Object (("version", `Number "1") :: fields))))
    | _ -> assert false);
  let calendar_json = R.Rule.to_json (List.nth_exn rules 1) in
  (match calendar_json with
   | `Object fields ->
     List.iter [ "start_date"; "ofday" ] ~f:(fun changed ->
       let huge = `String (String.make 10_000 '0') in
       let forged =
         `Object
           (List.map fields ~f:(fun (key, value) ->
              key, if String.equal key changed then huge else value))
       in
       assert (Result.is_error (R.Rule.of_json forged)));
     assert (
       Result.is_error
         (R.Rule.of_json (`Object (fields @ List.init 10_000 ~f:(fun _ -> "x", `Null)))))
   | _ -> assert false);
  List.iter [ 0L; -1L; Int64.max_value ] ~f:(fun milliseconds ->
    assert (Result.is_error (R.Rule.Period.create_ms milliseconds)));
  let maximum = Time_ns.of_int63_ns_since_epoch Int63.max_value in
  let minimum = Time_ns.of_int63_ns_since_epoch Int63.min_value in
  List.iter [ minimum; maximum ] ~f:(fun at ->
    let date, ofday = R.Zone.to_local full_range_utc at |> ok in
    assert (
      Result.equal
        R.Zone.Resolution.equal
        R.Timing_error.equal
        (R.Zone.resolve full_range_utc ~date ~ofday)
        (Error Out_of_coverage)));
  let anchor = R.Clock.snapshot ~wall:maximum ~elapsed_ns:0L |> ok in
  assert (Result.is_error (R.Clock.after anchor ~delay:(period 1L)));
  let rule = R.Rule.Interval.create ~mode:Fixed_rate ~period:(period 1L) in
  let due =
    R.Cursor.Interval.due_through
      (R.Cursor.Interval.create ~first_due:maximum)
      ~rule
      ~now:maximum
    |> ok
    |> Option.value_exn
  in
  assert (Option.is_none due.next);
  let at_limit = R.Cursor.Interval.create ~first_due:maximum in
  assert (Result.is_error (R.Cursor.Interval.consume_count at_limit ~rule ~count:0L));
  assert (Result.is_error (R.Cursor.Interval.consume_count at_limit ~rule ~count:2L));
  assert (
    Result.equal
      R.Zone.Resolution.equal
      R.Timing_error.equal
      (R.Zone.resolve utc.zone ~date:(date "2000-01-01") ~ofday:(ofday "12:00"))
      (Error Out_of_coverage));
  assert (Result.is_error (R.Zone.validate_name "../UTC"));
  let forged_cursor at =
    `Object [ "version", `Number "1"; "slot", `Number "0"; "due_at", `String at ]
  in
  List.iter
    [ "9999-12-31T12:00:00Z"
    ; "0001-01-01T12:00:00Z"
    ; "2024-01-01T12:00:60Z"
    ; "2024-01-01T24:00:00Z"
    ]
    ~f:(fun encoded ->
      assert (Result.is_error (R.Cursor.Interval.of_json (forged_cursor encoded))));
  assert (
    Result.is_error
      (R.Rule.Calendar.create
         ~pattern:(Month_days [ 1; 1 ])
         ~ofday:(ofday "12:00")
         ~zone:"UTC"
         ~start_date:(date "2024-01-01")
         ()));
  print_endline "validated codecs, explicit zone coverage and exact endpoint checks";
  [%expect {| validated codecs, explicit zone coverage and exact endpoint checks |}]
;;
