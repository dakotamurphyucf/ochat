open Core
module R = Recurrence_rules

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : R.Timing_error.t)]
;;

let time encoded = Time_ns.of_string_with_utc_offset encoded
let date encoded = Date.of_string encoded
let ofday encoded = Time_ns.Ofday.of_string encoded
let period milliseconds = R.Rule.Period.create_ms milliseconds |> ok
let ns time = Time_ns.to_int63_ns_since_epoch time |> Int63.to_int64

type zone_fixture =
  { zone : R.Zone.t
  ; initial : int
  ; shifts : (Time_ns.t * int) list
  }

let zone
      ?(from = "2000-01-01 00:00:00Z")
      ?(until = "2100-01-01 00:00:00Z")
      name
      initial
      shifts
  =
  let shifts = List.map shifts ~f:(fun (at, offset) -> time at, offset) in
  let provenance =
    R.Zone.Provenance.create ~source:"controlled-fixture-v1" ~sha256:(String.make 64 'a')
    |> ok
  in
  let zone =
    R.Zone.create
      ~name
      ~provenance
      ~from:(time from)
      ~until:(time until)
      ~initial_offset_seconds:initial
      ~transitions:
        (List.map shifts ~f:(fun (at, offset_seconds) ->
           { R.Zone.Transition.at; offset_seconds }))
    |> ok
  in
  { zone; initial; shifts }
;;

let utc = zone "UTC" 0 []

let full_range_utc =
  R.Zone.create
    ~name:"Fixture/Edges"
    ~provenance:(R.Zone.provenance utc.zone)
    ~from:(Time_ns.of_int63_ns_since_epoch Int63.min_value)
    ~until:(Time_ns.of_int63_ns_since_epoch Int63.max_value)
    ~initial_offset_seconds:0
    ~transitions:[]
  |> ok
;;

let chicago =
  zone
    "America/Chicago"
    (-21_600)
    [ "2024-03-10 08:00:00Z", -18_000; "2024-11-03 07:00:00Z", -21_600 ]
;;

let lord_howe =
  zone
    "Australia/Lord_Howe"
    39_600
    [ "2024-04-06 15:00:00Z", 37_800; "2024-10-05 15:30:00Z", 39_600 ]
;;

let midnight =
  zone "Fixture/Midnight" 0 [ "2024-06-01 00:00:00Z", 3_600; "2024-10-01 00:00:00Z", 0 ]
;;

let apia = zone "Pacific/Apia" (-36_000) [ "2011-12-30 10:00:00Z", 50_400 ]

let multiple =
  zone
    "Fixture/Multiple"
    0
    [ "2024-06-01 00:00:00Z", 3_600
    ; "2024-06-01 01:00:00Z", 0
    ; "2024-06-01 02:00:00Z", 1_800
    ]
;;

let interleaved = zone "Fixture/Interleaved" 86_400 [ "2024-06-02 00:00:00Z", -86_400 ]

(** Independent oracle: enumerate UTC minutes, then apply fixture regimes in
    forward direction. Production enumerates distinct offsets and solves inverse
    candidates. Fixtures intentionally have minute-aligned offsets/local labels. *)
let candidates fixture ~date ~ofday =
  let local =
    Time_ns.of_string_with_utc_offset
      (Date.to_string date ^ " " ^ Time_ns.Ofday.to_string ofday ^ "Z")
    |> ns
  in
  List.range (-1440) 1441
  |> List.filter_map ~f:(fun minute ->
    let candidate_ns = Int64.(local + (of_int minute * 60_000_000_000L)) in
    let candidate = Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn candidate_ns) in
    let offset =
      List.fold fixture.shifts ~init:fixture.initial ~f:(fun offset (at, next) ->
        if Time_ns.compare candidate at >= 0 then next else offset)
    in
    Option.some_if
      Int64.(candidate_ns + (of_int offset * 1_000_000_000L) = local)
      candidate)
;;

let calendar ?gap ?repeat ?end_date ~fixture ~start ~ofday:at pattern =
  R.Rule.Calendar.create
    ?gap
    ?repeat
    ?end_date
    ~pattern
    ~ofday:(ofday at)
    ~zone:(R.Zone.name fixture.zone)
    ~start_date:(date start)
    ()
  |> ok
;;

let rec collect ?(restore = false) cursor rule zone ~steps ~remaining =
  if remaining <= 0 then failwith "search did not terminate within test limit";
  let cursor =
    if restore
    then R.Cursor.Calendar.of_json (R.Cursor.Calendar.to_json cursor) |> ok
    else cursor
  in
  match R.Cursor.Calendar.next cursor ~rule ~zone ~max_steps:steps |> ok with
  | End -> []
  | Continue cursor -> collect ~restore cursor rule zone ~steps ~remaining:(remaining - 1)
  | Found (occurrence, cursor) ->
    occurrence :: collect ~restore cursor rule zone ~steps ~remaining:(remaining - 1)
;;
