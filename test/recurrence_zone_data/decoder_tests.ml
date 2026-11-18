open Core
open Fixtures
module R = Recurrence_rules
module S = Synthetic
module M = Calendar_model

let decode bytes = D.of_string bytes ~source:"controlled"
let rejected bytes = assert (Result.is_error (decode bytes))
let project data from until = D.zone data ~name:"Fixture/Typed" ~from ~until

let assert_offset bytes at expected =
  let zone = zone bytes in
  let actual = offset zone (time at) in
  if actual <> expected
  then raise_s [%sexp (at : string), (expected : int), (actual : int)]
;;

let candidates bytes day at expected =
  let actual =
    Z.resolve (zone bytes) ~date:(Date.of_string day) ~ofday:(Time_ns.Ofday.of_string at)
    |> timing_ok
  in
  match actual with
  | Candidates actual ->
    assert (List.equal Time_ns.equal actual (List.map expected ~f:time))
  | Gap _ -> failwith "expected exact repeated-label candidates"
;;

let gap bytes day at expected =
  match
    Z.resolve (zone bytes) ~date:(Date.of_string day) ~ofday:(Time_ns.Ofday.of_string at)
    |> timing_ok
  with
  | Gap { ends_at } -> assert (Time_ns.equal ends_at (time expected))
  | Candidates _ -> failwith "expected proved missing civil label"
;;

let%expect_test "pinned real TZif future data, inverse candidates and calendar cursor" =
  List.iter all ~f:(fun (_, bytes, sha256) ->
    assert (String.equal (Z.Provenance.sha256 (D.provenance (data bytes))) sha256));
  List.iter
    [ new_york, "2050-03-13 06:59:59Z", -18000
    ; new_york, "2050-03-13 07:00:00Z", -14400
    ; new_york, "2050-11-06 06:00:00Z", -18000
    ; lord_howe, "2050-04-02 14:59:59Z", 39600
    ; lord_howe, "2050-04-02 15:00:00Z", 37800
    ; lord_howe, "2050-10-01 15:30:00Z", 39600
    ; santiago, "2050-09-04 03:59:59Z", -14400
    ; santiago, "2050-09-04 04:00:00Z", -10800
    ; gaza, "2100-03-27 00:00:00Z", 10800
    ; gaza, "2100-10-29 23:00:00Z", 7200
    ; dublin, "2050-03-27 01:00:00Z", 3600
    ; dublin, "2050-10-30 01:00:00Z", 0
    ; utc, "2100-01-01 00:00:00Z", 0
    ; apia, "2011-12-30 09:59:59Z", -36000
    ; apia, "2011-12-30 10:00:00Z", 50400
    ]
    ~f:(fun (bytes, at, expected) -> assert_offset bytes at expected);
  candidates
    new_york
    "2050-11-06"
    "01:30"
    [ "2050-11-06 05:30:00Z"; "2050-11-06 06:30:00Z" ];
  candidates
    lord_howe
    "2050-04-03"
    "01:45"
    [ "2050-04-02 14:45:00Z"; "2050-04-02 15:15:00Z" ];
  candidates
    dublin
    "2050-10-30"
    "01:30"
    [ "2050-10-30 00:30:00Z"; "2050-10-30 01:30:00Z" ];
  gap santiago "2050-09-04" "00:30" "2050-09-04 04:00:00Z";
  gap apia "2011-12-30" "12:00" "2011-12-30 10:00:00Z";
  let zone = zone lord_howe in
  let rule =
    R.Rule.Calendar.create
      ~repeat:Both
      ~pattern:Daily
      ~ofday:(Time_ns.Ofday.of_string "01:45")
      ~zone:(Z.name zone)
      ~start_date:(Date.of_string "2050-04-03")
      ~end_date:(Date.of_string "2050-04-03")
      ()
    |> timing_ok
  in
  let rec collect cursor remaining =
    assert (remaining > 0);
    match R.Cursor.Calendar.next cursor ~rule ~zone ~max_steps:1 |> timing_ok with
    | End -> []
    | Continue cursor -> collect cursor (remaining - 1)
    | Found (row, cursor) -> row.at :: collect cursor (remaining - 1)
  in
  assert (
    List.equal
      Time_ns.equal
      (collect (R.Cursor.Calendar.create rule) 20)
      (List.map [ "2050-04-02 14:45:00Z"; "2050-04-02 15:15:00Z" ] ~f:time));
  print_endline
    "exact provenance; future /24 and /50; half-hour folds; skipped midnight/day; cursor";
  [%expect
    {| exact provenance; future /24 and /50; half-hour folds; skipped midnight/day; cursor |}]
;;

let%expect_test "binary grammar and bounded malformed-input matrix" =
  let basic = S.make "STD0" in
  for length = 0 to String.length basic - 1 do
    rejected (String.prefix basic length)
  done;
  let first_block =
    S.block ~version:'3' ~width:4 ~types:[ 0, false, "STD" ] ~transitions:[]
  in
  let second = String.length first_block in
  List.iter
    [ 0, "BAD!"
    ; 5, "X"
    ; 20, S.integer 1L 4
    ; 24, S.integer 2L 4
    ; 28, S.integer 1L 4
    ; 32, S.integer 0xffff_ffffL 4
    ; 36, S.integer 0L 4
    ; 36, S.integer 257L 4
    ; 40, S.integer 65537L 4
    ; 48, "\002"
    ; 49, "\255"
    ; 53, "X"
    ; second, "BAD!"
    ; second + 4, "2"
    ; second + 28, S.integer 1L 4
    ; second + 32, S.integer 4097L 4
    ; second + 40, S.integer 0L 4
    ]
    ~f:(fun (at, replacement) -> rejected (S.replace basic ~at replacement));
  List.iter [ '\000'; '1'; '4'; '9' ] ~f:(fun version ->
    match decode (S.make ~version "STD0") with
    | Error (Unsupported_version actual) -> assert (Char.equal actual version)
    | _ -> failwith "expected unsupported version");
  List.iter
    [ "STD0DST"
    ; ":external"
    ; "ST0"
    ; "<S!D>0"
    ; "STD0junk"
    ; "STD0DST,M0.1.0,M11.1.0"
    ; "STD0DST,M3.0.0,M11.1.0"
    ; "STD0DST,M3.1.7,M11.1.0"
    ; "STD0DST,J0,J300"
    ; "STD0DST,366,300"
    ; "STD0DST,J1/168,J300"
    ; "STD0DST,J1/2:60,J300"
    ; "STD0DST,J1/2:00:60,J300"
    ; "STD0DST,J1/2junk,J300"
    ; "STD0\000"
    ; "STD0\nUTC0"
    ]
    ~f:(fun footer -> rejected (S.make footer));
  List.iter
    [ "STD0DST,J1/-1,J300"; "STD0DST,J1/+1,J300"; "STD0DST,J1/25,J300" ]
    ~f:(fun footer -> rejected (S.make ~version:'2' footer));
  rejected (basic ^ "extra");
  rejected (S.make (String.make (D.maximum_footer_bytes + 1) 'X'));
  (match decode (String.make (D.maximum_bytes + 1) 'X') with
   | Error Too_large -> ()
   | _ -> failwith "input bound not enforced");
  rejected (S.make ~transitions:[ 0L, 0; 0L, 0 ] "");
  rejected (S.make ~transitions:[ 1L, 0; 0L, 0 ] "");
  rejected (S.make ~transitions:[ 0L, 1 ] "");
  rejected (S.make ~types:[ -2_147_483_648, false, "STD" ] "");
  List.iter [ "XXX0"; "STD1"; "STD0DST,J1/0,J365/0" ] ~f:(fun footer ->
    rejected (S.make ~transitions:[ 0L, 0 ] footer));
  let flags =
    S.replace basic ~at:(second + 20) (S.integer 1L 4)
    |> fun bytes -> S.with_footer bytes "STD0"
  in
  (* Declare a UTC indicator and insert it after the second designation table;
     no corresponding standard indicator exists, so the relation must reject. *)
  let footer_start = String.length flags - String.length "\nSTD0\n" in
  let flags =
    String.prefix flags footer_start ^ "\001" ^ String.drop_prefix flags footer_start
  in
  rejected flags;
  let bad_flags = S.replace flags ~at:footer_start "\002" in
  rejected bad_flags;
  print_endline
    "truncations, bounded counts, type/designation/flag relations, strict footer and \
     versions";
  [%expect
    {| truncations, bounded counts, type/designation/flag relations, strict footer and versions |}]
;;

let%expect_test "truthful finite coverage and signed-second/nanosecond boundaries" =
  let point = time "1970-01-01 00:00:00Z" in
  let before = of_ns (-1L) in
  let after = of_ns 1L in
  let first_dst =
    S.make ~types:[ 1800, true, "DST"; 0, false, "STD" ] ~transitions:[ 0L, 1 ] "" |> data
  in
  assert (offset (project first_dst before before |> ok) before = 1800);
  assert (Result.is_error (project first_dst before point));
  assert (Result.is_error (project first_dst point after));
  let fixed = S.make ~types:[ 1234, true, "FIX" ] "" |> data in
  let minimum = Time_ns.of_int63_ns_since_epoch Int63.min_value in
  let maximum = Time_ns.of_int63_ns_since_epoch Int63.max_value in
  let fixed = project fixed minimum maximum |> ok in
  assert (offset fixed minimum = 1234 && offset fixed maximum = 1234);
  let unknown = S.make ~types:[ 0, false, "-00" ] "" |> data in
  assert (Result.is_error (project unknown point point));
  let hole =
    S.make
      ~types:[ 0, false, "STD"; 0, false, "-00" ]
      ~transitions:[ 0L, 1; 10L, 0; 20L, 0 ]
      ""
    |> data
  in
  assert (Result.is_error (project hole before (second 15L)));
  assert (offset (project hole (second 10L) (second 19L) |> ok) (second 10L) = 0);
  let outside =
    S.make
      ~types:[ 90000, false, "BIG"; 0, false, "STD" ]
      ~transitions:[ 0L, 1; 20L, 1 ]
      ""
    |> data
  in
  assert (Result.is_error (project outside before before));
  assert (offset (project outside point (second 19L) |> ok) point = 0);
  (* Outlandish raw seconds establish initial state without multiplication. *)
  let extremes =
    S.make
      ~types:[ 0, false, "STD"; 1800, true, "DST" ]
      ~transitions:[ Int64.min_value, 1; Int64.max_value, 0 ]
      ""
    |> data
  in
  assert (offset (project extremes minimum maximum |> ok) point = 1800);
  let tail =
    S.make
      ~types:[ 0, false, "STD"; 3600, true, "DST" ]
      ~transitions:[ 0L, 0 ]
      "STD0DST,M3.2.0,M11.1.0"
    |> data
  in
  assert (offset (project tail before after |> ok) before = 0);
  assert (offset (project tail before after |> ok) point = 0);
  assert (Result.is_error (D.zone tail ~name:"../UTC" ~from:point ~until:point));
  assert (Result.is_error (project tail after before));
  assert (Result.is_error (D.of_string (S.make "STD0") ~source:(String.make 257 'x')));
  let bounded_rows =
    List.init D.maximum_transitions ~f:(fun index ->
      Int64.of_int (index - D.maximum_transitions + 1), if index % 2 = 0 then 1 else 0)
  in
  let full =
    S.make
      ~types:[ 0, false, "STD"; 3600, true, "DST" ]
      ~transitions:bounded_rows
      "STD0DST,M3.2.0,M11.1.0"
    |> data
  in
  let lower = second (Int64.of_int (-D.maximum_transitions)) in
  assert (Result.is_ok (project full lower point));
  (match project full lower (time "1971-01-01 00:00:00Z") with
   | Error Projection_limit -> ()
   | _ -> failwith "projected transition bound");
  let endpoint_zone = project tail minimum maximum |> ok in
  let lower, upper = Z.coverage endpoint_zone in
  assert (Time_ns.equal lower minimum && Time_ns.equal upper maximum);
  let crossing = "STD0DST,J1/-167,J200/167" in
  (* Captured pinned reference observations include both cycle boundaries and
     arbitrary signed64-bit instants reduced with Python's unbounded integers.
     Exact DST/designation, not offset alone, must match the final table type. *)
  List.iter
    [ Int64.min_value, (3600, true, "DST")
    ; Int64.max_value, (0, false, "STD")
    ; 13_569_465_600L, (3600, true, "DST")
    ]
    ~f:(fun (at, local_type) ->
      let bytes = S.make ~types:[ local_type ] ~transitions:[ at, 0 ] crossing in
      assert (Result.is_ok (decode bytes));
      let offset, dst, name = local_type in
      List.iter
        [ offset + 1, dst, name; offset, not dst, name; offset, dst, "BAD" ]
        ~f:(fun wrong ->
          rejected (S.make ~types:[ wrong ] ~transitions:[ at, 0 ] crossing)));
  List.iter
    [ "XXX3EDT4,0/0,J365/23", -14400; "STD0DST,J1/-167,J365/167", 3600 ]
    ~f:(fun (footer, expected) ->
      let zone = S.make footer |> zone in
      List.iter
        [ "1999-12-31 23:59:59Z"; "2000-01-01 00:00:00Z"; "2000-07-01 00:00:00Z" ]
        ~f:(fun at -> assert (offset zone (time at) = expected)));
  let cycle = S.make crossing |> zone in
  List.iter
    [ "1999-12-25 00:59:59Z", 0
    ; "1999-12-25 01:00:00Z", 3600
    ; "2000-01-01 00:00:00Z", 3600
    ; "2000-07-25 22:00:00Z", 0
    ]
    ~f:(fun (at, expected) -> assert (offset cycle (time at) = expected));
  print_endline
    "type0, fixed/no-transition, -00 holes, no final-offset extrapolation, full Time_ns \
     endpoints";
  [%expect
    {| type0, fixed/no-transition, -00 holes, no final-offset extrapolation, full Time_ns endpoints |}]
;;

let%expect_test "footer offsets match independent Gregorian interval oracle" =
  let rule day seconds = { M.day; seconds } in
  let cases =
    List.concat_map [ 59; 60; 365 ] ~f:(fun day ->
      [ ( Printf.sprintf "STD0DST-0:30,J%d/1:02:03,J200/4:05:06" day
        , { M.standard = 0
          ; daylight = 1800
          ; start = rule (Julian day) 3723
          ; finish = rule (Julian 200) 14706
          } )
      ; ( Printf.sprintf "STD0DST,%d,200" day
        , { M.standard = 0
          ; daylight = 3600
          ; start = rule (Ordinal day) 7200
          ; finish = rule (Ordinal 200) 7200
          } )
      ])
    @ [ ( "STD-1DST0,M10.5.0,M3.5.0/1"
        , { M.standard = 3600
          ; daylight = 0
          ; start = rule (Month (10, 5, 0)) 7200
          ; finish = rule (Month (3, 5, 0)) 3600
          } )
      ; ( "<+1030>-10:30<+11>-11,M10.1.0,M4.1.0"
        , { M.standard = 37800
          ; daylight = 39600
          ; start = rule (Month (10, 1, 0)) 7200
          ; finish = rule (Month (4, 1, 0)) 7200
          } )
      ; ( "STD+2:03:04DST+1:03:04,M3.5.0/-167:59:59,M10.5.0/167:59:59"
        , { M.standard = -7384
          ; daylight = -3784
          ; start = rule (Month (3, 5, 0)) (-604799)
          ; finish = rule (Month (10, 5, 0)) 604799
          } )
      ; ( "XXX3EDT4,0/0,J365/23"
        , { M.standard = -10800
          ; daylight = -14400
          ; start = rule (Ordinal 0) 0
          ; finish = rule (Julian 365) 82800
          } )
      ; ( "STD0DST,J1/-167,J365/167"
        , { M.standard = 0
          ; daylight = 3600
          ; start = rule (Julian 1) (-601200)
          ; finish = rule (Julian 365) 601200
          } )
      ]
  in
  List.iter cases ~f:(fun (footer, model) ->
    let zone = S.make footer |> zone in
    List.iter [ 1999; 2000; 2001; 2004; 2099; 2100 ] ~f:(fun year ->
      let points =
        [ M.at model.start year model.standard; M.at model.finish year model.daylight ]
      in
      List.iter points ~f:(fun at ->
        List.iter [ -1L; 0L; 1L ] ~f:(fun delta ->
          let probe = of_ns Int64.((at * 1_000_000_000L) + delta) in
          let whole = if Int64.(delta < 0L) then Int64.(at - 1L) else at in
          let expected = M.offset model ~year ~at:whole in
          let actual = offset zone probe in
          if actual <> expected
          then
            raise_s
              [%sexp
                (footer : string)
              , (year : int)
              , (at : int64)
              , (delta : int64)
              , (expected : int)
              , (actual : int)]))));
  print_endline
    "Jn/n leap rules; month/week5; defaults; reversed signs; cross-year times; \
     all-year/negative/nonhour DST";
  [%expect
    {| Jn/n leap rules; month/week5; defaults; reversed signs; cross-year times; all-year/negative/nonhour DST |}]
;;
