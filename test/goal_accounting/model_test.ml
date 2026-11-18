open! Core
open Fixture

(* Test-only unbounded history: retain individual facts forever and recompute
   directly. This deliberately does not use production projection/aggregate code. *)
type cell =
  | Actual of int
  | Estimated of int * bool
  | Missing of O.Count.unknown_reason
[@@deriving equal]

type state =
  | Prepared
  | Running
  | Ended
[@@deriving equal]

type row =
  { handle : L.Handle.t
  ; tracked : bool
  ; sealed : bool
  ; state : state
  ; latest : (int64 * cell list) option
  }

let reasons =
  [ O.Count.Not_reported; Explicit_null; Interrupted; Not_submitted; Before_tracking ]
;;

let cells seed step =
  [ Actual ((seed + step) mod 17)
  ; Estimated (((seed * 3) + step) mod 13, step mod 2 = 0)
  ; Missing Not_reported
  ; Actual 0
  ; Missing Explicit_null
  ; (if step mod 3 = 0 then Missing Interrupted else Actual (step mod 5))
  ]
;;

let views cells =
  List.zip_exn components cells
  |> List.map ~f:(fun (component, cell) ->
    let view =
      match cell with
      | Actual tokens -> O.Count.Actual (Int64.of_int tokens)
      | Estimated (tokens, tag) ->
        let estimator =
          O.Estimator.create
            ~method_:(if tag then Utf8_heuristic else O200k_serialized_history)
            ~version:(if tag then 1 else 2)
          |> observed
        in
        O.Count.Estimated { tokens = Int64.of_int tokens; estimator }
      | Missing reason -> O.Count.Unknown reason
    in
    component, view)
;;

let selected row index =
  match row.latest with
  | Some (_, cells) -> List.nth_exn cells index
  | None ->
    Missing
      (match row.state with
       | Prepared -> Not_submitted
       | Running -> Not_reported
       | Ended -> Interrupted)
;;

let verify accounting current rows ~window =
  let rows = Map.data rows in
  assert (Int64.equal (A.admitted_attempts accounting) (Int64.of_int (List.length rows)));
  assert (Int64.equal (A.admitted_turns accounting) 0L);
  let live = List.count rows ~f:(fun row -> row.tracked && not row.sealed) in
  assert (List.length (A.bindings accounting) = live && live <= window);
  let totals = A.usage accounting ~ledger:current |> account in
  List.iteri components ~f:(fun index component ->
    let contributions =
      List.filter_map rows ~f:(fun row ->
        if row.tracked then Some (selected row index) else None)
    in
    let actual =
      List.filter_map contributions ~f:(function
        | Actual tokens -> Some tokens
        | Estimated _ | Missing _ -> None)
    in
    let estimates =
      List.filter_map contributions ~f:(function
        | Estimated (tokens, tag) -> Some (tokens, tag)
        | Actual _ | Missing _ -> None)
    in
    let metric = U.component totals component in
    assert (
      U.Sum.equal_sum
        (U.Metric.actual metric)
        (Tokens (Int64.of_int (List.fold actual ~init:0 ~f:( + )))));
    assert (
      Int64.equal (U.Metric.actual_attempts metric) (Int64.of_int (List.length actual)));
    assert (
      U.Sum.equal_sum
        (U.Metric.estimated metric)
        (Tokens
           (Int64.of_int
              (List.fold estimates ~init:0 ~f:(fun total (tokens, _) -> total + tokens)))));
    assert (
      Int64.equal
        (U.Metric.estimated_attempts metric)
        (Int64.of_int (List.length estimates)));
    let true_tag = List.exists estimates ~f:(fun (_, tag) -> tag) in
    let false_tag = List.exists estimates ~f:(fun (_, tag) -> not tag) in
    assert (Bool.equal (U.Metric.mixed_estimators metric) (true_tag && false_tag));
    List.iter reasons ~f:(fun reason ->
      let expected =
        List.count contributions ~f:(function
          | Missing other -> O.Count.equal_unknown_reason reason other
          | Actual _ | Estimated _ -> false)
      in
      assert (
        Int64.equal
          (U.Unknown.count (U.Metric.unknown metric) reason)
          (Int64.of_int expected)));
    assert (
      Int64.equal
        (U.Metric.untracked_attempts metric)
        (Int64.of_int (List.count rows ~f:(fun row -> not row.tracked)))))
;;

let%expect_test "bounded canonical traces match independent unbounded latest-fact history"
  =
  List.iter [ 1; 2; 3 ] ~f:(fun window ->
    List.iter (List.init 6 ~f:Fn.id) ~f:(fun seed ->
      let goal, episode = goal () in
      let current = ref (make_ledger ~attempts:window ()) in
      let accounting = ref (A.create goal ~ledger:!current |> account) in
      let rows = ref Int64.Map.empty in
      List.iter (List.init 36 ~f:Fn.id) ~f:(fun step_index ->
        let action = (seed + step_index) mod 7 in
        let before = !current in
        let select () =
          let choices = Map.data !rows |> List.rev in
          List.nth_exn choices ((seed + step_index) mod List.length choices)
        in
        if Map.is_empty !rows || action = 0 || action = 6
        then (
          let after, handle, tracking = admit before in
          accounting
          := step
               !accounting
               goal
               before
               after
               (Some (Attempt { episode; handle; tracking }));
          rows
          := Map.map !rows ~f:(fun row ->
               if
                 row.tracked
                 && (not row.sealed)
                 && Option.is_none (L.find after ~ordinal:(L.Handle.ordinal row.handle))
               then (
                 assert (equal_state row.state Ended);
                 { row with sealed = true })
               else row);
          let tracked = L.equal_tracking tracking Tracked in
          rows
          := Map.set
               !rows
               ~key:(L.Handle.ordinal handle)
               ~data:
                 { handle
                 ; tracked
                 ; sealed = not tracked
                 ; state = Prepared
                 ; latest = None
                 };
          current := after)
        else if action = 2 || action = 3
        then (
          let chosen = select () in
          if chosen.tracked && (not chosen.sealed) && not (equal_state chosen.state Ended)
          then (
            let state = if action = 2 then Ended else Running in
            let actual = if action = 2 then interrupted else O.Attempt_record.Running in
            let after = L.set_state before chosen.handle actual |> ledger in
            accounting := step !accounting goal before after None;
            rows
            := Map.set
                 !rows
                 ~key:(L.Handle.ordinal chosen.handle)
                 ~data:{ chosen with state };
            current := after))
        else (
          let chosen = select () in
          let revision, incoming =
            match action, chosen.latest with
            | 4, Some (revision, old) -> revision, old
            | 5, Some (revision, old) ->
              if Int64.(revision > 0L)
              then Int64.pred revision, cells seed step_index
              else revision, old
            | 1, _ | 4, None | 5, None ->
              Int64.of_int (1 + (step_index / 3)), cells seed step_index
            | 0, _ | 2, _ | 3, _ | 6, _ -> assert false
            | _, _ -> assert false
          in
          let changed, conflict =
            if (not chosen.tracked) || chosen.sealed
            then false, false
            else (
              match chosen.latest with
              | None -> true, false
              | Some (previous, old) ->
                if Int64.(revision > previous)
                then true, false
                else if Int64.equal revision previous
                then false, not (List.equal equal_cell old incoming)
                else false, false)
          in
          match
            L.observe
              before
              chosen.handle
              (usage chosen.handle ~revision (views incoming))
          with
          | Error _ -> assert conflict
          | Ok (after, _) ->
            assert (not conflict);
            accounting := step !accounting goal before after None;
            if changed
            then
              rows
              := Map.set
                   !rows
                   ~key:(L.Handle.ordinal chosen.handle)
                   ~data:{ chosen with latest = Some (revision, incoming) };
            current := after);
        verify !accounting !current !rows ~window)));
  print_endline
    "18 traces ×36 steps: replacement, reordered/duplicate revisions, absence/state \
     changes, untracked and small-window sealing match independent history";
  [%expect
    {| 18 traces ×36 steps: replacement, reordered/duplicate revisions, absence/state changes, untracked and small-window sealing match independent history |}]
;;
