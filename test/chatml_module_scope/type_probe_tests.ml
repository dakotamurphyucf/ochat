open Core
module T = Chatml_typechecker
module Probe = T.Type_probe

let checkpoint () = ()
let no_rigids = String.Set.empty

let ok = function
  | Ok () -> ()
  | Error error -> raise_s [%sexp (error : Probe.Error.t)]
;;

let cells_of_types types =
  List.map types ~f:(function
    | T.Var cell -> cell
    | _ -> failwith "expected variable root")
;;

let free name = ref (T.Free { name; level = 0; equality = None })

let unchanged cells snapshots =
  List.iter2_exn cells snapshots ~f:(fun cell snapshot ->
    assert (phys_equal !cell snapshot))
;;

let edge target = T.Record (T.Row (T.Env.singleton "next" (T.Var target), T.Empty_row))

let%test_unit "copied and committed finite graphs preserve identity, sharing and cycles" =
  (* An independent graph model chooses either a free leaf or one labelled edge
     to any vertex. Exhaust every graph of up to four vertices, including
     shared targets, disconnected roots and cycles. *)
  for size = 1 to 4 do
    let rec graphs remaining reversed =
      if remaining > 0
      then
        for successor = -1 to size - 1 do
          graphs (remaining - 1) (successor :: reversed)
        done
      else (
        let successors = List.rev reversed in
        let originals = List.init size ~f:(fun index -> free (Int.to_string index)) in
        List.iter2_exn originals successors ~f:(fun cell successor ->
          if successor >= 0 then cell := T.Bound (edge (List.nth_exn originals successor)));
        let snapshots = List.map originals ~f:(fun cell -> !cell) in
        let probe =
          Probe.create (List.map originals ~f:(fun cell -> T.Var cell)) ~checkpoint
        in
        let copies = cells_of_types (Probe.types probe) in
        let inspect cells =
          List.iter2_exn cells successors ~f:(fun cell successor ->
            match !cell with
            | T.Free _ -> assert (successor < 0)
            | Bound (Record (Row (fields, Empty_row))) when successor >= 0 ->
              (match Map.find_exn fields "next" with
               | Var target -> assert (phys_equal target (List.nth_exn cells successor))
               | _ -> failwith "graph edge stopped referring to a variable")
            | _ -> failwith "copied graph differs from its reference model")
        in
        inspect copies;
        List.iter copies ~f:(fun copy ->
          assert (not (List.exists originals ~f:(phys_equal copy))));
        unchanged originals snapshots;
        Probe.commit probe ~rigid_names:no_rigids ~checkpoint |> ok;
        inspect originals;
        let committed = List.map originals ~f:(fun cell -> !cell) in
        List.iter copies ~f:(fun copy -> copy := Bound T.Boolean);
        unchanged originals committed;
        match Probe.commit probe ~rigid_names:no_rigids ~checkpoint with
        | Error Already_committed -> ()
        | _ -> failwith "probe committed twice")
    in
    graphs size []
  done
;;

let%test_unit
    "rigid escape and incompatible shared storage cannot mutate implementation types"
  =
  List.iter [ false; true ] ~f:(fun quantified ->
    let state = T.create_state () in
    let variable =
      if quantified
      then T.Generic { name = "a"; equality = None }
      else T.Var (free "weak")
    in
    let implementation = T.Fun ([], T.Array variable) in
    let probe = Probe.create [ implementation ] ~checkpoint in
    let rigid = T.Generic { name = "specification"; equality = None } in
    T.unify
      state
      (T.instantiate state (List.hd_exn (Probe.types probe)))
      (T.Fun ([], T.Array rigid));
    match
      Probe.commit probe ~rigid_names:(String.Set.singleton "specification") ~checkpoint
    with
    | Ok () -> assert quantified
    | Error (Escaping_parameter name) ->
      assert (not quantified);
      [%test_eq: string] name "specification";
      (match variable with
       | Var { contents = Free _ } -> ()
       | _ -> failwith "rejected universal claim changed shared storage")
    | Error error -> raise_s [%sexp (error : Probe.Error.t)]);
  let original = free "shared" in
  let before = !original in
  let probe =
    Probe.create [ T.Array (T.Var original); T.Array (T.Var original) ] ~checkpoint
  in
  let copies = Probe.types probe in
  let state = T.create_state () in
  T.unify state (List.nth_exn copies 0) (T.Array T.TInt);
  (match T.unify state (List.nth_exn copies 1) (T.Array T.Boolean) with
   | () -> failwith "two copies of shared storage specialized independently"
   | exception T.Type_error _ -> ());
  assert (phys_equal !original before)
;;

let%test_unit "commit owns fresh probe variables and rejects stale originals" =
  let original = free "original" in
  let probe = Probe.create [ T.Var original ] ~checkpoint in
  let copied = List.hd_exn (cells_of_types (Probe.types probe)) in
  let fresh = free "probe-only" in
  copied := T.Bound (T.Array (T.Var fresh));
  Probe.commit probe ~rigid_names:no_rigids ~checkpoint |> ok;
  (match !original with
   | Bound (Array (Var committed)) ->
     assert (not (phys_equal fresh committed));
     fresh := Bound T.String;
     (match !committed with
      | Free _ -> ()
      | _ -> failwith "probe retained mutation authority")
   | _ -> failwith "missing committed constraint");
  let probe = Probe.create [ T.Var original ] ~checkpoint in
  original := T.Bound T.Boolean;
  let updated = !original in
  (match Probe.commit probe ~rigid_names:no_rigids ~checkpoint with
   | Error Original_changed -> ()
   | _ -> failwith "stale probe overwrote newer inference");
  assert (phys_equal !original updated)
;;

let%test_unit "cancellation at every commit checkpoint leaves every original untouched" =
  let make () =
    let originals = [ free "first"; free "second" ] in
    let probe =
      Probe.create (List.map originals ~f:(fun cell -> T.Var cell)) ~checkpoint
    in
    let copies = cells_of_types (Probe.types probe) in
    let fresh = free "fresh" in
    fresh := T.Bound (edge (List.nth_exn copies 0));
    List.nth_exn copies 0 := T.Bound (T.Array (T.Var fresh));
    List.nth_exn copies 1 := T.Bound T.TInt;
    originals, probe
  in
  let polls = ref 0 in
  let _, probe = make () in
  Probe.commit probe ~rigid_names:no_rigids ~checkpoint:(fun () -> Int.incr polls) |> ok;
  let checkpoints = !polls in
  assert (checkpoints > 0);
  for interrupt = 1 to checkpoints do
    let originals, probe = make () in
    let snapshots = List.map originals ~f:(fun cell -> !cell) in
    let polls = ref 0 in
    let exception Cancelled in
    match
      Probe.commit probe ~rigid_names:no_rigids ~checkpoint:(fun () ->
        Int.incr polls;
        if !polls = interrupt then raise Cancelled)
    with
    | exception Cancelled -> unchanged originals snapshots
    | _ -> failwith "commit skipped a cancellation checkpoint"
  done
;;
