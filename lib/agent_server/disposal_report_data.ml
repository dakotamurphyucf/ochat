open! Core
module Failure = Agent_session.Callback_cleanup.Failure
module Diagnostic = Agent_session.Cleanup_diagnostic

type disposition =
  | Retained
  | Disposed
[@@deriving equal, sexp_of]

type t =
  { disposition : disposition
  ; diagnostics : Diagnostic.t list
  ; primary : Diagnostic.t option
  }

let unique diagnostics =
  List.fold diagnostics ~init:[] ~f:(fun reversed diagnostic ->
    if List.mem reversed diagnostic ~equal:Diagnostic.equal
    then reversed
    else diagnostic :: reversed)
  |> List.rev
;;

let create ?primary ~disposition diagnostics =
  let diagnostics = unique diagnostics in
  if
    Option.exists primary ~f:(fun primary ->
      not (List.mem diagnostics primary ~equal:Diagnostic.equal))
  then invalid_arg "legacy disposal primary is not an actual report group";
  { disposition; diagnostics; primary }
;;

let disposition t = t.disposition
let diagnostics t = t.diagnostics
let primary t = t.primary
let failures t = List.concat_map t.diagnostics ~f:Diagnostic.failures

(* A view of a child forwards the same original diagnostic groups. Status is
   supplied by the actual enclosing stage, never inferred from empty failures. *)
let combine ~disposition reports =
  create
    ?primary:(List.find_map reports ~f:(fun report -> report.primary))
    ~disposition
    (List.concat_map reports ~f:diagnostics)
;;

(* Legacy one-shot delivery never changes immutable reported failures. Mark all
   actual groups delivered even when the original primary hides secondaries;
   each remains inspectable through the reported API. Newly observed groups
   have their own delivery state and cannot be hidden by an earlier report. *)
let take_legacy_diagnostic_exn t =
  Diagnostic.take_legacy_diagnostic_exn ?primary:t.primary t.diagnostics
;;

let take_legacy_failure_exn t = Option.map (take_legacy_diagnostic_exn t) ~f:snd

let%expect_test "disposal reports forward one actual diagnostic group once" =
  let sentinel = Failure "same exception, independent actual stages" in
  let shared_trace = Stdlib.Printexc.get_callstack 8 in
  let final_trace = Stdlib.Printexc.get_callstack 8 in
  let shared_payload = [ Failure.Raised (sentinel, shared_trace) ] in
  let first = Diagnostic.create shared_payload in
  let second = Diagnostic.create shared_payload in
  let final = Diagnostic.create [ Failure.Raised (sentinel, final_trace) ] in
  let child = create ~disposition:Disposed [ first ] in
  let parent = combine ~disposition:Disposed [ child; child ] in
  let root =
    combine
      ~disposition:Disposed
      [ parent; create ~disposition:Disposed [ second; final ]; child ]
  in
  (match failures root with
   | [ Failure.Raised (first, first_raw)
     ; Failure.Raised (second, second_raw)
     ; Failure.Raised (final, final_raw)
     ] ->
     assert (phys_equal first sentinel);
     assert (phys_equal second sentinel);
     assert (phys_equal final sentinel);
     assert (phys_equal first_raw shared_trace);
     assert (phys_equal second_raw shared_trace);
     assert (phys_equal final_raw final_trace)
   | _ -> failwith "diagnostic group provenance or original order changed");
  assert (Int.equal (List.length (failures child)) 1);
  let delivered = create ~primary:final ~disposition:Disposed (diagnostics root) in
  (match take_legacy_failure_exn delivered with
   | Some (Failure.Raised (actual, trace)) ->
     assert (phys_equal actual sentinel);
     assert (phys_equal trace final_trace)
   | None | Some (Protocol _) -> failwith "actual legacy primary changed");
  assert (Option.is_none (take_legacy_failure_exn root));
  assert (Int.equal (List.length (failures root)) 3);
  let later = create ~disposition:Disposed [ Diagnostic.create shared_payload ] in
  (match take_legacy_failure_exn (combine ~disposition:Disposed [ root; later ]) with
   | Some (Failure.Raised (actual, trace)) ->
     assert (phys_equal actual sentinel);
     assert (phys_equal trace shared_trace)
   | None | Some (Protocol _) -> failwith "new actual failure was swallowed");
  (* The lifted carrier is the SAME server type. Invalid primary/domain
     validation cannot partly consume an earlier valid group. Reuse this actual
     provenance/delivery invariant instead of a second lower-module test copy. *)
  let unconsumed = Diagnostic.create shared_payload in
  (match
     Agent_session.Close_observation.create
       (Returned ())
       ~diagnostics:[ unconsumed ]
       ~primary:final
       ()
   with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "close observation accepted an unrelated primary");
  (match Diagnostic.take_legacy_diagnostic_exn ~primary:final [ unconsumed ] with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "legacy delivery accepted an unrelated primary");
  let foreign =
    Stdlib.Domain.spawn (fun () -> Diagnostic.create shared_payload) |> Stdlib.Domain.join
  in
  (match Diagnostic.take_legacy_diagnostic_exn [ unconsumed; foreign ] with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "foreign diagnostic was delivered");
  (match Diagnostic.take_legacy_diagnostic_exn [ unconsumed; unconsumed ] with
   | Some (actual, Failure.Raised (exn, trace)) ->
     assert (Diagnostic.equal actual unconsumed);
     assert (phys_equal exn sentinel && phys_equal trace shared_trace)
   | None | Some (_, Protocol _) -> failwith "validation consumed valid diagnostic");
  assert (Option.is_none (Diagnostic.take_legacy_diagnostic_exn [ unconsumed ]));
  print_endline
    "same carrier once; identical payloads stay distinct; legacy delivery preserves \
     reports";
  [%expect
    {| same carrier once; identical payloads stay distinct; legacy delivery preserves reports |}]
;;
