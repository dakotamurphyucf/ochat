open! Core

(* Test-only unbounded facts. Tree membership is fixed independently by the
   fixture. No branch prefix, nonce, contribution subtotal, or receipt is used
   to calculate expectations. *)
module Dimension = struct
  type t =
    | Attempt
    | Host_turn
  [@@deriving compare, equal, sexp]
end

module Occurrence = struct
  module T = struct
    type kind =
      | Attempt of int64
      | Host_turn of string
    [@@deriving compare, equal, sexp]

    type t =
      { leaf : string
      ; generation : int
      ; candidate_revision : int64
      ; kind : kind
      }
    [@@deriving compare, equal, sexp]
  end

  include T
  include Comparator.Make (T)

  let dimension t =
    match t.kind with
    | Attempt _ -> Dimension.Attempt
    | Host_turn _ -> Host_turn
  ;;
end

type status =
  | Pending
  | Included of { input_tokens : int64 }
  | No_writer

type t =
  { paths : string list String.Map.t
  ; facts : (Occurrence.t, status, Occurrence.comparator_witness) Map.t
  ; unknown_edges : String.Set.t
  }

type totals =
  { committed : int64
  ; pending : int64
  ; input_tokens : int64
  ; coverage_unknown : bool
  }
[@@deriving equal, sexp_of]

let create ~paths ~unknown_edges =
  let paths = String.Map.of_alist_exn paths in
  List.iter unknown_edges ~f:(fun edge -> assert (Map.mem paths edge));
  { paths
  ; facts = Map.empty (module Occurrence)
  ; unknown_edges = String.Set.of_list unknown_edges
  }
;;

(* The test records the action's factual intended outcome after checking the SUT
   response. These setters deliberately do not reproduce production admissibility
   or revision logic; failed/duplicate/stale SUT operations do not change facts. *)
let record t ~occurrence ~status =
  assert (Map.mem t.paths occurrence.Occurrence.leaf);
  { t with facts = Map.set t.facts ~key:occurrence ~data:status }
;;

let observe_edge t edge = { t with unknown_edges = Set.remove t.unknown_edges edge }
let belongs t ~root leaf = List.mem (Map.find_exn t.paths leaf) root ~equal:String.equal

let totals t ~root ~dimension =
  let coverage_unknown = Set.exists t.unknown_edges ~f:(belongs t ~root) in
  Map.fold
    t.facts
    ~init:{ committed = 0L; pending = 0L; input_tokens = 0L; coverage_unknown }
    ~f:(fun ~key:occurrence ~data:status total ->
      if
        (not (belongs t ~root occurrence.leaf))
        || not (Dimension.equal dimension (Occurrence.dimension occurrence))
      then total
      else (
        match status with
        | Pending -> { total with pending = Int64.succ total.pending }
        | No_writer -> total
        | Included { input_tokens } ->
          { total with
            committed = Int64.succ total.committed
          ; input_tokens = Int64.(total.input_tokens + input_tokens)
          }))
;;

let occupied total = Int64.(total.committed + total.pending)

(* Only the test trace uses this helper: it checks fresh identities returned by
   SUT; it does not allocate or predict nonces. Do not call on exact replay. *)
let record_fresh_nonce previously_issued nonce =
  assert (Int64.(nonce > 0L));
  assert (not (Set.mem previously_issued nonce));
  Set.add previously_issued nonce
;;

(* One illustrative model self-check; the acceptance evidence must additionally
   compare real Goal_descendants/Child_contributions results after each action. *)
let%expect_test "independent facts count each occurrence once on its fixed root path" =
  let model =
    create
      ~paths:[ "leaf", [ "A"; "B" ]; "other", [ "C" ]; "unrelated", [] ]
      ~unknown_edges:[ "other"; "unrelated" ]
  in
  let occurrence : Occurrence.t =
    { leaf = "leaf"; generation = 0; candidate_revision = 1L; kind = Attempt 1L }
  in
  let pending = record model ~occurrence ~status:Pending in
  let included = record pending ~occurrence ~status:(Included { input_tokens = 7L }) in
  let corrected = record included ~occurrence ~status:(Included { input_tokens = 3L }) in
  let pending_total = totals pending ~root:"A" ~dimension:Attempt in
  let included_total = totals included ~root:"A" ~dimension:Attempt in
  assert (Int64.equal (occupied pending_total) (occupied included_total));
  List.iter [ "A"; "B"; "C" ] ~f:(fun root ->
    print_s [%sexp (root : string), (totals corrected ~root ~dimension:Attempt : totals)]);
  [%expect
    {|
    (A ((committed 1) (pending 0) (input_tokens 3) (coverage_unknown false)))
    (B ((committed 1) (pending 0) (input_tokens 3) (coverage_unknown false)))
    (C ((committed 0) (pending 0) (input_tokens 0) (coverage_unknown true)))
    |}]
;;
