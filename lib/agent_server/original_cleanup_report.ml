open! Core

module Segment = struct
  type t = Disposal_report_data.Diagnostic.t

  let create = Disposal_report_data.Diagnostic.create
end

type t =
  { pieces : piece list
  ; diagnostics_complete : bool
  }

and piece =
  | Own of Segment.t
  | Dependency_copy of Segment.t
  | Child of t

module Piece = struct
  type report = t

  type t = piece =
    | Own of Segment.t
    | Dependency_copy of Segment.t
    | Child of report
end

(* Explicit pending sibling lists keep traversal stack-safe without inventing
   an ownership visited-set. Aliases preserve their pieces; only diagnostic
   carrier identity is coalesced at the final pure projection. *)
let fold t ~init ~f =
  let rec loop pieces pending state =
    match pieces, pending with
    | [], [] -> state
    | [], next :: rest -> loop next rest state
    | Child child :: rest, pending ->
      loop child.pieces (rest :: pending) (f state (Child child))
    | ((Own _ | Dependency_copy _) as piece) :: rest, pending ->
      loop rest pending (f state piece)
  in
  loop t.pieces [] init
;;

let create ~diagnostics_complete pieces =
  { pieces
  ; diagnostics_complete =
      diagnostics_complete
      && List.for_all pieces ~f:(function
        | Own _ | Dependency_copy _ -> true
        | Child child -> child.diagnostics_complete)
  }
;;

let diagnostics_complete t = t.diagnostics_complete

let diagnostics t =
  let owned =
    fold t ~init:[] ~f:(fun owned -> function
      | Own segment -> segment :: owned
      | Dependency_copy _ | Child _ -> owned)
  in
  let add reversed segment ~copy =
    if
      (copy && List.mem owned segment ~equal:Disposal_report_data.Diagnostic.equal)
      || List.mem reversed segment ~equal:Disposal_report_data.Diagnostic.equal
    then reversed
    else segment :: reversed
  in
  fold t ~init:[] ~f:(fun reversed -> function
    | Child _ -> reversed
    | Own segment -> add reversed segment ~copy:false
    | Dependency_copy segment -> add reversed segment ~copy:true)
  |> List.rev
;;

let%expect_test "original diagnostic views preserve late children and producer aliases" =
  let module D = Disposal_report_data in
  let failure =
    Agent_session.Callback_cleanup.Failure.Raised
      (Failure "same actual payload", Stdlib.Printexc.get_callstack 0)
  in
  let payload = [ failure ] in
  let parent_segment = Segment.create payload in
  let first_child_segment = Segment.create payload in
  let later_child_segment = Segment.create payload in
  let child_before = create ~diagnostics_complete:false [ Own first_child_segment ] in
  let parent_before =
    create ~diagnostics_complete:true [ Own parent_segment; Child child_before ]
  in
  let child_after =
    create ~diagnostics_complete:true [ Own first_child_segment; Own later_child_segment ]
  in
  let parent_after =
    create
      ~diagnostics_complete:true
      [ Dependency_copy later_child_segment
      ; Own parent_segment
      ; Child child_after
      ; Child child_after
      ]
  in
  let copied_only =
    create ~diagnostics_complete:true [ Dependency_copy first_child_segment ]
  in
  let before = diagnostics parent_before in
  let after = diagnostics parent_after in
  let before_delivery = D.create ~disposition:Retained before in
  ignore
    (D.take_legacy_diagnostic_exn before_delivery : (D.Diagnostic.t * D.Failure.t) option);
  let after_delivery = D.create ~disposition:Disposed after in
  let newly_delivered =
    Option.exists (D.take_legacy_diagnostic_exn after_delivery) ~f:(fun (actual, _) ->
      D.Diagnostic.equal actual later_child_segment)
  in
  print_s
    [%sexp
      (List.length before : int)
    , (List.length after : int)
    , (diagnostics_complete parent_before : bool)
    , (diagnostics_complete parent_after : bool)
    , (List.equal
         D.Diagnostic.equal
         after
         [ parent_segment; first_child_segment; later_child_segment ]
       : bool)
    , (List.equal D.Diagnostic.equal before (diagnostics parent_before) : bool)
    , (List.equal D.Diagnostic.equal (diagnostics copied_only) [ first_child_segment ]
       : bool)
    , (newly_delivered : bool)];
  [%expect {| (2 3 false true true true true true) |}]
;;
