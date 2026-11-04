open! Core
module P = Agent_protocol
module D = Document_schema
module C = D.Extension_carrier
module X = Persistence_codec
module F = Recurrence_definition
module I = Recurrence_identity
module R = Recurrence_rules
module V = Recurrence_frontier_records
module S = V.State
module Codec = Recurrence_frontier_codec
module T = Recurrence_frontier_transition
module Summary = V.Summary
module Reason = V.Reason
module Issue = V.Issue

type t = S.t C.t

let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
;;

let value (t : t) = C.value t
let limits = Codec.limits

let initial definition =
  if Int64.equal (I.Definition.revision (F.key definition)) 0L
  then Ok (C.of_authored_value (S.initial definition))
  else invalid "initial frontier requires a new recurrence identity"
;;

let ready t = List.map (value t).ready ~f:V.Stored_nominal.value
let protected_children t = (value t).protected_children
let child_associations t = (value t).child_associations
let unlinked_children t = S.unlinked_children (value t)
let pending_count t = List.length (value t).ready + List.length (protected_children t)

let candidates t =
  Option.value_map (value t).batch ~default:[] ~f:(fun b ->
    List.map b.candidates ~f:V.Stored_nominal.value)
;;

let calendar_cutoff t = Option.map (value t).batch ~f:(fun b -> b.cutoff)
let lookahead t = Option.map (value t).lookahead ~f:V.Stored_nominal.value
let alarm t = Option.map (value t).alarm ~f:V.Stored_alarm.value
let issue t = (value t).issue
let revision t = (value t).revision

let summary t =
  S.summary (value t)
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (P.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let validate t ~definitions ~cursor =
  T.validate_associations (value t) ~definitions ~cursor
;;

(* Storage accounting only. This intentionally is not a domain projection:
   no maximum-spelling counter or synthetic range is admitted as evidence. Keeping
   all current history is conservative even when ordinary retention would prune. *)
let withdrawal_probe (s : S.t) =
  let missing_associations = not (List.is_empty (S.unlinked_children s)) in
  let pending =
    s.ready @ Option.value_map s.batch ~default:[] ~f:(fun b -> b.candidates)
  in
  let rows =
    List.map pending ~f:(fun nominal ->
      let key = Recurrence_nominal.key (V.Stored_nominal.value nominal) in
      Recurrence_frontier_codec.detail_json
        V.Detail.
          { definition = I.Occurrence.definition key
          ; first_slot = I.Occurrence.slot key
          ; last_slot = I.Occurrence.slot key
          ; kind = Withdrawn
          ; reason = Control_withdrawal
          ; nominal = Some nominal
          ; original_json = None
          })
  in
  let archives =
    Option.to_list
      (Option.bind s.alarm ~f:(fun alarm ->
         if V.Stored_alarm.has_extensions alarm
         then
           Some
             V.Control_detail.
               { kind = Retired_alarm
               ; json = V.Stored_alarm.json alarm
               ; original_json = None
               }
         else None))
    @ Option.to_list
        (Option.bind s.batch ~f:(fun batch ->
           if Codec.batch_has_extensions batch
           then
             Some
               V.Control_detail.
                 { kind = Retired_batch
                 ; json = Codec.batch_json batch
                 ; original_json = None
                 }
           else None))
  in
  match Codec.encode s with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, json) ->
         let json =
           match name with
           | "revision" -> X.int64_json Int64.max_value
           | "ready" -> `Array []
           | "alarm" ->
             (* Linking can retain the current control container. Do not spend
                its removal as structural credit for an independent association.
                Extended containers already move their complete raw value into
                control_history, with additional wrapper growth. *)
             if
               missing_associations
               && Option.exists s.alarm ~f:(fun alarm ->
                 not (V.Stored_alarm.has_extensions alarm))
             then json
             else `Null
           | "batch" ->
             if
               missing_associations
               && Option.exists s.batch ~f:(fun batch ->
                 not (Codec.batch_has_extensions batch))
             then json
             else `Null
           | "control_history" ->
             (match json with
              | `Array old -> `Array (List.map archives ~f:Codec.control_json @ old)
              | _ -> assert false)
           | "history" ->
             (match json with
              | `Array old -> `Array (rows @ old)
              | _ -> assert false)
           | "marks" ->
             (match json with
              | `Array marks ->
                `Array
                  (List.map marks ~f:(function
                     | `Object fields ->
                       `Object
                         (List.map fields ~f:(fun (key, value) ->
                            ( key
                            , if
                                List.mem
                                  [ "omitted"; "withdrawn" ]
                                  key
                                  ~equal:String.equal
                              then X.int64_json Int64.max_value
                              else value )))
                     | _ -> assert false))
              | _ -> assert false)
           | _ -> json
         in
         name, json)
       |> fun fields ->
       let associations =
         List.map s.child_associations ~f:Recurrence_child_association.to_json
         @ List.map (S.unlinked_children s) ~f:(fun _ ->
           Recurrence_child_reservation.Association_capacity.growth_projection ())
       in
       if List.is_empty associations
       then fields
       else
         List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "child_associations"))
         @ [ "child_associations", `Array associations ])
  | _ -> assert false
;;

let probe_codec =
  D.Domain_codec.create
    ~limits
    ~kind:"ochat.recurrence_frontier"
    ~version:1
    ~shape:Codec.shape
    ~supported_semantics:
      [ "recurrence.child-reservation.v1"; "recurrence.child-association.v1" ]
    ~decode:(fun j -> Ok j)
    ~encode:(fun j -> Ok j)
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let require_creation_semantics ?(capacity = false) t document =
  let required =
    (if List.is_empty (protected_children t)
     then []
     else [ "recurrence.child-reservation.v1" ])
    @
    if
      List.is_empty (child_associations t)
      && not (capacity && not (List.is_empty (protected_children t)))
    then []
    else [ "recurrence.child-association.v1" ]
  in
  if
    List.for_all required ~f:(fun marker ->
      List.mem (D.Document.required_semantics document) marker ~equal:String.equal)
  then Ok document
  else (
    match D.Document.json document with
    | `Object fields ->
      let semantics =
        D.Document.required_semantics document
        @ List.filter required ~f:(fun marker ->
          not
            (List.mem (D.Document.required_semantics document) marker ~equal:String.equal))
      in
      D.Document.inspect
        ~limits
        (`Object
            (List.filter fields ~f:(fun (name, _) ->
               not (String.equal name "required_semantics"))
             @ [ ( "required_semantics"
                 , `Array (List.map semantics ~f:(fun text -> `String text)) )
               ]))
    | _ -> assert false)
;;

let capacity_projection t =
  let open Result.Let_syntax in
  let%bind document =
    D.Domain_codec.encode probe_codec (C.with_value t (withdrawal_probe (value t)))
  in
  let%map document = require_creation_semantics ~capacity:true t document in
  D.Document.json document
;;

let reserved_growth t =
  let open Result.Let_syntax in
  let%bind current = D.Domain_codec.encode Codec.codec t |> protocol in
  let%bind current = require_creation_semantics t current |> protocol in
  let%bind future = capacity_projection t |> protocol in
  let%bind current = Capacity_growth.measure (D.Document.json current) ~limits in
  let%map future = Capacity_growth.measure future ~limits in
  Capacity_growth.positive_difference future ~previous:current
;;

let to_document t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode Codec.codec t in
  let%bind document = require_creation_semantics t document in
  let%bind growth = reserved_growth t |> X.document_result in
  let%bind remaining =
    Capacity_growth.remaining_limits growth limits |> X.document_result
  in
  let%map () = D.Json.validate ~limits:remaining (D.Document.json document) in
  document
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind t = D.Domain_codec.decode Codec.codec document in
  let%bind () =
    if
      List.is_empty (protected_children t)
      || List.mem
           (D.Document.required_semantics document)
           "recurrence.child-reservation.v1"
           ~equal:String.equal
    then Ok ()
    else
      Error (D.Error.Required_semantics_unknown "missing recurrence.child-reservation.v1")
  in
  let%bind () =
    if
      List.is_empty (child_associations t)
      || List.mem
           (D.Document.required_semantics document)
           "recurrence.child-association.v1"
           ~equal:String.equal
    then Ok ()
    else
      Error (D.Error.Required_semantics_unknown "missing recurrence.child-association.v1")
  in
  let%map _ = to_document t in
  t
;;

let admit previous state =
  let candidate = C.with_value previous state in
  let%map.Result _ = to_document candidate |> protocol in
  candidate
;;

let bump (state : S.t) =
  let%bind.Result revision = V.add state.revision 1L in
  if Int64.(revision > max_value - of_int (List.length (S.unlinked_children state)))
  then invalid "frontier lacks reserved control and child association revisions"
  else Ok { state with revision }
;;

let link_child t association =
  let open Result.Let_syntax in
  let occurrence = Recurrence_child_association.occurrence association in
  let%bind reservation =
    Result.of_option
      (List.find (protected_children t) ~f:(fun row ->
         I.Occurrence.equal occurrence (Recurrence_child_reservation.occurrence row)))
      ~error:(P.Error.invalid_request "linked child has no retained reservation")
  in
  let%bind () = Recurrence_child_association.validate association ~reservation in
  match
    List.find (child_associations t) ~f:(fun old ->
      I.Occurrence.equal occurrence (Recurrence_child_association.occurrence old))
  with
  | Some old when Recurrence_child_association.equal old association -> Ok t
  | Some _ -> invalid "linked child association cannot be replaced"
  | None ->
    let state = value t in
    let%bind state =
      bump { state with child_associations = state.child_associations @ [ association ] }
    in
    admit t state
;;

let reserve_child
      t
      ~definition
      ~control_revision
      ~principal_id
      ~child_session_id
      ~transaction_id
      ~occurrence
  =
  let open Result.Let_syntax in
  match
    List.find (protected_children t) ~f:(fun row ->
      I.Occurrence.equal occurrence (Recurrence_child_reservation.occurrence row))
  with
  | Some row ->
    let%map () = Recurrence_child_reservation.validate row ~definition in
    t, row
  | None ->
    let state = value t in
    let%bind nominal =
      Result.of_option
        (List.find state.ready ~f:(fun nominal ->
           I.Occurrence.equal
             occurrence
             (Recurrence_nominal.key (V.Stored_nominal.value nominal))))
        ~error:(P.Error.invalid_request "creation occurrence is not Ready")
    in
    let%bind row =
      Recurrence_child_reservation.create
        ~definition
        ~control_revision
        ~principal_id
        ~child_session_id
        ~transaction_id
        ~nominal_json:(V.Stored_nominal.json nominal)
    in
    let%bind state =
      bump
        { state with
          ready =
            List.filter state.ready ~f:(fun other ->
              not (V.Stored_nominal.equal nominal other))
        ; protected_children = state.protected_children @ [ row ]
        }
    in
    let%bind () =
      if
        Int64.(
          state.revision
          <= max_value - of_int Int.(2 + List.length (S.unlinked_children state)))
      then Ok ()
      else invalid "creation frontier lacks independent control/association credits"
    in
    let%map t = admit t state in
    t, row
;;

let withdraw t ~definition =
  let open Result.Let_syntax in
  let%bind state = T.withdraw (value t) ~definition ~preserve_lookahead:true in
  if D.Json.equal (Codec.encode state) (Codec.encode (value t))
  then Ok t
  else (
    let%bind state = bump state in
    admit t state)
;;

let adopt_definition t ~previous ~definition =
  let open Result.Let_syntax in
  let%bind state = T.withdraw (value t) ~definition:previous ~preserve_lookahead:false in
  let%bind old = T.find_mark state previous in
  let%bind () =
    if
      List.length state.marks >= 8
      || (not
            (Int64.equal
               (I.Definition.revision (F.key definition))
               Int64.(I.Definition.revision old.definition + 1L)))
      || (not
            (P.Session_ref.equal
               (I.Definition.controller old.definition)
               (I.Definition.controller (F.key definition))))
      || not
           (P.Id.Recurrence.equal
              (I.Definition.recurrence_id old.definition)
              (I.Definition.recurrence_id (F.key definition)))
    then invalid "new frontier definition is not the next retained revision"
    else Ok ()
  in
  let state =
    { state with marks = state.marks @ [ V.Mark.initial definition ]; issue = None }
  in
  let%bind state = bump state in
  let%bind () =
    T.validate_current state ~definition ~cursor:(F.initial_cursor definition)
  in
  admit t state
;;

type frontier = t

module Work = struct
  type t =
    { original : frontier
    ; definition : F.t
    ; original_cursor : Recurrence_cursor.t
    ; state : S.t
    ; expected_cursor : Recurrence_cursor.t option
    }

  type omission =
    { first_slot : int64
    ; last_slot : int64
    ; reason : Reason.t
    }

  let start original ~definition ~cursor =
    let%map.Result () = T.validate_current (C.value original) ~definition ~cursor in
    { original
    ; definition
    ; original_cursor = cursor
    ; state = C.value original
    ; expected_cursor = None
    }
  ;;

  let consume_interval t ~(range : R.Cursor.Interval.due_range) ~selected ~omitted =
    let open Result.Let_syntax in
    let%bind rule =
      match F.schedule t.definition with
      | Calendar _ -> invalid "interval selection has calendar source"
      | Interval { rule; _ } -> Ok rule
    in
    let%bind actual =
      Recurrence_cursor.consume_interval t.original_cursor ~rule ~count:range.count
    in
    let%bind () =
      if
        Int64.equal actual.consumed.first_slot range.first_slot
        && Int64.equal actual.consumed.last_slot range.last_slot
        && Option.equal R.Cursor.Interval.equal actual.consumed.next range.next
      then Ok ()
      else invalid "interval range differs from timing-owner consumption"
    in
    let%bind last =
      Recurrence_nominal.create_interval t.definition ~slot:range.last_slot
    in
    let%bind selected = List.map selected ~f:V.Stored_nominal.create |> Result.all in
    let%map state =
      T.consume_range
        t.state
        ~definition:t.definition
        ~first_slot:range.first_slot
        ~last_slot:range.last_slot
        ~last_at:(Recurrence_nominal.at last)
        ~selected
        ~omitted:(List.map omitted ~f:(fun r -> r.first_slot, r.last_slot, r.reason))
    in
    { t with state; expected_cursor = Some actual.next }
  ;;

  let consume_initial_delay t ~nominal =
    let open Result.Let_syntax in
    let%bind rule =
      match F.schedule t.definition with
      | Calendar _ -> invalid "fixed delay has calendar source"
      | Interval { rule; _ } -> Ok rule
    in
    let%bind expected_cursor =
      Recurrence_cursor.consume_initial_delay t.original_cursor ~rule
    in
    let%bind nominal = V.Stored_nominal.create nominal in
    let%map state =
      T.consume_range
        t.state
        ~definition:t.definition
        ~first_slot:0L
        ~last_slot:0L
        ~last_at:(Recurrence_nominal.at (V.Stored_nominal.value nominal))
        ~selected:[ nominal ]
        ~omitted:[]
    in
    { t with state; expected_cursor = Some expected_cursor }
  ;;

  let begin_calendar t ~cutoff =
    match F.schedule t.definition, t.state.batch with
    | Interval _, _ -> invalid "calendar batch has interval source"
    | Calendar _, Some batch ->
      if Time_ns.equal batch.cutoff cutoff
      then Ok t
      else invalid "calendar cutoff changed during collection"
    | Calendar _, None ->
      Ok
        { t with
          state =
            { t.state with
              batch = Some V.Batch.{ cutoff; candidates = []; original_json = None }
            ; issue = None
            }
        }
  ;;

  let collect_calendar t ~nominal =
    let open Result.Let_syntax in
    let%bind nominal =
      match t.state.lookahead with
      | Some retained
        when Recurrence_nominal.equal nominal (V.Stored_nominal.value retained) ->
        Ok retained
      | None | Some _ -> V.Stored_nominal.create nominal
    in
    let%map state = T.collect_calendar t.state ~definition:t.definition ~nominal in
    { t with
      state = { (T.retire_alarm state ~definition:t.definition) with lookahead = None }
    }
  ;;

  let finish_calendar t ~lookahead =
    let open Result.Let_syntax in
    let%bind lookahead =
      match lookahead with
      | None -> Ok None
      | Some nominal ->
        let retained =
          match t.state.lookahead with
          | Some retained
            when Recurrence_nominal.equal nominal (V.Stored_nominal.value retained) ->
            Ok retained
          | None | Some _ -> V.Stored_nominal.create nominal
        in
        Result.map retained ~f:Option.some
    in
    let%map state = T.finish_calendar t.state ~definition:t.definition ~lookahead in
    { t with state }
  ;;

  let set_lookahead t ~nominal =
    let%map.Result nominal = V.Stored_nominal.create nominal in
    { t with state = { t.state with lookahead = Some nominal } }
  ;;

  let retire_alarm t =
    Ok { t with state = T.retire_alarm t.state ~definition:t.definition }
  ;;

  let arm t ~slot ~due_at =
    let%map.Result state = T.arm t.state ~definition:t.definition ~slot ~due_at in
    { t with state }
  ;;

  let set_issue t issue = { t with state = { t.state with issue } }

  let finish t ~cursor =
    let open Result.Let_syntax in
    let%bind () =
      if Option.for_all t.expected_cursor ~f:(Recurrence_cursor.equal cursor)
      then Ok ()
      else invalid "candidate cursor differs from checked interval transition"
    in
    let%bind () = T.validate_current t.state ~definition:t.definition ~cursor in
    let changed =
      (not (D.Json.equal (Codec.encode t.state) (Codec.encode (C.value t.original))))
      || not (Recurrence_cursor.equal cursor t.original_cursor)
    in
    if not changed
    then Ok t.original
    else (
      let%bind state = bump t.state in
      if
        Int64.(
          state.revision
          > max_value - of_int Int.(2 + List.length (S.unlinked_children state)))
      then invalid "due frontier lacks reserved control revision steps"
      else admit t.original state)
  ;;
end

let validate_transition previous ~incoming ~definitions ~cursor =
  let open Result.Let_syntax in
  let%bind () = validate incoming ~definitions ~cursor in
  let p = C.value previous
  and n = C.value incoming in
  let%bind () =
    if D.Json.equal (Codec.encode p) (Codec.encode n)
    then Ok ()
    else if
      Int64.(p.revision = max_value)
      || not (Int64.equal n.revision (Int64.succ p.revision))
    then invalid "frontier must advance once"
    else Ok ()
  in
  let%bind () =
    List.fold_result p.marks ~init:() ~f:(fun () (old : V.Mark.t) ->
      match
        List.find n.marks ~f:(fun next ->
          I.Definition.equal old.definition next.definition)
      with
      | None -> invalid "immutable recurrence high-water disappeared"
      | Some next ->
        let monotonic compare a b = Option.compare compare a b <= 0 in
        let immutable_old =
          match List.last p.marks with
          | Some current when I.Definition.equal current.definition old.definition -> true
          | None | Some _ -> V.Mark.equal old next
        in
        if
          immutable_old
          && String.equal old.definition_sha256 next.definition_sha256
          && monotonic Int64.compare old.through next.through
          && monotonic Time_ns.compare old.last_at next.last_at
          && monotonic Int64.compare old.last_alarm_sequence next.last_alarm_sequence
          && Int64.(old.omitted <= next.omitted && old.withdrawn <= next.withdrawn)
        then Ok ()
        else invalid "immutable recurrence high-water regressed")
  in
  let rows state =
    state.S.ready
    @ Option.value_map state.batch ~default:[] ~f:(fun b -> b.candidates)
    @ Option.to_list state.lookahead
    @ List.filter_map state.history ~f:(fun detail -> detail.nominal)
  in
  let%bind () =
    List.fold_result (rows p) ~init:() ~f:(fun () old ->
      let key = Recurrence_nominal.key (V.Stored_nominal.value old) in
      match
        List.find (rows n) ~f:(fun row ->
          I.Occurrence.equal key (Recurrence_nominal.key (V.Stored_nominal.value row)))
      with
      | None ->
        (match
           List.find n.protected_children ~f:(fun row ->
             I.Occurrence.equal key (Recurrence_child_reservation.occurrence row))
         with
         | None -> Ok ()
         | Some row ->
           if
             D.Json.equal
               (Recurrence_child_reservation.nominal_json row)
               (V.Stored_nominal.json old)
           then Ok ()
           else invalid "protected nominal lost immutable original custody")
      | Some row when V.Stored_nominal.equal old row -> Ok ()
      | Some _ -> invalid "retained selected nominal changed immutable bytes")
  in
  let%bind () =
    List.fold_result n.protected_children ~init:() ~f:(fun () row ->
      match
        List.find p.protected_children ~f:(fun old ->
          I.Occurrence.equal
            (Recurrence_child_reservation.occurrence old)
            (Recurrence_child_reservation.occurrence row))
      with
      | Some _ -> Ok ()
      | None ->
        if
          List.exists p.ready ~f:(fun old ->
            I.Occurrence.equal
              (Recurrence_nominal.key (V.Stored_nominal.value old))
              (Recurrence_child_reservation.occurrence row)
            && D.Json.equal
                 (V.Stored_nominal.json old)
                 (Recurrence_child_reservation.nominal_json row))
        then Ok ()
        else invalid "new protected creation did not consume exact prior Ready")
  in
  let%bind () =
    List.fold_result p.protected_children ~init:() ~f:(fun () old ->
      match
        List.find n.protected_children ~f:(fun row ->
          I.Occurrence.equal
            (Recurrence_child_reservation.occurrence old)
            (Recurrence_child_reservation.occurrence row))
      with
      | Some row when Recurrence_child_reservation.equal old row -> Ok ()
      | None | Some _ -> invalid "protected creation evidence was erased or replaced")
  in
  let%bind () =
    List.fold_result p.child_associations ~init:() ~f:(fun () old ->
      match
        List.find n.child_associations ~f:(fun row ->
          I.Occurrence.equal
            (Recurrence_child_association.occurrence old)
            (Recurrence_child_association.occurrence row))
      with
      | Some row when Recurrence_child_association.equal old row -> Ok ()
      | None | Some _ -> invalid "immutable child association was erased or replaced")
  in
  let added_associations =
    List.filter n.child_associations ~f:(fun association ->
      not
        (List.exists p.child_associations ~f:(fun old ->
           I.Occurrence.equal
             (Recurrence_child_association.occurrence association)
             (Recurrence_child_association.occurrence old))))
  in
  let%bind () =
    if List.length added_associations > 1
    then invalid "one transition cannot link multiple children"
    else
      List.fold_result added_associations ~init:() ~f:(fun () association ->
        match
          List.find p.protected_children ~f:(fun reservation ->
            I.Occurrence.equal
              (Recurrence_child_association.occurrence association)
              (Recurrence_child_reservation.occurrence reservation))
        with
        | None -> invalid "child linkage requires a previously protected reservation"
        | Some reservation ->
          Recurrence_child_association.validate association ~reservation)
  in
  let%bind () =
    List.fold_result p.history ~init:() ~f:(fun () old ->
      match
        List.find n.history ~f:(fun row ->
          String.equal (V.Detail.identity old) (V.Detail.identity row))
      with
      | None -> Ok ()
      | Some row when D.Json.equal (Codec.detail_json old) (Codec.detail_json row) ->
        Ok ()
      | Some _ -> invalid "retained disposition detail changed immutable bytes")
  in
  (* The native candidate can still carry the envelope from before its first
     protected reservation or child link. Persistence, by contrast, has decoded
     the marker-bearing wire returned by [to_document]. Materialize the same
     validated candidate value through each preservation basis before adoption:
     only this owner's positive facts may add its required semantics. All prior
     envelope fields and unknown payload fields still pass strict adoption. *)
  let materialize candidate =
    let%bind document = to_document candidate |> protocol in
    D.Domain_codec.decode Codec.codec document |> protocol
  in
  let%bind previous = materialize (C.with_value previous n) in
  let%bind incoming = materialize incoming in
  let%bind adopted = D.Domain_codec.adopt Codec.codec ~previous ~incoming |> protocol in
  let%bind adopted = to_document adopted |> protocol in
  let%bind incoming = to_document incoming |> protocol in
  if D.Json.equal (D.Document.json adopted) (D.Document.json incoming)
  then Ok ()
  else invalid "frontier update discarded compatible unknown fields"
;;
