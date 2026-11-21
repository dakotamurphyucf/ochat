open! Core
module P = Agent_protocol

type t =
  { canonical : P.History.entry list
  ; pending_revision : P.Pending_input.Revision.t
  ; entries : P.History.entry list
  ; wakes : P.Delivery.t list
  ; wakes_authorized : bool
  ; required : History_entry.t list
  }

let selected state ~boundary ~runtime_admission_open =
  let open Result.Let_syntax in
  let%bind eligibility =
    Pending_eligibility.create state ~boundary ~runtime_admission_open
  in
  let%map pending =
    Pending_eligibility.eligible_prefix
      eligibility
      state.conversation.deferred_user_entries
  in
  List.map pending ~f:Pending_input_document.entry
;;

let selected_wakes (state : Session_state.t) =
  let open Result.Let_syntax in
  let by_id = Hashtbl.create (module P.History.Id) in
  List.iter state.conversation.canonical_history ~f:(fun entry ->
    Hashtbl.set by_id ~key:entry.id ~data:entry);
  let wakes =
    List.filter state.deliveries ~f:(fun delivery ->
      Int.equal delivery.context.generation state.identity.generation
      && P.Id.Session.equal delivery.context.session_id state.identity.session_id
      &&
      match delivery.status, delivery.wake_disposition with
      | Committed { history_id; _ }, Some Pending_wake -> Hashtbl.mem by_id history_id
      | _ -> false)
  in
  let%bind wakes = Notification_wake_selection.select state ~context:Ordinary wakes in
  let%map () =
    List.fold_result wakes ~init:() ~f:(fun () wake ->
      match wake.status with
      | Committed { history_id; _ } ->
        Notification_history.validate ~delivery:wake (Hashtbl.find_exn by_id history_id)
      | Pending | Failed _ ->
        Error (P.Error.invalid_request "rollover wake has no committed history"))
  in
  wakes
;;

let dispatch_selection state ~operation =
  let open Result.Let_syntax in
  let%bind entries =
    selected state ~boundary:(Successor operation) ~runtime_admission_open:true
  in
  let%map wakes = selected_wakes state in
  entries, wakes
;;

let with_current_wakes t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind wakes = selected_wakes state in
  let canonical = state.conversation.canonical_history in
  let wake_members = Hash_set.create (module P.History.Id) in
  List.iter wakes ~f:(fun wake ->
    match wake.status with
    | Committed { history_id; _ } -> Hash_set.add wake_members history_id
    | Pending | Failed _ -> ());
  let required =
    List.filter canonical ~f:(fun entry ->
      Hash_set.mem wake_members entry.id
      || (P.History.equal_role entry.role User
          && P.History.equal_kind entry.kind Message
          && P.History.equal_provenance entry.provenance Canonical
          && not entry.redacted))
  in
  let%map required = History_codec.all_of_protocol (required @ t.entries) in
  { t with canonical; wakes; required }
;;

let capture (state : Session_state.t) ~runtime_admission_open =
  let open Result.Let_syntax in
  let%bind entries = selected state ~boundary:Idle_start ~runtime_admission_open in
  with_current_wakes
    { canonical = []
    ; pending_revision = state.conversation.pending_revision
    ; entries
    ; wakes = []
    ; wakes_authorized = false
    ; required = []
    }
    state
;;

let required t = t.required
let entries t = t.entries
let wakes t = t.wakes
let wakes_authorized t = t.wakes_authorized || List.is_empty t.wakes

let check t state ~operation ~runtime_admission_open =
  let open Result.Let_syntax in
  let%bind entries =
    selected state ~boundary:(Successor operation) ~runtime_admission_open
  in
  let%bind wakes = selected_wakes state in
  let wake_equal (left : P.Delivery.t) (right : P.Delivery.t) =
    P.Delivery.equal_context left.context right.context
    && Int.equal left.attempt right.attempt
    && P.Delivery.equal_status left.status right.status
    && Option.equal
         P.Delivery.equal_wake_disposition
         left.wake_disposition
         right.wake_disposition
    && Option.equal
         (List.equal (Tuple2.equal ~eq1:String.equal ~eq2:String.equal))
         left.disclosure_pins
         right.disclosure_pins
    && Option.equal
         P.Completion_projection.equal
         left.completion_projection
         right.completion_projection
  in
  if
    runtime_admission_open
    && P.Pending_input.Revision.equal
         t.pending_revision
         state.conversation.pending_revision
    && List.equal P.History.equal_entry t.canonical state.conversation.canonical_history
    && List.equal P.History.equal_entry t.entries entries
    && List.equal wake_equal t.wakes wakes
  then Ok ()
  else
    Error
      (P.Error.create
         Conflict
         ~message:"rollover dispatch input changed"
         ~retryable:true
         ())
;;

let extend_canonical t (state : Session_state.t) returned ~owns_entry =
  let actual = state.conversation.canonical_history in
  let prefix, suffix = List.split_n actual (List.length t.canonical) in
  let expected = History_codec.all_to_protocol ~previous:suffix returned in
  if
    P.Pending_input.Revision.equal t.pending_revision state.conversation.pending_revision
    && List.equal P.History.equal_entry t.canonical prefix
    && List.for_all suffix ~f:owns_entry
    && List.equal P.History.equal_entry expected suffix
  then Ok { t with canonical = actual }
  else Error (P.Error.invalid_request "successor input changed outside its publication")
;;

let record_authoring t state returned =
  extend_canonical t state returned ~owns_entry:(fun entry ->
    match entry.provenance with
    | Runtime_authoring _ -> true
    | Canonical | Moderator_inserted | Moderator_replaced _ | Runtime_notification _ ->
      false)
;;

let record_notifications t state returned ~(proposal : Notification_delivery.idle) =
  let open Result.Let_syntax in
  let%bind t =
    extend_canonical t state returned ~owns_entry:(fun entry ->
      match entry.provenance with
      | Runtime_notification _ -> true
      | Canonical | Moderator_inserted | Moderator_replaced _ | Runtime_authoring _ ->
        false)
  in
  let%bind t = with_current_wakes t state in
  let approved = Hash_set.create (module P.Id.Delivery) in
  List.iter proposal.wakes ~f:(fun wake -> Hash_set.add approved wake.context.id);
  List.iter proposal.pending.actions ~f:(function
    | Notification_delivery.Publish delivery -> Hash_set.add approved delivery.context.id
    | Fail _ -> ());
  if List.for_all t.wakes ~f:(fun wake -> Hash_set.mem approved wake.context.id)
  then Ok { t with wakes_authorized = true }
  else
    Error (P.Error.invalid_request "successor wakes exceed current publisher selection")
;;
