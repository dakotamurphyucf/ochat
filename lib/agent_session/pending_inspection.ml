open! Core
module P = Agent_protocol
module Q = P.Pending_query

type t = Session_state.t
type projection = P.History.entry -> (P.Public_history.t, P.Error.t) result

let item document ~project =
  let open Result.Let_syntax in
  let input = Pending_input_document.value document in
  let%bind history = project (P.Pending_input.entry input) in
  Q.Item.create
    ~history
    ~generation:(P.Pending_input.generation input)
    ~binding:(P.Pending_input.binding input)
;;

let pending state history_id =
  List.find state.Session_state.conversation.deferred_user_entries ~f:(fun document ->
    P.History.Id.equal
      history_id
      (P.Pending_input.history_id (Pending_input_document.value document)))
;;

let disposition state history_id =
  List.find state.Session_state.conversation.pending_dispositions ~f:(fun document ->
    P.History.Id.equal
      history_id
      (Pending_disposition.history_id (Pending_disposition_document.value document)))
;;

let reason = function
  | Pending_disposition.Retirement_reason.Source_reset -> Q.Retirement_reason.Source_reset
  | Source_replaced -> Source_replaced
  | Canonical_history_retired -> Canonical_history_retired
;;

let lookup state ~history_id ~project =
  let open Result.Let_syntax in
  match pending state history_id with
  | Some document ->
    let%map item = item document ~project in
    Q.Outcome.pending item
  | None ->
    (match disposition state history_id with
     | None -> Ok (Q.Outcome.unavailable history_id)
     | Some document ->
       (match
          Pending_disposition.outcome (Pending_disposition_document.value document)
        with
        | Cancelled -> Ok (Q.Outcome.cancelled history_id)
        | Retired retired -> Ok (Q.Outcome.retired history_id ~reason:(reason retired))
        | Adopted admitted_content_revision ->
          let%bind current =
            match
              List.find state.conversation.canonical_history ~f:(fun entry ->
                P.History.Id.equal entry.id history_id)
            with
            | None -> Ok None
            | Some entry -> Result.map (project entry) ~f:Option.some
          in
          Q.Outcome.adopted ~history_id ~admitted_content_revision ~current))
;;

let authorize_control state ~history_id ~principal =
  let owner =
    match pending state history_id with
    | Some document -> Pending_input_document.owner document
    | None ->
      (match disposition state history_id with
       | Some document -> Pending_disposition_document.owner document
       | None -> Pending_input_document.Owner.Unknown)
  in
  Pending_input_document.Owner.authorize owner ~principal
;;
