open! Core
module J = Json_codec
module W = Recurrence_wire

module Features = struct
  let inspect = "sessions.recurrences"
end

let max_envelope_bytes = 4 * 1024 * 1024
let list_method = "session.recurrences"
let get_method = "session.recurrence"
let status_method = "session.recurrence.status"
let history_method = "session.recurrence.history"

module Request = struct
  type selector =
    | List
    | Get of Id.Recurrence.t
    | Status of Id.Recurrence.t
    | History of Id.Recurrence.t

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t option
    ; selector : selector
    }

  let methods = [ list_method; get_method; status_method; history_method ]

  let method_name t =
    match t.selector with
    | List -> list_method
    | Get _ -> get_method
    | Status _ -> status_method
    | History _ -> history_method
  ;;

  let params t =
    let fields =
      [ "session", Session_ref.to_json t.session
      ; "attachment_id", W.option_json Id.Attachment.to_json t.attachment_id
      ]
    in
    match t.selector with
    | List -> `Object fields
    | Get id | Status id | History id ->
      `Object (("recurrence_id", Id.Recurrence.to_json id) :: fields)
  ;;

  let of_method_and_params ~method_ ~params =
    let open Core.Result.Let_syntax in
    let%bind () = Extension_codec.validate_json ~max_bytes:4096 ~max_depth:8 params in
    let fields = [ "session"; "attachment_id" ] in
    let%bind fields =
      W.fields
        params
        (if String.equal method_ list_method then fields else "recurrence_id" :: fields)
    in
    let%bind session = J.required_as fields "session" W.session in
    let%bind attachment_id =
      J.required_as fields "attachment_id" (W.nullable Id.Attachment.of_json)
    in
    let%map selector =
      if String.equal method_ list_method
      then Ok List
      else (
        let%bind id = J.required_as fields "recurrence_id" Id.Recurrence.of_json in
        if String.equal method_ get_method
        then Ok (Get id)
        else if String.equal method_ status_method
        then Ok (Status id)
        else if String.equal method_ history_method
        then Ok (History id)
        else W.invalid "unknown recurrence query method")
    in
    { session; attachment_id; selector }
  ;;
end

module Availability = Recurrence_query_definition.Availability
module Basis = Recurrence_query_definition.Basis
module Definition = Recurrence_query_definition.Definition
module Planning = Recurrence_query_snapshot.Planning
module Totals = Recurrence_query_snapshot.Totals
module Definition_id = Recurrence_query_evidence.Definition_id
module Occurrence_id = Recurrence_query_evidence.Occurrence_id
module Nominal = Recurrence_query_evidence.Nominal
module Outcome = Recurrence_query_evidence.Outcome
module Run_observation = Recurrence_query_evidence.Run_observation
module Target = Recurrence_query_evidence.Target
module Attempt = Recurrence_query_evidence.Attempt
module Occurrence = Recurrence_query_evidence.Occurrence
module Protected_creation = Recurrence_query_evidence.Protected_creation
module Status = Recurrence_query_snapshot.Status
module History = Recurrence_query_snapshot.History

let definition_matches (entry : Definition.t) (definition : Definition_id.t) =
  Session_ref.equal entry.controller definition.controller
  && Id.Recurrence.equal entry.id definition.recurrence_id
  && Int64.(definition.revision <= entry.definition_revision)
;;

let nominal_matches entry (nominal : Nominal.t) =
  definition_matches entry nominal.occurrence.definition
;;

let protected_matches entry (row : Protected_creation.t) =
  nominal_matches entry row.nominal
;;

let occurrence_matches entry (row : Occurrence.t) =
  nominal_matches entry row.nominal
  && List.for_all row.attempts ~f:(fun attempt ->
    match attempt.target with
    | Denied | Unavailable -> true
    | Visible { session; _ } ->
      Id.Server.equal
        (Session_ref.server_id session)
        (Session_ref.server_id entry.controller))
;;

let can_capture (basis : Basis.t) = basis.automatic && Option.is_some basis.source

let entry_matches (basis : Basis.t) (entry : Definition.t) =
  let birth =
    Session_lifecycle.Revision.compare
      entry.birth_lifecycle_revision
      basis.lifecycle_revision
  in
  Session_ref.equal basis.session entry.controller
  && entry.source.generation <= basis.generation
  && birth <= 0
  && (match entry.control with
      | Retired _ -> true
      | Enabled | Paused _ -> birth = 0)
  && List.for_all entry.actions ~f:(function
    | Edit -> can_capture basis
    | Enable ->
      can_capture basis && Option.exists basis.source ~f:(Run_source.equal entry.source)
    | Create | Pause | Retire -> true)
;;

let current_nominal_matches (entry : Definition.t) (nominal : Nominal.t) =
  nominal_matches entry nominal
  && Int64.equal nominal.occurrence.definition.revision entry.definition_revision
;;

module Result = struct
  type 'a view =
    { basis : Basis.t
    ; can_create : bool
    ; value : 'a
    }

  type 'a outcome =
    | Available of 'a view
    | Unavailable of Session_ref.t

  type t =
    | List of Definition.t list outcome
    | Get of Definition.t outcome
    | Status of Status.t outcome
    | History of History.t outcome

  let session = function
    | Available { basis; _ } -> basis.session
    | Unavailable session -> session
  ;;

  let method_name = function
    | List _ -> list_method
    | Get _ -> get_method
    | Status _ -> status_method
    | History _ -> history_method
  ;;

  let outcome_json encode = function
    | Available { basis; can_create; value } ->
      W.tagged
        "available"
        [ "basis", Basis.to_json basis
        ; "can_create", W.bool_json can_create
        ; "value", encode value
        ]
    | Unavailable session ->
      W.tagged "unavailable" [ "session", Session_ref.to_json session ]
  ;;

  let payload = function
    | List outcome -> outcome_json (W.array Definition.to_json) outcome
    | Get outcome -> outcome_json Definition.to_json outcome
    | Status outcome -> outcome_json Status.to_json outcome
    | History outcome -> outcome_json History.to_json outcome
  ;;

  let outcome_of_json decode ~validate json =
    let open Core.Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "available" ->
      let%bind () =
        Extension_codec.closed fields [ "kind"; "basis"; "can_create"; "value" ]
      in
      let%bind basis = J.required_as fields "basis" Basis.of_json in
      let%bind can_create = J.required_as fields "can_create" J.bool in
      let%bind value = J.required_as fields "value" decode in
      if ((not can_create) || can_capture basis) && validate basis value
      then Ok (Available { basis; can_create; value })
      else W.invalid "recurrence query values disagree with their controller"
    | "unavailable" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "session" ] in
      let%map session = J.required_as fields "session" W.session in
      Unavailable session
    | _ -> W.invalid "unsupported recurrence query result"
  ;;

  let of_method_and_result ~method_ ~result =
    let open Core.Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_json ~max_bytes:max_envelope_bytes ~max_depth:32 result
    in
    if String.equal method_ list_method
    then (
      let%map outcome =
        outcome_of_json
          (W.list 16 Definition.of_json)
          result
          ~validate:(fun basis entries ->
            List.for_all entries ~f:(entry_matches basis)
            && Option.is_none
                 (List.find_a_dup
                    (List.map entries ~f:(fun entry -> entry.id))
                    ~compare:Id.Recurrence.compare))
      in
      List outcome)
    else if String.equal method_ get_method
    then (
      let%map outcome =
        outcome_of_json Definition.of_json result ~validate:entry_matches
      in
      Get outcome)
    else if String.equal method_ status_method
    then (
      let%map outcome =
        outcome_of_json Status.of_json result ~validate:(fun basis status ->
          entry_matches basis status.entry
          && Int.equal
               status.pending_count
               (status.unresolved_count + List.length status.ready)
          && List.for_all status.ready ~f:(current_nominal_matches status.entry)
          && Option.for_all status.lookahead ~f:(current_nominal_matches status.entry)
          && List.for_all status.protected_creations ~f:(protected_matches status.entry)
          && List.for_all status.occurrences ~f:(fun occurrence ->
            occurrence_matches status.entry occurrence
            && Int.equal (List.length occurrence.attempts) 1))
      in
      Status outcome)
    else if String.equal method_ history_method
    then (
      let%map outcome =
        outcome_of_json History.of_json result ~validate:(fun basis history ->
          entry_matches basis history.entry
          && List.for_all history.retained_ranges ~f:(fun range ->
            definition_matches history.entry range.definition)
          && List.for_all history.protected_creations ~f:(protected_matches history.entry)
          && List.for_all history.occurrences ~f:(fun occurrence ->
            occurrence_matches history.entry occurrence
            && List.for_alli occurrence.attempts ~f:(fun number attempt ->
              Int.equal number attempt.number)
            && List.for_all (List.drop_last_exn occurrence.attempts) ~f:(fun attempt ->
              attempt.retry_resolution_recorded)))
      in
      History outcome)
    else W.invalid "unknown recurrence query result method"
  ;;
end
