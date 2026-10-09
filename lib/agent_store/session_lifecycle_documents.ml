open! Core
module R = Session_archive_record
module C = Session_archive_document
module P = Agent_protocol

let encoded_document document =
  C.to_document document
  |> Document_fields.store
  |> Result.map ~f:Document_schema.Document.to_string
;;

type t =
  { document : C.t option
  ; value : R.t
  ; stamp : string option
  }

let create ~session_id document =
  let open Result.Let_syntax in
  match document with
  | None -> Ok { document; value = R.initial ~session_id; stamp = None }
  | Some admitted ->
    if not (P.Id.Session.equal session_id (R.session_id (C.value admitted)))
    then Error (Store_error.Corrupt "lifecycle document has a different session owner")
    else (
      let%map bytes = encoded_document admitted in
      { document; value = C.value admitted; stamp = Some bytes })
;;

let value t = t.value
let document t = t.document

let equal left right =
  Option.equal String.equal left.stamp right.stamp && R.equal left.value right.value
;;

module Target = struct
  type t =
    | Upsert of Session_index_entry.t
    | Remove of P.Id.Session.t
end

module Prepared = struct
  type t =
    { document : C.t
    ; bytes : string
    ; outcome : R.Outcome.t
    ; target : Target.t
    }

  let document t = t.document
  let bytes t = t.bytes
  let outcome t = t.outcome
  let target t = t.target

  let has_current_outcome t =
    let current = C.value t.document in
    R.Revision.equal t.outcome.lifecycle_revision (R.revision current)
    && R.Status.equal t.outcome.status (R.status current)
    && R.Admission.equal t.outcome.admission (R.admission current)
  ;;
end

let prepare t ~current_entry ~transition ~now =
  let open Result.Let_syntax in
  let%bind () = Session_index_entry.validate current_entry in
  let%bind () =
    if R.equal t.value (R.Prepared.previous transition)
    then Ok ()
    else Error (Store_error.Corrupt "lifecycle transition has a different observed basis")
  in
  let outcome = R.Prepared.outcome transition in
  let session = current_entry.Session_index_entry.session in
  let%bind () =
    if
      P.Id.Session.equal session.id (R.session_id t.value)
      && Int.equal outcome.anchor.generation session.generation
      && Int64.equal outcome.anchor.session_revision session.revision
      && Int64.equal outcome.anchor.latest_event_sequence session.latest_event_sequence
    then Ok ()
    else Error (Store_error.Corrupt "lifecycle outcome canonical anchor is stale")
  in
  let%bind original =
    match t.document with
    | Some document -> Ok document
    | None -> C.authored t.value |> Document_fields.store
  in
  let%bind document = C.prepare original transition ~now |> Document_fields.store in
  let%bind bytes = encoded_document document in
  let%map target =
    match R.status (C.value document) with
    | Removed -> Ok (Target.Remove session.id)
    | Active | Archived ->
      Session_index_entry.with_lifecycle current_entry (C.value document)
      |> Result.map ~f:(fun entry -> Target.Upsert entry)
  in
  { Prepared.document; bytes; outcome; target }
;;

let acknowledge t ~key ~request_digest =
  let open Result.Let_syntax in
  let%bind document =
    t.document
    |> Result.of_option ~error:(Store_error.Corrupt "lifecycle receipt carrier is absent")
  in
  let%bind document =
    C.acknowledge document ~key ~request_digest |> Document_fields.store
  in
  create ~session_id:(R.session_id t.value) (Some document)
;;
