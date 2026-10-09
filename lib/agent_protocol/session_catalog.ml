open! Core

type t =
  { session : Session.t
  ; active_owner_principal_id : Id.Principal.t option
  ; archived : bool
  ; lifecycle_revision : Session_lifecycle.Revision.t
  ; admission : Session_lifecycle.Result.Admission.t
  ; effective_organization : Session_organization.Values.t
  }
[@@deriving sexp]

let to_json t =
  match Session.to_json t.session with
  | `Object fields ->
    let owner =
      match t.active_owner_principal_id with
      | None -> []
      | Some id -> [ "active_owner_principal_id", Id.Principal.to_json id ]
    in
    `Object
      (fields
       @ [ ("archived", if t.archived then `True else `False)
         ; ( "lifecycle_revision"
           , `Number
               (Int64.to_string
                  (Session_lifecycle.Revision.to_int64 t.lifecycle_revision)) )
         ; "admission", Session_lifecycle.Result.Admission.to_json t.admission
         ; ( "effective_organization"
           , Session_organization.Values.to_json t.effective_organization )
         ]
       @ owner)
  | _ -> assert false
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind session = Session.of_json json in
  let%bind fields = Json_codec.fields json in
  let%bind archived = Json_codec.optional_as fields "archived" Json_codec.bool in
  let%bind active_owner_principal_id =
    Json_codec.optional_as fields "active_owner_principal_id" Id.Principal.of_json
  in
  let%bind lifecycle_revision =
    Json_codec.optional_as fields "lifecycle_revision" (fun json ->
      Result.bind
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value json)
        ~f:Session_lifecycle.Revision.of_int64)
  in
  let%bind admission =
    Json_codec.optional_as fields "admission" Session_lifecycle.Result.Admission.of_json
  in
  let%bind organization =
    Json_codec.optional_as
      fields
      "effective_organization"
      Session_organization.Values.of_json
  in
  let archived = Option.value archived ~default:false in
  let%bind lifecycle_revision, admission =
    match lifecycle_revision, admission with
    | Some revision, Some admission -> Ok (revision, admission)
    | None, None ->
      if archived
      then (
        let%map revision = Session_lifecycle.Revision.of_int64 1L in
        revision, Session_lifecycle.Result.Admission.Explicit_resume_required)
      else
        Ok (Session_lifecycle.Revision.zero, Session_lifecycle.Result.Admission.Automatic)
    | Some _, None | None, Some _ ->
      Error (Protocol_error.invalid_request "incomplete catalog lifecycle observation")
  in
  if
    (Session_lifecycle.Revision.equal lifecycle_revision Session_lifecycle.Revision.zero
     && not (Session_lifecycle.Result.Admission.equal admission Automatic))
    || (archived
        && (Session_lifecycle.Revision.equal
              lifecycle_revision
              Session_lifecycle.Revision.zero
            || Session_lifecycle.Result.Admission.equal admission Automatic))
  then Error (Protocol_error.invalid_request "archived catalog lifecycle is inconsistent")
  else
    Ok
      { session
      ; active_owner_principal_id
      ; archived
      ; lifecycle_revision
      ; admission
      ; effective_organization =
          Option.value organization ~default:Session_organization.Values.empty
      }
;;
