open Core
module P = Agent_protocol
module D = Document_schema
module F = Document_fields

module Key = struct
  type t =
    { parent_session_id : P.Id.Session.t
    ; parent_generation : int
    ; principal_id : P.Id.Principal.t
    ; idempotency_key : P.Idempotency_key.t
    }
  [@@deriving equal, sexp]
end

module Admission = struct
  type authored_tool =
    { name : string
    ; source_sha256 : string
    }
  [@@deriving equal, sexp]

  type lifetime =
    | Owned
    | Invocation_owned of { invocation_id : P.Id.Invocation.t }
    | Independent of { authorization_sha256 : string }
  [@@deriving equal, sexp]

  type t =
    { child_session_id : P.Id.Session.t
    ; revision_id : P.Id.Prompt_revision.t
    ; transaction_id : P.Id.Transaction.t
    ; manifest_sha256 : string
    ; parent_revision_id : P.Id.Prompt_revision.t
    ; parent_stop_epoch : int64 option [@sexp.option]
    ; authority_sha256 : string
    ; authored_tool : authored_tool option [@sexp.option]
    ; capability_pins : (string * string) list
    ; lifetime : lifetime
    ; created_at : P.Timestamp.t
    ; inference_target : (Inference.Request.Target.t[@sexp.opaque]) option [@sexp.option]
    }
  [@@deriving equal, sexp]
end

type stage =
  | Reserved
  | Artifact_installed
  | Child_installed
  | Linked
[@@deriving equal, sexp]

module Reference = struct
  type t =
    { key : Key.t
    ; child_session_id : P.Id.Session.t
    ; revision_id : P.Id.Prompt_revision.t
    ; request_sha256 : string
    ; admission_sha256 : string
    }
  [@@deriving equal, sexp]
end

type revocation =
  | Parent_stopped
  | Parent_deleted
  | Authority_changed
  | Admission_failed
[@@deriving equal, sexp]

type artifact_collection = Prepared [@@deriving equal, sexp]

type t =
  { key : Key.t
  ; request_sha256 : string
  ; admission : Admission.t
  ; stage : stage
  ; revocation : revocation option
  ; artifact_collection : artifact_collection option [@sexp.option]
  ; preservation : (unit D.Extension_carrier.t[@sexp.opaque]) option
        [@sexp.option] [@equal.ignore]
  }
[@@deriving equal, sexp]

let corrupt message = Error (Store_error.Corrupt message)

let sha256 value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let validate_key (key : Key.t) =
  let open Result.Let_syntax in
  let%bind _ =
    protocol (P.Id.Session.of_string (P.Id.Session.to_string key.parent_session_id))
  in
  let%bind _ =
    protocol (P.Id.Principal.of_string (P.Id.Principal.to_string key.principal_id))
  in
  let%bind _ =
    protocol
      (P.Idempotency_key.of_string (P.Idempotency_key.to_string key.idempotency_key))
  in
  match key.parent_generation >= 0 with
  | true -> Ok ()
  | false -> corrupt "negative delegation parent generation"
;;

let validate (record : t) ~limits =
  let open Result.Let_syntax in
  let a = record.admission in
  let%bind () =
    Option.value_map
      a.inference_target
      ~default:(corrupt "delegation admission requires a captured inference target")
      ~f:(fun target ->
        Inference.Request.Target.validate target ~limits
        |> Result.map_error ~f:(fun error ->
          Store_error.Corrupt
            (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))))
  in
  let%bind () = validate_key record.key in
  let%bind _ =
    protocol (P.Id.Session.of_string (P.Id.Session.to_string a.child_session_id))
  in
  let%bind _ =
    protocol
      (P.Id.Prompt_revision.of_string (P.Id.Prompt_revision.to_string a.revision_id))
  in
  let%bind _ =
    protocol
      (P.Id.Prompt_revision.of_string
         (P.Id.Prompt_revision.to_string a.parent_revision_id))
  in
  let%bind _ =
    protocol (P.Id.Transaction.of_string (P.Id.Transaction.to_string a.transaction_id))
  in
  let%bind _ = protocol (P.Timestamp.of_string (P.Timestamp.to_string a.created_at)) in
  let lifetime_valid =
    match a.lifetime with
    | Owned -> true
    | Invocation_owned { invocation_id } ->
      Option.is_some a.authored_tool
      && Result.is_ok
           (P.Id.Invocation.of_string (P.Id.Invocation.to_string invocation_id))
    | Independent { authorization_sha256 } -> sha256 authorization_sha256
  in
  let authored_valid =
    Option.for_all a.authored_tool ~f:(fun authored ->
      (not (String.is_empty authored.name))
      && String.length authored.name <= 1024
      && (not
            (String.exists authored.name ~f:(fun c ->
               Char.to_int c < 32 || Char.equal c '\127')))
      && sha256 authored.source_sha256)
  in
  match
    (not (P.Id.Session.equal record.key.parent_session_id a.child_session_id))
    && (not (P.Id.Prompt_revision.equal a.parent_revision_id a.revision_id))
    && sha256 record.request_sha256
    && sha256 a.manifest_sha256
    && sha256 a.authority_sha256
    && Option.for_all a.parent_stop_epoch ~f:(fun epoch -> Int64.(epoch >= 0L))
    && lifetime_valid
    && authored_valid
    && (match record.artifact_collection, record.stage, record.revocation with
        | None, _, _ -> true
        | Some Prepared, (Reserved | Artifact_installed), Some _ -> true
        | Some Prepared, _, _ -> false)
    && List.is_sorted_strictly a.capability_pins ~compare:(fun (left, _) (right, _) ->
      String.compare left right)
    && List.for_all a.capability_pins ~f:(fun (name, pin) ->
      (not (String.is_empty name))
      && String.length name <= 1024
      && (not
            (String.exists name ~f:(fun c -> Char.to_int c < 32 || Char.equal c '\127')))
      && sha256 pin)
  with
  | true -> Ok ()
  | false -> corrupt "invalid delegated creation identity or capability pins"
;;
