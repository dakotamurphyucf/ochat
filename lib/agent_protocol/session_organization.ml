open! Core
module Error = Protocol_error

let collections values =
  if List.length values > 128 || List.contains_dup values ~compare:Id.Collection.compare
  then
    Error
      (Error.invalid_request
         "organization collections must be distinct and bounded to 128")
  else Ok (List.sort values ~compare:Id.Collection.compare)
;;

module Values = struct
  type t =
    { project_id : Id.Project.t option
    ; collection_ids : Id.Collection.t list
    }
  [@@deriving equal, sexp]

  let empty = { project_id = None; collection_ids = [] }

  let create ~project_id ~collection_ids =
    Result.map (collections collection_ids) ~f:(fun collection_ids ->
      { project_id; collection_ids })
  ;;

  let to_json t =
    `Object
      (("collection_ids", `Array (List.map t.collection_ids ~f:Id.Collection.to_json))
       :: Option.to_list
            (Option.map t.project_id ~f:(fun id -> "project_id", Id.Project.to_json id)))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind project_id = Json_codec.optional_as fields "project_id" Id.Project.of_json in
    let%bind collection_ids =
      Json_codec.required_as
        fields
        "collection_ids"
        (Json_codec.list Id.Collection.of_json)
    in
    create ~project_id ~collection_ids
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match create ~project_id:value.project_id ~collection_ids:value.collection_ids with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Patch = struct
  type project_change =
    | Keep
    | Set of Id.Project.t
    | Clear
  [@@deriving equal, sexp]

  type t =
    { project : project_change
    ; add_collections : Id.Collection.t list
    ; remove_collections : Id.Collection.t list
    }
  [@@deriving sexp]

  let create ~project ~add_collections ~remove_collections =
    let open Result.Let_syntax in
    let%bind add_collections = collections add_collections in
    let%bind remove_collections = collections remove_collections in
    if
      List.exists add_collections ~f:(fun id ->
        List.mem remove_collections id ~equal:Id.Collection.equal)
    then
      Error
        (Error.invalid_request "organization collection add/remove operations overlap")
    else Ok { project; add_collections; remove_collections }
  ;;

  let requested_groups t =
    Values.
      { project_id =
          (match t.project with
           | Keep | Clear -> None
           | Set id -> Some id)
      ; collection_ids = t.add_collections
      }
  ;;

  let apply t ~previous =
    let project_id =
      match t.project with
      | Keep -> previous.Values.project_id
      | Clear -> None
      | Set id -> Some id
    in
    let retained =
      List.filter previous.collection_ids ~f:(fun id ->
        not (List.mem t.remove_collections id ~equal:Id.Collection.equal))
    in
    let additions =
      List.filter t.add_collections ~f:(fun id ->
        not (List.mem retained id ~equal:Id.Collection.equal))
    in
    Values.create ~project_id ~collection_ids:(retained @ additions)
  ;;

  let to_json t =
    let project =
      match t.project with
      | Keep -> []
      | Clear -> [ "project_id", `Null ]
      | Set id -> [ "project_id", Id.Project.to_json id ]
    in
    `Object
      (project
       @ [ "add_collections", `Array (List.map t.add_collections ~f:Id.Collection.to_json)
         ; ( "remove_collections"
           , `Array (List.map t.remove_collections ~f:Id.Collection.to_json) )
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind project =
      match Json_codec.optional fields "project_id" with
      | None -> Ok Keep
      | Some `Null -> Ok Clear
      | Some value -> Result.map (Id.Project.of_json value) ~f:(fun id -> Set id)
    in
    let optional_list name =
      match Json_codec.optional fields name with
      | None -> Ok []
      | Some value -> Json_codec.list Id.Collection.of_json value
    in
    let%bind add_collections = optional_list "add_collections" in
    let%bind remove_collections = optional_list "remove_collections" in
    create ~project ~add_collections ~remove_collections
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match
      create
        ~project:value.project
        ~add_collections:value.add_collections
        ~remove_collections:value.remove_collections
    with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Request = struct
  type t =
    { host_id : Id.Server.t
    ; session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_metadata_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "host_id", Id.Server.to_json t.host_id
      ; "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; ( "expected_metadata_revision"
        , `Number (Int64.to_string t.expected_metadata_revision) )
      ; "patch", Patch.to_json t.patch
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind host_id = Json_codec.required_as fields "host_id" Id.Server.of_json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id =
      Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
    in
    let%bind expected_metadata_revision =
      Json_codec.required_as
        fields
        "expected_metadata_revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    let%bind patch = Json_codec.required_as fields "patch" Patch.of_json in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { host_id
    ; session_id
    ; attachment_id
    ; expected_metadata_revision
    ; patch
    ; idempotency_key
    }
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Query = struct
  module Project = struct
    type t =
      | Any
      | Unassigned
      | Project of Id.Project.t
    [@@deriving equal, sexp]
  end

  type t =
    { project : Project.t
    ; collection_all_of : Id.Collection.t list
    }
  [@@deriving equal, sexp]

  let default = { project = Any; collection_all_of = [] }

  let create ~project ~collection_all_of =
    Result.map (collections collection_all_of) ~f:(fun collection_all_of ->
      { project; collection_all_of })
  ;;

  let requires_organization_view t =
    (not (Project.equal t.project Any)) || not (List.is_empty t.collection_all_of)
  ;;

  let matches t ~effective =
    (match t.project with
     | Any -> true
     | Unassigned -> Option.is_none effective.Values.project_id
     | Project id -> Option.equal Id.Project.equal effective.project_id (Some id))
    && List.for_all t.collection_all_of ~f:(fun id ->
      List.mem effective.collection_ids id ~equal:Id.Collection.equal)
  ;;

  let to_fields t =
    (match t.project with
     | Any -> []
     | Unassigned -> [ "project_filter", `String "unassigned" ]
     | Project id -> [ "project_filter", Id.Project.to_json id ])
    @ [ ( "collection_all_of"
        , `Array (List.map t.collection_all_of ~f:Id.Collection.to_json) )
      ]
  ;;

  let of_fields fields =
    let open Result.Let_syntax in
    let%bind project =
      match Json_codec.optional fields "project_filter" with
      | None -> Ok Project.Any
      | Some (`String "unassigned") -> Ok Unassigned
      | Some json ->
        Result.map (Id.Project.of_json json) ~f:(fun id -> Project.Project id)
    in
    let%bind collection_all_of =
      match Json_codec.optional fields "collection_all_of" with
      | None -> Ok []
      | Some json -> Json_codec.list Id.Collection.of_json json
    in
    create ~project ~collection_all_of
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match create ~project:value.project ~collection_all_of:value.collection_all_of with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end
