open Core
module Error = Protocol_error

module Project_deleted = struct
  type t =
    { id : Id.Project.t
    ; revision : int64
    ; deleted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  let create ~id ~revision ~deleted_at =
    if Int64.(revision < 1L)
    then Error (Error.invalid_request "deleted group revision must be positive")
    else Ok { id; revision; deleted_at }
  ;;

  let to_json t =
    `Object
      [ "id", Id.Project.to_json t.id
      ; "revision", `Number (Int64.to_string t.revision)
      ; "deleted_at", Timestamp.to_json t.deleted_at
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = Json_codec.fields json in
    let%bind id = Json_codec.required_as f "id" Id.Project.of_json in
    let%bind revision =
      Json_codec.required_as
        f
        "revision"
        (Json_codec.bounded_int64 ~min:1L ~max:Int64.max_value)
    in
    let%bind deleted_at = Json_codec.required_as f "deleted_at" Timestamp.of_json in
    create ~id ~revision ~deleted_at
  ;;

  let t_of_sexp sexp =
    let value = t_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error ->
      Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
  ;;
end

module Collection_deleted = struct
  type t =
    { id : Id.Collection.t
    ; revision : int64
    ; deleted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  let create ~id ~revision ~deleted_at =
    if Int64.(revision < 1L)
    then Error (Error.invalid_request "deleted group revision must be positive")
    else Ok { id; revision; deleted_at }
  ;;

  let to_json t =
    `Object
      [ "id", Id.Collection.to_json t.id
      ; "revision", `Number (Int64.to_string t.revision)
      ; "deleted_at", Timestamp.to_json t.deleted_at
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = Json_codec.fields json in
    let%bind id = Json_codec.required_as f "id" Id.Collection.of_json in
    let%bind revision =
      Json_codec.required_as
        f
        "revision"
        (Json_codec.bounded_int64 ~min:1L ~max:Int64.max_value)
    in
    let%bind deleted_at = Json_codec.required_as f "deleted_at" Timestamp.of_json in
    create ~id ~revision ~deleted_at
  ;;

  let t_of_sexp sexp =
    let value = t_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error ->
      Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
  ;;
end

type t =
  | Project_created of Organization_group.Project.t
  | Project_updated of Organization_group.Project.t
  | Project_deleted of Project_deleted.t
  | Collection_created of Organization_group.Collection.t
  | Collection_updated of Organization_group.Collection.t
  | Collection_deleted of Collection_deleted.t
[@@deriving equal, sexp]

let to_json t =
  let tag, value =
    match t with
    | Project_created value -> "project.created", Organization_group.Project.to_json value
    | Project_updated value -> "project.updated", Organization_group.Project.to_json value
    | Project_deleted value -> "project.deleted", Project_deleted.to_json value
    | Collection_created value ->
      "collection.created", Organization_group.Collection.to_json value
    | Collection_updated value ->
      "collection.updated", Organization_group.Collection.to_json value
    | Collection_deleted value -> "collection.deleted", Collection_deleted.to_json value
  in
  `Object [ "kind", `String tag; "value", value ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = Json_codec.fields json in
  let%bind tag = Json_codec.required_as f "kind" Json_codec.string in
  let%bind value = Json_codec.required f "value" in
  match tag with
  | "project.created" ->
    Result.map (Organization_group.Project.of_json value) ~f:(fun v -> Project_created v)
  | "project.updated" ->
    Result.map (Organization_group.Project.of_json value) ~f:(fun v -> Project_updated v)
  | "project.deleted" ->
    Result.map (Project_deleted.of_json value) ~f:(fun v -> Project_deleted v)
  | "collection.created" ->
    Result.map (Organization_group.Collection.of_json value) ~f:(fun v ->
      Collection_created v)
  | "collection.updated" ->
    Result.map (Organization_group.Collection.of_json value) ~f:(fun v ->
      Collection_updated v)
  | "collection.deleted" ->
    Result.map (Collection_deleted.of_json value) ~f:(fun v -> Collection_deleted v)
  | _ -> Error (Error.invalid_request "unknown organization result kind")
;;

let t_of_sexp sexp =
  let value = t_of_sexp sexp in
  match of_json (to_json value) with
  | Ok value -> value
  | Error error ->
    Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
;;
