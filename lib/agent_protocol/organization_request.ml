open Core
module Error = Protocol_error

module Create = struct
  type t =
    { host_id : Id.Server.t
    ; name : Organization_group.Name.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "host_id", Id.Server.to_json t.host_id
      ; "name", Organization_group.Name.to_json t.name
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = Json_codec.fields json in
    let%bind host_id = Json_codec.required_as f "host_id" Id.Server.of_json in
    let%bind name = Json_codec.required_as f "name" Organization_group.Name.of_json in
    let%map idempotency_key =
      Json_codec.required_as f "idempotency_key" Idempotency_key.of_json
    in
    { host_id; name; idempotency_key }
  ;;

  let t_of_sexp sexp =
    let value = t_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error ->
      Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
  ;;
end

module List = struct
  type t =
    { host_id : Id.Server.t
    ; creator_principal_id : Id.Principal.t option
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      ([ "host_id", Id.Server.to_json t.host_id ]
       @ Page.Request.to_fields t.page
       @ Option.value_map t.creator_principal_id ~default:[] ~f:(fun id ->
         [ "creator_principal_id", Id.Principal.to_json id ]))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = Json_codec.fields json in
    let%bind host_id = Json_codec.required_as f "host_id" Id.Server.of_json in
    let%bind creator_principal_id =
      Json_codec.optional_as f "creator_principal_id" Id.Principal.of_json
    in
    let%map page = Page.Request.of_fields f in
    { host_id; creator_principal_id; page }
  ;;

  let t_of_sexp sexp =
    let value = t_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error ->
      Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
  ;;
end

module type S = sig
  type id [@@deriving sexp]

  module Get : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Update : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      ; expected_revision : int64
      ; name : Organization_group.Name.t
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Delete : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      ; expected_revision : int64
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end
end

module Make (Identifier : Id.S) = struct
  type id = Identifier.t [@@deriving sexp]

  module Get = struct
    type t =
      { host_id : Id.Server.t
      ; id : Identifier.t
      }
    [@@deriving sexp]

    let to_json t =
      `Object [ "host_id", Id.Server.to_json t.host_id; "id", Identifier.to_json t.id ]
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind f = Json_codec.fields json in
      let%bind host_id = Json_codec.required_as f "host_id" Id.Server.of_json in
      let%bind id = Json_codec.required_as f "id" Identifier.of_json in
      Ok { host_id; id }
    ;;

    let t_of_sexp sexp =
      let value = t_of_sexp sexp in
      match of_json (to_json value) with
      | Ok value -> value
      | Error error ->
        Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
    ;;
  end

  module Update = struct
    type t =
      { host_id : Id.Server.t
      ; id : Identifier.t
      ; expected_revision : int64
      ; name : Organization_group.Name.t
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    let to_json t =
      `Object
        [ "host_id", Id.Server.to_json t.host_id
        ; "id", Identifier.to_json t.id
        ; "expected_revision", `Number (Int64.to_string t.expected_revision)
        ; "name", Organization_group.Name.to_json t.name
        ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
        ]
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind f = Json_codec.fields json in
      let%bind host_id = Json_codec.required_as f "host_id" Id.Server.of_json in
      let%bind id = Json_codec.required_as f "id" Identifier.of_json in
      let%bind expected_revision =
        Json_codec.required_as
          f
          "expected_revision"
          (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
      in
      let%bind name = Json_codec.required_as f "name" Organization_group.Name.of_json in
      let%bind idempotency_key =
        Json_codec.required_as f "idempotency_key" Idempotency_key.of_json
      in
      Ok { host_id; id; expected_revision; name; idempotency_key }
    ;;

    let t_of_sexp sexp =
      let value = t_of_sexp sexp in
      match of_json (to_json value) with
      | Ok value -> value
      | Error error ->
        Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
    ;;
  end

  module Delete = struct
    type t =
      { host_id : Id.Server.t
      ; id : Identifier.t
      ; expected_revision : int64
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    let to_json t =
      `Object
        [ "host_id", Id.Server.to_json t.host_id
        ; "id", Identifier.to_json t.id
        ; "expected_revision", `Number (Int64.to_string t.expected_revision)
        ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
        ]
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind f = Json_codec.fields json in
      let%bind host_id = Json_codec.required_as f "host_id" Id.Server.of_json in
      let%bind id = Json_codec.required_as f "id" Identifier.of_json in
      let%bind expected_revision =
        Json_codec.required_as
          f
          "expected_revision"
          (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
      in
      let%bind idempotency_key =
        Json_codec.required_as f "idempotency_key" Idempotency_key.of_json
      in
      Ok { host_id; id; expected_revision; idempotency_key }
    ;;

    let t_of_sexp sexp =
      let value = t_of_sexp sexp in
      match of_json (to_json value) with
      | Ok value -> value
      | Error error ->
        Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
    ;;
  end
end

module Project = Make (Id.Project)
module Collection = Make (Id.Collection)
