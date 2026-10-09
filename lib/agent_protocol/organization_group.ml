open Core
module Error = Protocol_error

module Name = struct
  type t = string [@@deriving equal, sexp]

  let create value =
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String value) in
    let rec valid () =
      match Uutf.decode decoder with
      | `End -> true
      | `Uchar uchar ->
        let scalar = Stdlib.Uchar.to_int uchar in
        scalar >= 32
        && scalar <> 127
        && (not (scalar >= 128 && scalar <= 159))
        && valid ()
      | `Malformed _ -> false
      | `Await -> assert false
    in
    if String.is_empty value || String.length value > 1024 || not (valid ())
    then
      Error
        (Error.invalid_request
           "organization name must be nonempty UTF-8 text without controls, at most 1024 \
            bytes")
    else Ok value
  ;;

  let to_string t = t
  let to_json t = `String t
  let of_json json = Result.bind (Json_codec.string json) ~f:create

  let t_of_sexp sexp =
    let value = t_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error ->
      Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
  ;;
end

module type S = sig
  type id [@@deriving equal, sexp]

  type t = private
    { id : id
    ; creator_principal_id : Id.Principal.t
    ; name : Name.t
    ; revision : int64
    ; created_at : Timestamp.t
    ; updated_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val create
    :  id:id
    -> creator_principal_id:Id.Principal.t
    -> name:Name.t
    -> revision:int64
    -> created_at:Timestamp.t
    -> updated_at:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Make (Identifier : Id.S) = struct
  type id = Identifier.t [@@deriving equal, sexp]

  type t =
    { id : id
    ; creator_principal_id : Id.Principal.t
    ; name : Name.t
    ; revision : int64
    ; created_at : Timestamp.t
    ; updated_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  let create ~id ~creator_principal_id ~name ~revision ~created_at ~updated_at =
    if Int64.(revision < 0L) || Timestamp.compare updated_at created_at < 0
    then Error (Error.invalid_request "invalid organization revision or timestamp order")
    else Ok { id; creator_principal_id; name; revision; created_at; updated_at }
  ;;

  let to_json t =
    `Object
      [ "id", Identifier.to_json t.id
      ; "creator_principal_id", Id.Principal.to_json t.creator_principal_id
      ; "name", Name.to_json t.name
      ; "revision", `Number (Int64.to_string t.revision)
      ; "created_at", Timestamp.to_json t.created_at
      ; "updated_at", Timestamp.to_json t.updated_at
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind id = Json_codec.required_as fields "id" Identifier.of_json in
    let%bind creator_principal_id =
      Json_codec.required_as fields "creator_principal_id" Id.Principal.of_json
    in
    let%bind name = Json_codec.required_as fields "name" Name.of_json in
    let%bind revision =
      Json_codec.required_as
        fields
        "revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
    let%bind updated_at = Json_codec.required_as fields "updated_at" Timestamp.of_json in
    create ~id ~creator_principal_id ~name ~revision ~created_at ~updated_at
  ;;

  let t_of_sexp sexp =
    let value = t_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error ->
      Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
  ;;
end

module Project = Make (Id.Project)
module Collection = Make (Id.Collection)
