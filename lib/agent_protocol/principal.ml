open Core

type t =
  { id : Id.Principal.t
  ; authentication_kind : string
  ; scopes : Scope.Set.t
  ; attributes : (string * string) list
  }
[@@deriving sexp]

let is_kind_character = function
  | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' -> true
  | _ -> false
;;

let validate_authentication_kind kind =
  if String.is_empty kind || not (String.for_all kind ~f:is_kind_character)
  then Error (Protocol_error.invalid_request "authentication kind is invalid")
  else Ok ()
;;

let validate_attributes attributes =
  let names = List.map attributes ~f:fst in
  if List.exists names ~f:String.is_empty
  then Error (Protocol_error.invalid_request "principal attribute key must be nonempty")
  else (
    match List.find_a_dup names ~compare:String.compare with
    | None -> Ok ()
    | Some name ->
      Error (Protocol_error.invalid_request ("duplicate principal attribute: " ^ name)))
;;

let create ~id ~authentication_kind ~scopes ~attributes =
  let open Result.Let_syntax in
  let%bind () = validate_authentication_kind authentication_kind in
  let%map () = validate_attributes attributes in
  let attributes =
    List.sort attributes ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  { id; authentication_kind; scopes; attributes }
;;

let has_scope t scope = Core.Set.mem t.scopes scope

let to_json t =
  let attributes =
    `Object (List.map t.attributes ~f:(fun (name, value) -> name, `String value))
  in
  `Object
    [ "id", Id.Principal.to_json t.id
    ; "authentication_kind", `String t.authentication_kind
    ; "scopes", Scope.set_to_json t.scopes
    ; "attributes", attributes
    ]
;;

let attributes_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  Result.all
    (List.map (Json_codec.to_alist fields) ~f:(fun (name, value) ->
       let%map value = Json_codec.string value in
       name, value))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id = Json_codec.required_as fields "id" Id.Principal.of_json in
  let%bind authentication_kind =
    Json_codec.required_as fields "authentication_kind" Json_codec.string
  in
  let%bind scopes = Json_codec.required_as fields "scopes" Scope.set_of_json in
  let%bind attributes = Json_codec.optional_as fields "attributes" attributes_of_json in
  create
    ~id
    ~authentication_kind
    ~scopes
    ~attributes:(Option.value attributes ~default:[])
;;
