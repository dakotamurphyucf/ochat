open! Core
module Error = Protocol_error

module Values = struct
  type t =
    { display_name : string option
    ; labels : (string * string) list
    }
  [@@deriving equal, sexp]

  let text ~name ~max_bytes ~nonempty value =
    let valid_utf8 =
      Uutf.String.fold_utf_8
        (fun valid _ -> function
           | `Uchar _ -> valid
           | `Malformed _ -> false)
        true
        value
    in
    if
      (not valid_utf8)
      || (nonempty && String.is_empty value)
      || String.length value > max_bytes
      || String.exists value ~f:(fun c -> Char.to_int c < 32 || Char.to_int c = 127)
    then
      Error (Error.invalid_request (name ^ " is empty, oversized or contains controls"))
    else Ok ()
  ;;

  let create ~display_name ~labels =
    if Option.is_some (List.find_a_dup (List.map labels ~f:fst) ~compare:String.compare)
    then Error (Error.invalid_request "label keys must be unique")
    else
      Ok
        { display_name
        ; labels = List.sort labels ~compare:(fun (a, _) (b, _) -> String.compare a b)
        }
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match create ~display_name:value.display_name ~labels:value.labels with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Patch = struct
  type name_change =
    | Keep
    | Set of string
    | Clear
  [@@deriving equal, sexp]

  type t =
    { name : name_change
    ; set_labels : (string * string) list
    ; remove_labels : string list
    }
  [@@deriving sexp]

  let create ~name ~set_labels ~remove_labels =
    let open Result.Let_syntax in
    let%bind () =
      match name with
      | Keep | Clear -> Ok ()
      | Set value -> Values.text ~name:"display name" ~max_bytes:1024 ~nonempty:true value
    in
    let%bind () =
      if List.length set_labels > 128 || List.length remove_labels > 128
      then Error (Error.invalid_request "metadata patch has too many labels")
      else Ok ()
    in
    let%bind () =
      Result.all_unit
        (List.map set_labels ~f:(fun (key, value) ->
           let%bind () =
             Values.text ~name:"label key" ~max_bytes:256 ~nonempty:true key
           in
           Values.text ~name:"label value" ~max_bytes:4096 ~nonempty:false value))
    in
    let%bind values = Values.create ~display_name:None ~labels:set_labels in
    let%bind _ =
      Values.create
        ~display_name:None
        ~labels:(List.map remove_labels ~f:(fun key -> key, ""))
    in
    if
      List.exists remove_labels ~f:(fun key ->
        List.Assoc.mem values.labels key ~equal:String.equal)
    then Error (Error.invalid_request "label set/remove operations overlap")
    else
      Ok
        { name
        ; set_labels = values.labels
        ; remove_labels = List.sort remove_labels ~compare:String.compare
        }
  ;;

  let apply t (values : Values.t) =
    let display_name =
      match t.name with
      | Keep -> values.display_name
      | Clear -> None
      | Set name -> Some name
    in
    let labels =
      List.filter values.labels ~f:(fun (key, _) ->
        (not (List.mem t.remove_labels key ~equal:String.equal))
        && not (List.Assoc.mem t.set_labels key ~equal:String.equal))
      @ t.set_labels
    in
    Values.create ~display_name ~labels
  ;;

  let to_json t =
    let name =
      match t.name with
      | Keep -> []
      | Clear -> [ "display_name", `Null ]
      | Set value -> [ "display_name", `String value ]
    in
    `Object
      (name
       @ [ "set_labels", `Object (List.map t.set_labels ~f:(fun (k, v) -> k, `String v))
         ; "remove_labels", `Array (List.map t.remove_labels ~f:(fun k -> `String k))
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind name =
      match Json_codec.optional fields "display_name" with
      | None -> Ok Keep
      | Some `Null -> Ok Clear
      | Some value -> Result.map (Json_codec.string value) ~f:(fun v -> Set v)
    in
    let%bind set_labels =
      match Json_codec.optional fields "set_labels" with
      | None -> Ok []
      | Some value ->
        let%bind fields = Json_codec.fields value in
        Result.all
          (List.map (Json_codec.to_alist fields) ~f:(fun (k, v) ->
             let%map v = Json_codec.string v in
             k, v))
    in
    let%bind remove_labels =
      match Json_codec.optional fields "remove_labels" with
      | None -> Ok []
      | Some value -> Json_codec.list Json_codec.string value
    in
    create ~name ~set_labels ~remove_labels
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match
      create
        ~name:value.name
        ~set_labels:value.set_labels
        ~remove_labels:value.remove_labels
    with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_metadata_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
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
    let%bind idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    if Int64.(expected_metadata_revision < zero)
    then Error (Error.invalid_request "metadata revision must be nonnegative")
    else
      Ok { session_id; attachment_id; expected_metadata_revision; patch; idempotency_key }
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end
