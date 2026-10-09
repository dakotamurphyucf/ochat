open! Core
module D = Openai.Responses_driver

module Error = struct
  type t =
    | Invalid_descriptor
    | Invalid_document
  [@@deriving equal, sexp_of]
end

type t =
  { id : string
  ; credential_owner : string
  ; revision : string
  ; defaults : D.Setting.t list
  }

let valid_label value =
  (not (String.is_empty (String.strip value)))
  && String.length value <= 256
  && (not (String.exists value ~f:(fun c -> Char.to_int c < 32 || Char.to_int c = 127)))
  && Result.is_ok
       (Document_schema.Json.validate
          (`String value)
          ~limits:Document_schema.Limits.default)
;;

let valid_defaults defaults =
  let module Request = Openai.Responses_codec.Request in
  let open Or_error.Let_syntax in
  let result =
    let%bind base =
      Request.create ~model:"host-profile-validation" ~input:[] ~stream:true ()
    in
    let fields =
      match Request.jsonaf_of_t base with
      | `Object fields -> fields
      | _ -> assert false
    in
    let%bind additions =
      List.map defaults ~f:(fun setting ->
        match D.Setting.value setting with
        | Absent -> Or_error.error_string "absent profile default"
        | Null -> Ok (D.Setting.name setting, `Null)
        | Value value -> Ok (D.Setting.name setting, value))
      |> Or_error.all
    in
    Request.of_jsonaf (`Object (fields @ additions))
  in
  Result.is_ok result
;;

let create ~id ~credential_owner ~revision ~defaults =
  if
    (not (List.for_all [ id; credential_owner; revision ] ~f:valid_label))
    || String.equal id credential_owner
    || List.length defaults > 64
    || List.exists defaults ~f:(fun setting ->
      not (D.Setting.equal_provenance (D.Setting.provenance setting) Profile_default))
    || List.contains_dup (List.map defaults ~f:D.Setting.name) ~compare:String.compare
    || not (valid_defaults defaults)
  then Error Error.Invalid_descriptor
  else Ok { id; credential_owner; revision; defaults }
;;

let id t = t.id
let credential_owner t = t.credential_owner
let revision t = t.revision

let derive t ~canonical =
  if not (String.equal (D.Profile.id canonical) t.credential_owner)
  then Error Error.Invalid_descriptor
  else
    D.Profile.with_configuration canonical ~id:t.id ~defaults:t.defaults
    |> Result.map_error ~f:(fun _ -> Error.Invalid_descriptor)
;;

let document_limits =
  Document_schema.Limits.create
    ~max_bytes:1_048_576
    ~max_depth:16
    ~max_fields:8192
    ~max_nodes:16384
  |> Result.map_error ~f:(fun _ -> "invalid static profile-choice limits")
  |> Result.ok_or_failwith
;;

let of_json json =
  let invalid = Error Error.Invalid_document in
  let descriptor = function
    | `Object fields ->
      let names = List.map fields ~f:fst in
      if
        List.contains_dup names ~compare:String.compare
        || not
             (List.equal
                String.equal
                (List.sort names ~compare:String.compare)
                [ "credential_owner"; "defaults"; "id"; "revision" ])
      then invalid
      else (
        match
          ( List.Assoc.find fields "id" ~equal:String.equal
          , List.Assoc.find fields "credential_owner" ~equal:String.equal
          , List.Assoc.find fields "revision" ~equal:String.equal
          , List.Assoc.find fields "defaults" ~equal:String.equal )
        with
        | ( Some (`String id)
          , Some (`String credential_owner)
          , Some (`String revision)
          , Some (`Object defaults) ) ->
          let open Result.Let_syntax in
          let%bind defaults =
            List.map defaults ~f:(fun (name, value) ->
              D.Setting.create
                ~name
                ~value:
                  (match value with
                   | `Null -> Null
                   | value -> Value value)
                ~provenance:Profile_default
              |> Result.map_error ~f:(fun _ -> Error.Invalid_document))
            |> Result.all
          in
          create ~id ~credential_owner ~revision ~defaults
        | _ -> invalid)
    | _ -> invalid
  in
  match Document_schema.Json.validate json ~limits:document_limits, json with
  | Ok (), `Array entries when List.length entries <= 128 ->
    let open Result.Let_syntax in
    let%bind choices = List.map entries ~f:descriptor |> Result.all in
    if List.contains_dup (List.map choices ~f:id) ~compare:String.compare
    then invalid
    else Ok choices
  | _ -> invalid
;;

let of_string contents =
  let open Result.Let_syntax in
  let%bind json =
    Document_schema.Json.decode contents ~limits:document_limits
    |> Result.map_error ~f:(fun _ -> Error.Invalid_document)
  in
  of_json json
;;

let validate_set choices ~credential_owners =
  let owners = String.Set.of_list credential_owners in
  if
    Set.length owners + List.length choices > 128
    || (not (Set.for_all owners ~f:valid_label))
    || List.contains_dup (List.map choices ~f:id) ~compare:String.compare
    || List.exists choices ~f:(fun choice ->
      Set.mem owners choice.id || not (Set.mem owners choice.credential_owner))
  then Error Error.Invalid_descriptor
  else Ok ()
;;
