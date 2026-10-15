open! Core
module J = Json_codec
module P = Protocol_error

type t =
  { revision_id : Id.Prompt_revision.t
  ; manifest_sha256 : Chatml_source.Digest.t
  ; root_relative_path : Source.Portable_path.t
  }

type artifact = t

let create ~revision_id ~manifest_sha256 ~root_relative_path =
  { revision_id; manifest_sha256; root_relative_path }
;;

let revision_id t = t.revision_id
let manifest_sha256 t = t.manifest_sha256
let root_relative_path t = t.root_relative_path
let invalid message = Error (P.invalid_request message)

let strict_fields names json =
  let open Result.Let_syntax in
  let rec within remaining = function
    | [] -> true
    | (name, _) :: rest ->
      remaining > 0
      && List.mem names name ~equal:String.equal
      && within (remaining - 1) rest
  in
  let%bind () =
    match json with
    | `Object fields when within (List.length names) fields -> Ok ()
    | `Object _ -> invalid "unknown saved Agent request field"
    | _ -> invalid "expected a JSON object"
  in
  J.fields json
;;

let to_json t =
  `Object
    [ "revision_id", Id.Prompt_revision.to_json t.revision_id
    ; "manifest_sha256", `String (Chatml_source.Digest.to_hex t.manifest_sha256)
    ; "root_relative_path", `String (Source.Portable_path.to_string t.root_relative_path)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields =
    strict_fields [ "revision_id"; "manifest_sha256"; "root_relative_path" ] json
  in
  let%bind revision_id = J.required_as fields "revision_id" Id.Prompt_revision.of_json in
  let%bind manifest_sha256 = J.required_as fields "manifest_sha256" J.string in
  let%bind manifest_sha256 =
    Chatml_source.Digest.of_hex manifest_sha256
    |> Result.map_error ~f:(fun _ -> P.invalid_request "invalid selected artifact digest")
  in
  let%bind root_relative_path = J.required_as fields "root_relative_path" J.string in
  let%map root_relative_path =
    Source.Portable_path.create root_relative_path
    |> Result.map_error ~f:(fun _ ->
      P.invalid_request "invalid selected artifact root member")
  in
  create ~revision_id ~manifest_sha256 ~root_relative_path
;;

module Saved_tool_request = struct
  type nonrec artifact = artifact

  type t =
    { artifact : artifact
    ; selected_name : string
    }

  let artifact t = t.artifact
  let selected_name t = t.selected_name

  let valid_digest value =
    String.length value = 64
    && String.for_all value ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  ;;

  let of_payload json =
    let open Result.Let_syntax in
    let%bind fields =
      strict_fields
        [ "version"; "target"; "input"; "pins"; "policy"; "agent_artifact" ]
        json
    in
    let%bind _version = J.required_as fields "version" (J.bounded_int ~min:2 ~max:2) in
    let%bind target =
      J.required fields "target" |> Result.bind ~f:(strict_fields [ "kind"; "name" ])
    in
    let%bind kind = J.required_as target "kind" J.string in
    let%bind () =
      if String.equal kind "tool" then Ok () else invalid "saved Agent requires Tool"
    in
    let%bind selected_name = J.required_as target "name" J.string in
    let%bind () =
      if String.is_empty selected_name || String.length selected_name > 256
      then invalid "invalid saved Agent Tool name"
      else Ok ()
    in
    let%bind pins = J.required fields "pins" in
    let%bind pin_fields = strict_fields [ selected_name ] pins in
    let%bind pin = J.required_as pin_fields selected_name J.string in
    let%bind () =
      if valid_digest pin then Ok () else invalid "invalid saved Agent capability pin"
    in
    let%bind artifact = J.required_as fields "agent_artifact" of_json in
    let%bind _input = J.required fields "input" in
    let%bind _policy = J.required fields "policy" in
    Ok { artifact; selected_name }
  ;;
end
