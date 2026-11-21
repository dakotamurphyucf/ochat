open! Core
module J = Json_codec
module Revision = History.Content_revision

module Digest = struct
  include Chatml_source.Digest

  let compare a b = String.compare (to_hex a) (to_hex b)
end

type origin =
  | Unchanged of Revision.t
  | Inserted of
      { incarnation : int64
      ; change_id : int
      }
  | Replaced of
      { incarnation : int64
      ; change_id : int
      ; base : Revision.t
      }
  | Captured of { session_revision : int64 }
[@@deriving compare, equal, sexp_of]

type t =
  | Canonical of Revision.t
  | Effective of
      { origin : origin
      ; digest : Digest.t
      }
[@@deriving compare, equal, sexp_of]

let canonical revision = Canonical revision

let effective ~origin ~digest =
  let valid =
    match origin with
    | Unchanged _ -> true
    | Inserted { incarnation; change_id } | Replaced { incarnation; change_id; _ } ->
      Int64.(incarnation >= 0L) && change_id >= 0
    | Captured { session_revision } -> Int64.(session_revision >= 0L)
  in
  if valid
  then Ok (Effective { origin; digest })
  else Error (Protocol_error.invalid_request "negative effective history coordinate")
;;

let origin_to_json = function
  | Unchanged rev -> `Object [ "kind", `String "unchanged"; "base", Revision.to_json rev ]
  | Captured { session_revision } ->
    `Object
      [ "kind", `String "captured"
      ; "session_revision", Recovery_wire.revision_to_json session_revision
      ]
  | Inserted { incarnation; change_id } ->
    `Object
      [ "kind", `String "inserted"
      ; "incarnation", Recovery_wire.revision_to_json incarnation
      ; "change_id", `Number (Int.to_string change_id)
      ]
  | Replaced { incarnation; change_id; base } ->
    `Object
      [ "kind", `String "replaced"
      ; "incarnation", Recovery_wire.revision_to_json incarnation
      ; "change_id", `Number (Int.to_string change_id)
      ; "base", Revision.to_json base
      ]
;;

let origin_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  let change () =
    let%bind incarnation =
      J.required_as fields "incarnation" Recovery_wire.revision_of_json
    in
    let%map change_id =
      J.required_as fields "change_id" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    incarnation, change_id
  in
  match kind with
  | "unchanged" ->
    let%bind () = Extension_codec.closed fields [ "kind"; "base" ] in
    let%map base = J.required_as fields "base" Revision.of_json in
    Unchanged base
  | "captured" ->
    let%bind () = Extension_codec.closed fields [ "kind"; "session_revision" ] in
    let%map session_revision =
      J.required_as fields "session_revision" Recovery_wire.revision_of_json
    in
    Captured { session_revision }
  | "inserted" ->
    let%bind () = Extension_codec.closed fields [ "kind"; "incarnation"; "change_id" ] in
    let%map incarnation, change_id = change () in
    Inserted { incarnation; change_id }
  | "replaced" ->
    let%bind () =
      Extension_codec.closed fields [ "kind"; "incarnation"; "change_id"; "base" ]
    in
    let%bind incarnation, change_id = change () in
    let%map base = J.required_as fields "base" Revision.of_json in
    Replaced { incarnation; change_id; base }
  | _ -> Error (Protocol_error.invalid_request "unknown effective member origin")
;;

let to_json = function
  | Canonical rev ->
    `Object [ "view", `String "canonical"; "revision", Revision.to_json rev ]
  | Effective { origin; digest } ->
    `Object
      [ "view", `String "effective"
      ; "origin", origin_to_json origin
      ; "sha256", `String (Digest.to_hex digest)
      ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Recovery_wire.admit_coordinate json in
  let%bind fields = J.fields json in
  let%bind view = J.required_as fields "view" J.string in
  match view with
  | "canonical" ->
    let%bind () = Extension_codec.closed fields [ "view"; "revision" ] in
    let%map rev = J.required_as fields "revision" Revision.of_json in
    canonical rev
  | "effective" ->
    let%bind () = Extension_codec.closed fields [ "view"; "origin"; "sha256" ] in
    let%bind origin = J.required_as fields "origin" origin_of_json in
    let%bind hex = J.required_as fields "sha256" J.string in
    let%bind digest =
      Digest.of_hex hex
      |> Result.map_error ~f:(fun _ ->
        Protocol_error.invalid_request "invalid effective member SHA-256")
    in
    effective ~origin ~digest
  | _ -> Error (Protocol_error.invalid_request "unknown recovery evidence view")
;;
