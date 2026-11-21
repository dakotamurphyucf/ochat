open! Core

let max_members = 8192
module J = Json_codec
module Version = Recovery_item_version

let invalid message = Error (Protocol_error.invalid_request message)

module Wire = Recovery_wire

module Scope = struct
  type t =
    { session : Session_ref.t
    ; generation : int
    ; window : Id.Context_window.t
    }
  [@@deriving compare, equal, sexp_of]

  let create ~session ~generation ~window =
    let open Result.Let_syntax in
    let%bind _ = Session_ref.of_json (Session_ref.to_json session) in
    let%bind _ = Id.Context_window.of_string (Id.Context_window.to_string window) in
    if generation < 0
    then invalid "negative recovery window generation"
    else Ok { session; generation; window }
  ;;

  let session t = t.session
  let generation t = t.generation
  let window t = t.window

  let to_json t =
    `Object
      [ "session", Session_ref.to_json t.session
      ; "generation", `Number (Int.to_string t.generation)
      ; "window", Id.Context_window.to_json t.window
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Wire.admit_coordinate json in
    let%bind f = Wire.fields json [ "session"; "generation"; "window" ] in
    let%bind session_json = J.required f "session" in
    let%bind _ = Wire.fields session_json [ "session_id"; "server_id" ] in
    let%bind session = Session_ref.of_json session_json in
    let%bind generation =
      J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind window = J.required_as f "window" Id.Context_window.of_json in
    create ~session ~generation ~window
  ;;
end

module Member = struct
  module T = struct
    type t =
      { occurrence : History.Id.t
      ; version : Version.t
      }
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let create ~occurrence ~version =
    let namespace = History_entry.Id.namespace occurrence in
    if String.length namespace > 4096 || not (Stdlib.String.is_valid_utf_8 namespace)
    then invalid "recovery occurrence namespace exceeds 4096 UTF-8 bytes or is malformed"
    else Ok { occurrence; version }
  ;;

  let to_json t =
    `Object
      [ "occurrence", History.Id.to_json t.occurrence
      ; "version", Version.to_json t.version
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = Wire.fields json [ "occurrence"; "version" ] in
    let%bind occurrence = J.required_as f "occurrence" History.Id.of_json in
    let%bind version = J.required_as f "version" Version.of_json in
    create ~occurrence ~version
  ;;
end

module Citation = struct
  type t =
    { scope : Scope.t
    ; member : Member.t
    }
  [@@deriving equal, sexp_of]

  let create scope ~occurrence ~version =
    let%map.Result member = Member.create ~occurrence ~version in
    { scope; member }
  ;;

  let scope t = t.scope
  let occurrence t = t.member.occurrence
  let version t = t.member.version

  let to_json t =
    `Object [ "scope", Scope.to_json t.scope; "member", Member.to_json t.member ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Wire.admit_coordinate json in
    let%bind f = Wire.fields json [ "scope"; "member" ] in
    let%bind scope = J.required_as f "scope" Scope.of_json in
    let%map member = J.required_as f "member" Member.of_json in
    { scope; member }
  ;;
end

type t =
  { scope : Scope.t
  ; members : Member.t array
  ; ordinals : (Member.t, int, Member.comparator_witness) Map.t
  }

let max_members = 65_536
let max_bytes = 8 * 1024 * 1024

let limits =
  Wire.limits ~max_bytes ~max_fields:(16 * max_members) ~max_nodes:(24 * max_members)
;;

let measure json = Wire.measure ~limits json

let encode scope members =
  `Object [ "scope", Scope.to_json scope; "members", `Array members ]
;;

let build scope values ~decode =
  let open Result.Let_syntax in
  let%bind header = measure (encode scope []) in
  let rec loop ordinal bytes members ordinals = function
    | [] -> Ok { scope; members = Array.of_list (List.rev members); ordinals }
    | _ :: _ when ordinal >= max_members ->
      invalid "recovery window exceeds the member limit"
    | value :: rest ->
      let%bind member = decode value in
      let%bind size = measure (Member.to_json member) in
      let bytes = bytes + size + if ordinal = 0 then 0 else 1 in
      if bytes > max_bytes
      then invalid "recovery window exceeds the encoded byte limit"
      else (
        match Map.add ordinals ~key:member ~data:ordinal with
        | `Duplicate -> invalid "duplicate recovery window member"
        | `Ok ordinals -> loop (ordinal + 1) bytes (member :: members) ordinals rest)
  in
  loop 0 header [] (Map.empty (module Member)) values
;;

let create scope citations =
  build scope citations ~decode:(fun (c : Citation.t) ->
    if Scope.equal c.scope scope
    then Ok c.member
    else invalid "foreign recovery window member")
;;

let scope t = t.scope
let length t = Array.length t.members

let at t ~ordinal =
  if ordinal < 0 || ordinal >= length t
  then None
  else Some Citation.{ scope = t.scope; member = t.members.(ordinal) }
;;

let ordinal t (c : Citation.t) =
  if Scope.equal t.scope c.scope then Map.find t.ordinals c.member else None
;;

let to_json t = encode t.scope (Array.to_list t.members |> List.map ~f:Member.to_json)

let of_json json =
  let open Result.Let_syntax in
  let%bind _ = measure json in
  let%bind f = Wire.fields json [ "scope"; "members" ] in
  let%bind scope = J.required_as f "scope" Scope.of_json in
  let%bind members = J.required f "members" in
  match members with
  | `Array values -> build scope values ~decode:Member.of_json
  | _ -> invalid "recovery window members must be an array"
;;
