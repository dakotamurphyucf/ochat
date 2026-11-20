open! Core
module P = Agent_protocol
module X = Persistence_codec
module D = Document_schema
module J = P.Json_codec
module Manifest = P.Recovery_window
module Scope = Manifest.Scope
module Window_id = P.Id.Context_window
module Operation = P.Id.Operation
module Source = Recovery_window_location.Source
module Member = Recovery_window_location.Member

let invalid message = Error (P.Error.invalid_request message)
let max_members = 8192

type origin =
  | Root
  | Successor of
      { previous : Window_id.t
      ; operation : Operation.t
      }
  | New_generation of { previous : Window_id.t }
[@@deriving sexp_of]

type state =
  | Open
  | Sealed
[@@deriving equal, sexp_of]

type t =
  { manifest : Manifest.t
  ; origin : origin
  ; state : state
  ; members : Member.t array
  }

let create scope ~origin ~state members =
  let open Result.Let_syntax in
  if Stdlib.List.compare_length_with members max_members > 0
  then invalid "recovery window exceeds the ledger member limit"
  else (
    let%bind () =
      List.fold_result members ~init:() ~f:(fun () member ->
        match state, Member.source member with
        | Sealed, Source.Live _ when Member.is_withdrawn member -> Ok ()
        | Sealed, Source.Live _ -> invalid "sealed recovery window contains a live member"
        | Open, (Live _ | Archive _) | Sealed, Archive _ -> Ok ())
    in
    let%map manifest = Manifest.create scope (List.map members ~f:Member.citation) in
    { manifest; origin; state; members = Array.of_list members })
;;

let manifest t = t.manifest
let origin t = t.origin
let state t = t.state
let length t = Array.length t.members
let has_withdrawals t = Array.exists t.members ~f:Member.is_withdrawn

let at t ~ordinal =
  if ordinal < 0 || ordinal >= length t then None else Some t.members.(ordinal)
;;

let fold_members t ~init ~f = Array.fold_result t.members ~init ~f
let scope t = Manifest.scope (manifest t)
let id t = Scope.window (scope t)

let origin_to_jsonaf = function
  | Root -> `Object [ "kind", `String "root" ]
  | Successor { previous; operation } ->
    `Object
      [ "kind", `String "successor"
      ; "previous", Window_id.to_json previous
      ; "operation", Operation.to_json operation
      ]
  | New_generation { previous } ->
    `Object [ "kind", `String "new_generation"; "previous", Window_id.to_json previous ]
;;

let origin_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind kind = X.required fields "kind" J.string in
  match kind with
  | "root" -> Ok Root
  | "successor" ->
    let%bind previous = X.required fields "previous" Window_id.of_json in
    let%map operation = X.required fields "operation" Operation.of_json in
    Successor { previous; operation }
  | "new_generation" ->
    let%map previous = X.required fields "previous" Window_id.of_json in
    New_generation { previous }
  | _ -> invalid "unknown recovery window origin"
;;

let to_jsonaf t =
  `Object
    [ "id", Window_id.to_json (id t)
    ; "generation", X.host_counter_to_json (Scope.generation (scope t))
    ; "origin", origin_to_jsonaf t.origin
    ; ( "state"
      , `String
          (match t.state with
           | Open -> "open"
           | Sealed -> "sealed") )
    ; "members", `Array (Array.to_list t.members |> List.map ~f:Member.to_jsonaf)
    ]
;;

let state_of_jsonaf =
  J.enum ~name:"recovery window state" [ "open", Open; "sealed", Sealed ]
;;

let of_jsonaf owner json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind window = X.required fields "id" Window_id.of_json in
  let%bind generation = X.required fields "generation" X.host_counter_of_json in
  let%bind scope = Scope.create ~session:owner ~generation ~window in
  let%bind origin = X.required fields "origin" origin_of_jsonaf in
  let%bind state = X.required fields "state" state_of_jsonaf in
  let%bind members =
    X.required fields "members" (function
      | `Array members when Stdlib.List.compare_length_with members max_members <= 0 ->
        Ok members
      | `Array _ -> invalid "recovery window exceeds the ledger member limit"
      | _ -> invalid "recovery members must be an array")
  in
  let%bind members = List.map members ~f:(Member.of_jsonaf scope) |> Result.all in
  create scope ~origin ~state members
;;

let shape =
  X.shape_exn
    [ "id", D.Shape.value
    ; "generation", D.Shape.value
    ; ( "origin"
      , X.tagged_shape_exn
          ~discriminator:"kind"
          [ "root", X.fields_shape [ "kind" ]
          ; "successor", X.fields_shape [ "kind"; "previous"; "operation" ]
          ; "new_generation", X.fields_shape [ "kind"; "previous" ]
          ] )
    ; "state", D.Shape.value
    ; "members", X.array_shape_exn ~identity_field:"key" Member.shape
    ]
;;
