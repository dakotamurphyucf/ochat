open! Core
module Base = Developer_repl_wire_common
module Limits = Base.Limits
module Wire_id = Base.Wire_id
module C = Developer_codec

let ( let* ) result f = Result.bind result ~f
let get = Base.get

let valid_identifier name =
  let start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let continue c = start c || Char.is_digit c in
  String.length name > 0
  && String.length name <= 128
  && start name.[0]
  && String.for_all name ~f:continue
;;

module Request = struct
  type t =
    { instance : Wire_id.Instance.t
    ; epoch : Wire_id.Epoch.t
    ; expected_revision : Wire_id.Revision.t
    ; path : string list
    ; prefix : string
    ; limit : int
    }

  let to_json t =
    `Object
      [ "instance", `String (Wire_id.Instance.to_string t.instance)
      ; "epoch", `String (Wire_id.Epoch.to_string t.epoch)
      ; "expected_revision", `String (Wire_id.Revision.to_string t.expected_revision)
      ; "path", `Array (List.map t.path ~f:(fun name -> `String name))
      ; "prefix", `String t.prefix
      ; "limit", C.integer_json t.limit
      ]
  ;;

  let create ~limits ~instance ~epoch ~expected_revision ~path ~prefix ~limit =
    if
      List.length path > 16
      || List.exists path ~f:(fun name -> not (valid_identifier name))
      || (not (String.is_empty prefix || valid_identifier prefix))
      || limit < 1
      || limit > Int.min 16 (Limits.max_page_items limits)
    then C.invalid "invalid REPL completion selection"
    else (
      let t = { instance; epoch; expected_revision; path; prefix; limit } in
      Result.map (C.validate ~limits:(Limits.request limits) (to_json t)) ~f:(fun () -> t))
  ;;

  let instance t = t.instance
  let epoch t = t.epoch
  let expected_revision t = t.expected_revision
  let path t = t.path
  let prefix t = t.prefix
  let limit t = t.limit

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.request limits) json in
    let* fields =
      C.fields
        ~allowed:[ "instance"; "epoch"; "expected_revision"; "path"; "prefix"; "limit" ]
        json
    in
    let* instance =
      get fields "instance" (fun json ->
        let* text = C.string ~max_bytes:96 json in
        Wire_id.Instance.of_string text)
    in
    let* epoch =
      get fields "epoch" (fun json ->
        let* text = C.string ~max_bytes:96 json in
        Wire_id.Epoch.of_string text)
    in
    let* expected_revision =
      get fields "expected_revision" (fun json ->
        let* text = C.string ~max_bytes:96 json in
        Wire_id.Revision.of_string text)
    in
    let* path = get fields "path" (C.array ~max_items:16 (C.string ~max_bytes:128)) in
    let* prefix = get fields "prefix" (C.string ~max_bytes:128) in
    let* limit = get fields "limit" C.positive in
    create ~limits ~instance ~epoch ~expected_revision ~path ~prefix ~limit
  ;;
end

module Page = struct
  type status =
    | Complete
    | Limited
    | Unavailable
  [@@deriving equal, sexp_of]

  type t =
    { revision : Wire_id.Revision.t
    ; names : string list
    ; status : status
    }

  let to_json t =
    `Object
      [ "revision", `String (Wire_id.Revision.to_string t.revision)
      ; "names", `Array (List.map t.names ~f:(fun name -> `String name))
      ; ( "status"
        , `String
            (match t.status with
             | Complete -> "complete"
             | Limited -> "limited"
             | Unavailable -> "unavailable") )
      ]
  ;;

  let create ~limits ~revision ~names ~status =
    if
      List.length names > Int.min 16 (Limits.max_page_items limits)
      || List.exists names ~f:(fun name -> not (valid_identifier name))
      || (not
            (List.equal
               String.equal
               names
               (List.dedup_and_sort names ~compare:String.compare)))
      || (equal_status status Unavailable && not (List.is_empty names))
    then C.invalid "invalid REPL completion page"
    else if List.sum (module Int) names ~f:String.length > Limits.max_output_bytes limits
    then C.limit "REPL completion names exceed output limit"
    else (
      let t = { revision; names; status } in
      Result.map (C.validate ~limits:(Limits.result limits) (to_json t)) ~f:(fun () -> t))
  ;;

  let revision t = t.revision
  let names t = t.names
  let status t = t.status

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.result limits) json in
    let* fields = C.fields ~allowed:[ "revision"; "names"; "status" ] json in
    let* revision =
      get fields "revision" (fun json ->
        let* text = C.string ~max_bytes:96 json in
        Wire_id.Revision.of_string text)
    in
    let* names =
      get
        fields
        "names"
        (C.array
           ~max_items:(Int.min 16 (Limits.max_page_items limits))
           (C.string ~max_bytes:128))
    in
    let* status =
      get fields "status" (C.string ~max_bytes:16)
      |> Result.bind ~f:(function
        | "complete" -> Ok Complete
        | "limited" -> Ok Limited
        | "unavailable" -> Ok Unavailable
        | _ -> C.invalid "invalid REPL completion status")
    in
    create ~limits ~revision ~names ~status
  ;;
end
