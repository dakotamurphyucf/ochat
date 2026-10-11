open Core
module P = Agent_protocol
module H = P.History
module G = P.Authoring_guidance
module Presence = Authoring_presence
module J = P.Json_codec

type limits =
  { max_receipts : int
  ; max_bytes : int
  }

type t =
  { scope : string
  ; receipts : Presence.receipt list
  ; truncated : bool
  }

let default_limits = { max_receipts = 64; max_bytes = 64 * 1024 }
let invalid message = Error (P.Error.invalid_request message)
let scope t = t.scope
let receipts t = t.receipts
let truncated t = t.truncated

let receipt_json (receipt : Presence.receipt) =
  `Object
    [ "entry_id", H.Id.to_json receipt.entry_id; "guidance", G.to_json receipt.guidance ]
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; "scope", `String t.scope
    ; ("truncated", if t.truncated then `True else `False)
    ; "receipts", `Array (List.map t.receipts ~f:receipt_json)
    ]
;;

let encoded_bytes t = String.length (Jsonaf.to_string (to_json t))

(* Validate bounded traversal and directly constructed JSON before recursive
   public codecs or serialization. Encoded-size validation remains with J. *)
let validate_json limits json =
  let open Result.Let_syntax in
  let remaining = ref limits.max_bytes in
  let charge bytes =
    if bytes > !remaining
    then invalid "extension JSON exceeds byte limit"
    else (
      remaining := !remaining - bytes;
      Ok ())
  in
  let rec visit depth json =
    if depth > 16
    then invalid "extension JSON exceeds depth limit"
    else (
      let%bind () = charge 1 in
      match json with
      | `String value ->
        let%bind () = charge (String.length value) in
        charge 2
      | `Number value ->
        let%bind () = charge (String.length value) in
        (match Float.of_string_opt value with
         | Some number when Float.is_finite number ->
           (match Jsonaf.parse value with
            | Ok (`Number _) -> Ok ()
            | Ok _ | Error _ -> invalid "invalid JSON number")
         | _ -> invalid "extension JSON requires finite numbers")
      | `Array values ->
        List.fold_result values ~init:() ~f:(fun () value -> visit (depth + 1) value)
      | `Object values ->
        let%bind () =
          List.fold_result values ~init:() ~f:(fun () (name, value) ->
            let%bind () = charge (String.length name) in
            let%bind () = charge 3 in
            visit (depth + 1) value)
        in
        let%map _ = J.fields json in
        ()
      | `Null | `True | `False -> Ok ())
  in
  let%bind () = visit 1 json in
  J.validate_limits ~max_bytes:limits.max_bytes ~max_depth:16 json
;;

let closed fields allowed =
  match
    List.find (J.to_alist fields) ~f:(fun (key, _) ->
      not (List.mem allowed key ~equal:String.equal))
  with
  | None -> Ok ()
  | Some (key, _) -> invalid ("unknown extension field: " ^ key)
;;

let validate_limits limits scope =
  match limits.max_receipts >= 0 && limits.max_bytes > 0 with
  | false -> invalid "invalid authoring reference index limits"
  | true ->
    if String.is_empty scope || String.length scope > 1024
    then invalid "authoring reference scope must be nonempty and within its byte limit"
    else Ok ()
;;

let empty ?(limits = default_limits) ~scope () =
  let open Result.Let_syntax in
  let%bind () = validate_limits limits scope in
  let t = { scope; receipts = []; truncated = false } in
  match encoded_bytes t <= limits.max_bytes with
  | true -> Ok t
  | false -> invalid "authoring reference index envelope exceeds byte limit"
;;

let remember ?(limits = default_limits) t ~history =
  let open Result.Let_syntax in
  let%bind _ = empty ~limits ~scope:t.scope () in
  (* Validate identity conflicts even when one conflicting entry would later be
     evicted. Neither bounds nor payload filtering can launder a changed ID. *)
  let%bind _ = Presence.remember ~previous:t.receipts ~history in
  let redacted =
    List.filter_map history ~f:(fun entry -> Option.some_if entry.H.redacted entry.id)
    |> Hash_set.of_list (module H.Id)
  in
  let observed =
    List.filter_map history ~f:(fun entry ->
      match entry.H.provenance with
      | Runtime_authoring guidance
        when (not entry.redacted)
             && (not (G.equal_purpose guidance.purpose Rediscovery))
             && G.matches_payload guidance entry.payload ->
        Some Presence.{ entry_id = entry.id; guidance }
      | _ -> None)
  in
  let seen = Hash_set.create (module H.Id) in
  let recent =
    (* The observed batch is newer than the retained index. *)
    List.rev_append observed (List.rev t.receipts)
    |> List.filter ~f:(fun receipt ->
      let id = receipt.Presence.entry_id in
      match Hash_set.mem seen id || Hash_set.mem redacted id with
      | true -> false
      | false ->
        Hash_set.add seen id;
        true)
  in
  let base = encoded_bytes { t with receipts = [] } in
  let _, _, retained, dropped =
    List.fold
      recent
      ~init:(0, base, [], false)
      ~f:(fun (count, used, retained, dropped) receipt ->
        let size = String.length (Jsonaf.to_string (receipt_json receipt)) in
        let separator = Bool.to_int (count > 0) in
        match
          count < limits.max_receipts && size <= limits.max_bytes - used - separator
        with
        | true -> count + 1, used + size + separator, receipt :: retained, dropped
        | false -> count, used, retained, true)
  in
  (* Changing false to true shortens the JSON boolean by one byte, so using the
     previous envelope size above remains conservative when truncation begins. *)
  Ok { t with receipts = retained; truncated = t.truncated || dropped }
;;

let forget t ids =
  let removed = Hash_set.of_list (module H.Id) ids in
  { t with
    receipts =
      List.filter t.receipts ~f:(fun receipt ->
        not (Hash_set.mem removed receipt.entry_id))
  }
;;

let of_json ?(limits = default_limits) ~scope json =
  let open Result.Let_syntax in
  let%bind _ = empty ~limits ~scope () in
  let%bind () = validate_json limits json in
  let%bind fields = J.fields json in
  let%bind () = closed fields [ "version"; "scope"; "truncated"; "receipts" ] in
  let%bind _ = J.required_as fields "version" (J.bounded_int ~min:1 ~max:1) in
  let%bind restored_scope = J.required_as fields "scope" J.string in
  let%bind () =
    match String.equal scope restored_scope with
    | true -> Ok ()
    | false -> invalid "authoring reference index belongs to a different scope"
  in
  let%bind truncated = J.required_as fields "truncated" J.bool in
  let%bind values =
    J.required_as fields "receipts" (function
      | `Array values when List.length values <= limits.max_receipts -> Ok values
      | _ -> invalid "authoring reference index exceeds receipt limit")
  in
  let%bind receipts =
    List.map values ~f:(fun json ->
      let%bind fields = J.fields json in
      let%bind () = closed fields [ "entry_id"; "guidance" ] in
      let%bind entry_id = J.required_as fields "entry_id" H.Id.of_json in
      let%map guidance = J.required_as fields "guidance" G.of_json in
      Presence.{ entry_id; guidance })
    |> Result.all
  in
  let%bind () =
    match
      List.find_a_dup receipts ~compare:(fun a b -> H.Id.compare a.entry_id b.entry_id)
    with
    | None -> Ok ()
    | Some _ -> invalid "duplicate authoring reference index identity"
  in
  Ok { scope; receipts; truncated }
;;
