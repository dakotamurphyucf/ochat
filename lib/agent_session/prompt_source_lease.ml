open Core
module Receipt = Agent_store.Prompt_source_receipt
module Owner = Prompt_source_owner
module Selection = Prompt_catalog.Source_selection

module Original = struct
  type t =
    { receipt : Receipt.t
    ; selection : Selection.t option
    ; owner : (Owner.t, Owner.Error.t) Result.t
    ; mutable claimed : bool
    ; mutable closed : bool
    }
end

type ownership =
  | Owned
  | Borrowed

type t =
  | Legacy
  | Original of
      { value : Original.t
      ; ownership : ownership
      }

let legacy = Legacy

let is_legacy = function
  | Legacy -> true
  | Original _ -> false
;;

let create receipt ~selection ~sw ~checkpoint =
  match receipt with
  | None -> Legacy
  | Some receipt ->
    let owner =
      match selection with
      | None -> Error (Owner.Error.missing_live_root ())
      | Some selection -> Selection.lease selection ~expected:receipt ~sw ~checkpoint
    in
    Original
      { value = { receipt; selection; owner; claimed = false; closed = false }
      ; ownership = Owned
      }
;;

let receipt = function
  | Legacy -> None
  | Original { value; _ } -> Some value.receipt
;;

type availability =
  | Missing_receipt
  | Live
  | Unavailable of Owner.Error.t
  | Revoked
  | Closed

let current value =
  if value.Original.closed
  then Error (Owner.Error.missing_live_root ())
  else (
    match value.selection with
    | None -> Ok ()
    | Some selection -> Selection.check_intent selection value.receipt)
;;

let availability = function
  | Legacy -> Missing_receipt
  | Original { value; _ } ->
    if value.closed
    then Closed
    else (
      let observed =
        Result.bind (current value) ~f:(fun () -> Result.bind value.owner ~f:Owner.check)
      in
      match observed with
      | Ok () -> Live
      | Error error ->
        (match Owner.Error.code error with
         | Revoked -> Revoked
         | Closed -> Closed
         | Invalid_binding
         | Foreign_receipt
         | Missing_live_root
         | Busy
         | Lease_limit
         | Filesystem
         | Capture
         | Foreign_capture
         | Project_preparation
         | Document_capture -> Unavailable error))
;;

let borrow = function
  | Legacy -> Legacy
  | Original { value; _ } -> Original { value; ownership = Borrowed }
;;

let claim = function
  | Legacy | Original { ownership = Borrowed; _ } -> Ok ()
  | Original { value; ownership = Owned } ->
    if value.claimed || value.closed
    then Or_error.error_string "original source loan already attached or closed"
    else (
      value.claimed <- true;
      Ok ())
;;

let close = function
  | Legacy | Original { ownership = Borrowed; _ } -> ()
  | Original { value; ownership = Owned } ->
    if not value.closed
    then (
      value.closed <- true;
      Result.iter value.owner ~f:Owner.close)
;;

let with_scope t ~f =
  match t with
  | Legacy -> Error (Owner.Error.foreign_receipt ())
  | Original { value; _ } ->
    Result.bind (current value) ~f:(fun () ->
      Result.bind value.owner ~f:(fun owner -> Owner.with_scope owner ~f))
;;
