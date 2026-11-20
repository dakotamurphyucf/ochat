open! Core
module P = Agent_protocol
module D = Document_schema

let invalid message = P.Error.invalid_request message
let max_note_bytes = 64 * 1024
let max_catalog_bytes = 2 * 1024 * 1024
let max_notes = 128
let max_receipts = 512

module Note_path = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let valid_component component =
    let length = String.length component in
    length > 0
    && length <= 63
    && (not (String.equal component "." || String.equal component ".."))
    && String.for_all component ~f:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' -> true
      | _ -> false)
  ;;

  let of_string path =
    if String.length path > 240
    then Error (invalid "recovery note path exceeds 240 bytes")
    else (
      let components = String.split path ~on:'/' in
      if List.length components <= 8 && List.for_all components ~f:valid_component
      then Ok path
      else Error (invalid "invalid recovery note path component"))
  ;;

  let to_string t = t
end

module Note_id = struct
  type t = int64 [@@deriving compare, equal, sexp_of]

  let of_int64 value =
    if Int64.(value > 0L) then Ok value else Error (invalid "invalid recovery note ID")
  ;;

  let to_int64 t = t
end

module Note_revision = struct
  type t = int64 [@@deriving compare, equal, sexp_of]

  let of_int64 value =
    if Int64.(value > 0L)
    then Ok value
    else Error (invalid "invalid recovery note revision")
  ;;

  let to_int64 t = t
end

module Catalog_revision = struct
  type t = int64 [@@deriving compare, equal, sexp_of]

  let of_int64 value =
    if Int64.(value >= 0L)
    then Ok value
    else Error (invalid "invalid recovery note catalog revision")
  ;;

  let to_int64 t = t
end

module Content = struct
  type t = string [@@deriving equal]

  let of_string text =
    if String.length text > max_note_bytes
    then Error (invalid "recovery note exceeds 64 KiB")
    else if (not (Stdlib.String.is_valid_utf_8 text)) || String.mem text '\000'
    then Error (invalid "recovery note requires valid UTF-8 without NUL")
    else Ok text
  ;;

  let to_string t = t
end

type selection =
  { id : Note_id.t
  ; revision : Note_revision.t
  }
[@@deriving equal, sexp_of]

type metadata =
  { selection : selection
  ; path : Note_path.t
  ; byte_length : int
  }
[@@deriving sexp_of]

module Mutation_identity = struct
  module T = struct
    type t =
      | Client of
          { principal : P.Id.Principal.t
          ; key : P.Idempotency_key.t
          }
      | Native of P.Id.Invocation.t
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type edit =
  { expected : selection
  ; content : Content.t
  }

type mutation =
  | Create of
      { expected_catalog_revision : Catalog_revision.t
      ; path : Note_path.t
      ; content : Content.t
      }
  | Replace of edit
  | Append of edit
  | Delete of selection

module Receipt = struct
  type kind =
    | Created
    | Replaced
    | Appended
    | Deleted
  [@@deriving equal, sexp_of]

  type t =
    { kind : kind
    ; selection : selection
    ; catalog_revision : Catalog_revision.t
    }
  [@@deriving equal, sexp_of]
end

type read_error =
  | Note_not_found
  | Stale_note_revision of Note_revision.t
[@@deriving equal, sexp_of]

type mutation_error =
  | Note_precondition of read_error
  | Stale_catalog_revision of Catalog_revision.t
  | Path_already_exists
  | Empty_append
  | Note_byte_limit
  | Catalog_byte_limit
  | Catalog_entry_limit
  | Revision_exhausted
  | Idempotency_conflict
  | Invalid_identity
  | Document_error of D.Error.t
[@@deriving equal, sexp_of]

type note =
  { selection : selection
  ; path : Note_path.t
  ; content : Content.t
  ; changed_at : Catalog_revision.t
  }

type receipt_record =
  { identity : Mutation_identity.t
  ; request_digest : string
  ; receipt : Receipt.t
  }

type state =
  { owner : P.Session_ref.t
  ; revision : Catalog_revision.t
  ; notes : note list
  ; receipts : receipt_record list
  }

type t = state D.Extension_carrier.t

type outcome =
  | Applied of t * Receipt.t
  | Replayed of Receipt.t

let text_bytes notes =
  List.sum (module Int) notes ~f:(fun note -> String.length note.content)
;;
