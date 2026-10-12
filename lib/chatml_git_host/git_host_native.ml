module Native_unix = Unix
open Core

module Code = struct
  type t =
    | Missing
    | Exists
    | Denied
    | Unsupported
    | Corrupt
    | Busy
    | Too_large
    | Unavailable
  [@@deriving equal, sexp_of]

  let of_int = function
    | 1 -> Missing
    | 2 -> Exists
    | 3 -> Denied
    | 5 -> Unsupported
    | 6 -> Corrupt
    | 7 -> Too_large
    | 8 -> Busy
    | _ -> Unavailable
  ;;
end

let unit_result = function
  | 0 -> Ok ()
  | code -> Error (Code.of_int code)
;;

module Descriptor = struct
  type t

  external create : unit -> t = "ochat_git_host_descriptor"
  external close : t -> unit = "ochat_git_host_close"
  external root_raw : t -> Native_unix.file_descr -> bool -> int = "ochat_git_host_root"

  let root t fd = unit_result (root_raw t fd true)
  let trusted_directory t fd = unit_result (root_raw t fd false)

  external validate_root_raw : t -> int = "ochat_git_host_validate_root"

  let validate_root t = unit_result (validate_root_raw t)

  external directory_raw
    :  t
    -> t
    -> string
    -> bool
    -> bool
    -> int
    = "ochat_git_host_directory"

  let directory t ~parent ~name ~create ~exclusive =
    unit_result (directory_raw t parent name create exclusive)
  ;;

  external file_raw : t -> t -> string -> bool -> int = "ochat_git_host_file"

  let file t ~parent ~name ~create = unit_result (file_raw t parent name create)

  external borrowed_fd : t -> Native_unix.file_descr = "ochat_git_host_fd"
  external lock_raw : t -> int = "ochat_git_host_lock"

  let lock t = unit_result (lock_raw t)
end

module Entry = struct
  type kind =
    | Directory
    | Regular
    | Symlink
  [@@deriving equal, sexp_of]

  type t =
    { kind : kind
    ; payload_bytes : int64
    }
end

external entry_raw : Descriptor.t -> string -> int * int * int64 = "ochat_git_host_entry"

let entry t ~name =
  match entry_raw t name with
  | 0, kind, payload_bytes ->
    let kind =
      match kind with
      | 0 -> Entry.Directory
      | 1 -> Regular
      | _ -> Symlink
    in
    Ok { Entry.kind; payload_bytes }
  | code, _, _ -> Error (Code.of_int code)
;;

external read_raw : Descriptor.t -> Bigstring.t -> int -> int = "ochat_git_host_read"
external write_raw : Descriptor.t -> Bigstring.t -> int -> int = "ochat_git_host_write"

let count_result n = if n < 0 then Error (Code.of_int (-n)) else Ok n
let read t buffer ~max_bytes = count_result (read_raw t buffer max_bytes)
let write t buffer ~bytes = count_result (write_raw t buffer bytes)

external sync_raw : Descriptor.t -> int = "ochat_git_host_sync"

let sync t = unit_result (sync_raw t)

external unlink_raw : Descriptor.t -> string -> bool -> int = "ochat_git_host_unlink"

let unlink t ~name ~directory = unit_result (unlink_raw t name directory)

external publish_raw
  :  Descriptor.t
  -> string
  -> Descriptor.t
  -> string
  -> int
  = "ochat_git_host_publish"

let publish t ~name ~destination ~destination_name =
  unit_result (publish_raw t name destination destination_name)
;;

module Cursor = struct
  type t

  external create : unit -> t = "ochat_git_host_cursor"
  external close : t -> unit = "ochat_git_host_close_cursor"
  external open_raw : t -> Descriptor.t -> int = "ochat_git_host_open_cursor"

  let open_ t descriptor = unit_result (open_raw t descriptor)

  external next_raw : t -> Bigstring.t -> int = "ochat_git_host_next"

  let next t buffer =
    match count_result (next_raw t buffer) with
    | Error error -> Error error
    | Ok 0 -> Ok None
    | Ok n -> Ok (Some (Bigstring.to_string buffer ~pos:0 ~len:n))
  ;;
end
