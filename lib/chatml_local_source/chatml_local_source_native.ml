module Native_unix = Unix
open Core

module Code = struct
  type t =
    | Missing
    | Denied
    | Unsupported
    | Kind
    | Symlink_or_non_directory
    | Changed
    | Too_large
    | Unavailable
  [@@deriving equal, sexp_of]

  let of_int = function
    | 0 -> Missing
    | 1 -> Denied
    | 2 -> Unsupported
    | 3 -> Kind
    | 4 -> Symlink_or_non_directory
    | 5 -> Changed
    | 6 -> Too_large
    | _ -> Unavailable
  ;;
end

module Descriptor = struct
  type t

  external create : unit -> t = "ochat_local_source_create_descriptor"

  external duplicate_directory
    :  t
    -> Native_unix.file_descr
    -> (unit, Code.t) Result.t
    = "ochat_local_source_duplicate_directory"

  external clone_directory
    :  t
    -> parent:t
    -> (unit, Code.t) Result.t
    = "ochat_local_source_clone_directory"

  external open_directory
    :  t
    -> parent:t
    -> string
    -> (unit, Code.t) Result.t
    = "ochat_local_source_open_directory"

  external open_file
    :  t
    -> parent:t
    -> string
    -> (unit, Code.t) Result.t
    = "ochat_local_source_open_file"

  external close : t -> unit = "ochat_local_source_close_descriptor"
end

module Observation = struct
  type t = int64 array

  let byte_length t = t.(2)
end

external observe_file
  :  Descriptor.t
  -> (Observation.t, Code.t) Result.t
  = "ochat_local_source_observe_file"

let unchanged descriptor before =
  match observe_file descriptor with
  | Error error -> Error error
  | Ok after -> if Array.equal Int64.equal before after then Ok () else Error Code.Changed
;;

external same_entry
  :  Descriptor.t
  -> name:string
  -> file:Descriptor.t
  -> (unit, Code.t) Result.t
  = "ochat_local_source_same_entry"

external read_raw : Descriptor.t -> Bigstring.t -> int -> int = "ochat_local_source_read"

let read descriptor buffer ~max_bytes =
  let count = read_raw descriptor buffer max_bytes in
  if count < 0 then Error (Code.of_int (-count - 1)) else Ok count
;;

module Directory = struct
  type t

  external create : unit -> t = "ochat_local_source_create_stream"

  external open_
    :  t
    -> Descriptor.t
    -> (unit, Code.t) Result.t
    = "ochat_local_source_open_stream"

  external close : t -> unit = "ochat_local_source_close_stream"
  external next_raw : t -> Bigstring.t -> int = "ochat_local_source_next_entry"

  let next t buffer =
    let count = next_raw t buffer in
    if count < 0
    then Error (Code.of_int (-count - 1))
    else if count = 0
    then Ok None
    else Ok (Some (Bigstring.to_string buffer ~pos:0 ~len:count))
  ;;
end

module Directory_identity = struct
  type t =
    { device : int64
    ; inode : int64
    }
  [@@deriving equal, compare, sexp_of]
end

external observe_directory
  :  Descriptor.t
  -> (Directory_identity.t, Code.t) Result.t
  = "ochat_local_source_observe_directory"
