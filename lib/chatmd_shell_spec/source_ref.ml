open! Core
open Jsonaf.Export

type position =
  { offset : int
  ; line : int
  ; column : int
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { file : string
  ; source_dir : string
  ; prompt_dir : string
  ; namespace : string option
  ; start_pos : position
  ; end_pos : position
  ; source_sha256 : string
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let digest source = Digestif.SHA256.(to_hex (digest_string source))

let create ~file ~source_dir ~prompt_dir ~namespace ~start_pos ~end_pos ~source =
  { file
  ; source_dir
  ; prompt_dir
  ; namespace
  ; start_pos
  ; end_pos
  ; source_sha256 = digest source
  }
;;

let qualify t id =
  if String.mem id ':'
  then id
  else Option.value_map t.namespace ~default:id ~f:(fun namespace -> namespace ^ ":" ^ id)
;;
