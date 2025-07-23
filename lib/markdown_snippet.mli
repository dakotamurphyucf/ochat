open! Core

module Meta : sig
  type t =
    { id : string
    ; index : string
    ; doc_path : string
    ; title : string option
    ; line_start : int
    ; line_end : int
    }
  [@@deriving sexp, bin_io, compare, hash]
end

val slice
  :  index_name:string
  -> doc_path:string
  -> markdown:string
  -> tiki_token_bpe:string
  -> unit
  -> (Meta.t * string) list
