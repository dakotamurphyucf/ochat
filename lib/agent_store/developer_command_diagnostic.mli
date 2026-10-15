open! Core

(** Bounded scalar diagnostic projection. Comparison coordinates never grant
    source access; the enclosing receipt validates the complete JSON tree under
    its shared charged poll and aggregate document limits. *)
module Location : sig
  type t =
    { source : Chatml_source.Id.t
    ; digest : Chatml_source.Digest.t
    ; source_bytes : int
    ; start_byte : int
    ; end_byte : int
    }
  [@@deriving sexp_of]
end

type t =
  { code : string
  ; message : string
  ; location : Location.t option
  }
[@@deriving sexp_of]

(** Finite scalar/range validation; code 1..256 bytes, message at most 16 KiB.
    This is not whole-receipt JSON admission. *)
val validate : t -> (unit, Document_schema.Error.t) result

val to_json : t -> Jsonaf.t
