(** An admitted readable text part. Its index is its original position in one
    history item, including any intervening hidden or nontext parts. The
    caller owns item visibility, revision selection and aggregate budgets. *)
type t = private
  { index : int
  ; text : string
  }

val max_parts : int
val max_part_bytes : int
val default_slice_bytes : int
val max_slice_bytes : int

(** Admit up to 1 MiB of UTF-8 at an original index in [0, 4096). *)
val create : index:int -> text:string -> (t, Error.t) result

module Slice : sig
  type t = private
    { text : string
    ; next_offset : int option
    }
end

(** Read the half-open byte range in this part. Both endpoints must be scalar
    boundaries. The default reply budget is 4 KiB and its maximum is 16 KiB.
    A continuation always advances. A budget too small for the first scalar
    returns a resource-limit error. Empty ranges at valid boundaries succeed. *)
val read : ?max_bytes:int -> t -> start:int -> stop:int -> (Slice.t, Error.t) result

module Literal : sig
  type part := t
  type t

  val create : Search_term.t -> t

  module Match : sig
    type t = private
      { start : int
      ; stop : int
      ; snippet : Search_snippet.t
      }
  end

  (** First ASCII-insensitive literal match. Coordinates are half-open byte
      offsets in the original part. The snippet covers the entire match in
      at most 512 UTF-8 bytes. No concatenation, folding of Unicode or normalization. *)
  val find : t -> part -> (Match.t option, Error.t) result
end
