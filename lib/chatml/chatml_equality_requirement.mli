open Core

(** A finite compiler obligation, independent of source type-class syntax.
    [Value] requires ordinary value equality. [Row excluded] checks every field
    except labels already shadowed by an outer row layer. Row exclusions matter
    for type-changing copy-updates: an overwritten original field is not part of
    the value being compared. Sets and obligations are immutable. *)
type t =
  | Value
  | Row of String.Set.t
[@@deriving equal, sexp]

module Conflict : sig
  type t = Value_and_row [@@deriving equal, sexp]
end

(** Conjoin requirements. For rows, a field is exempt only when both uses exempt
    it, so exclusions intersect. A variable used as both a value and a raw row
    is an incompatible internal kind; neither obligation may be discarded. *)
val combine : t -> t -> (t, Conflict.t) result
