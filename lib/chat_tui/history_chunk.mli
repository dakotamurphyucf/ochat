open Core

(** Shared indexing for canonical history chunks and foreground render batches. *)

module Range : sig
  type t = private
    { first : int
    ; past : int
    }
  [@@deriving sexp_of]

  (** [create_exn ~first ~past] creates the half-open row range [first, past).

      @raise Invalid_argument if [first < 0] or [past < first]. *)
  val create_exn : first:int -> past:int -> t

  (** [length t] returns the number of rows in [t]. *)
  val length : t -> int

  (** [is_empty t] returns whether [t] contains no rows. *)
  val is_empty : t -> bool

  (** [clamp t ~row_count] restricts [t] to [0, row_count).

      @raise Invalid_argument if [row_count < 0]. *)
  val clamp : t -> row_count:int -> t
end

(** [canonical_size] is the number of rows in a canonical history chunk. *)
val canonical_size : int

(** [foreground_batch_size] is the maximum number of rows in a foreground
    render batch. *)
val foreground_batch_size : int

(** [canonical_index_of_row_exn ~row_index] returns the canonical chunk
    containing [row_index].

    @raise Invalid_argument if [row_index < 0]. *)
val canonical_index_of_row_exn : row_index:int -> int

(** [foreground_batch_index_of_row_exn ~row_index] returns the foreground
    batch containing [row_index].

    @raise Invalid_argument if [row_index < 0]. *)
val foreground_batch_index_of_row_exn : row_index:int -> int

(** [canonical_count ~row_count] returns the number of canonical chunks needed
    for [row_count] rows.

    @raise Invalid_argument if [row_count < 0]. *)
val canonical_count : row_count:int -> int

(** [foreground_batch_count ~row_count] returns the number of foreground
    batches needed for [row_count] rows.

    @raise Invalid_argument if [row_count < 0]. *)
val foreground_batch_count : row_count:int -> int

(** [canonical_range ~row_count ~chunk_index] returns the clipped row range for
    an existing canonical chunk.

    @raise Invalid_argument if [row_count < 0]. *)
val canonical_range : row_count:int -> chunk_index:int -> Range.t option

(** [foreground_batch_range ~row_count ~batch_index] returns the clipped row
    range for an existing foreground batch.

    @raise Invalid_argument if [row_count < 0]. *)
val foreground_batch_range : row_count:int -> batch_index:int -> Range.t option

(** [canonical_indices_intersecting ~row_count range] returns canonical chunk
    indices intersecting [range] in ascending order.

    @raise Invalid_argument if [row_count < 0]. *)
val canonical_indices_intersecting : row_count:int -> Range.t -> int list

(** [foreground_batch_indices_intersecting ~row_count range] returns
    foreground batch indices intersecting [range] in ascending order.

    @raise Invalid_argument if [row_count < 0]. *)
val foreground_batch_indices_intersecting : row_count:int -> Range.t -> int list

(** [expand_to_foreground_batches ~row_count range] expands a nonempty range
    to clipped 16-row batch boundaries. *)
val expand_to_foreground_batches : row_count:int -> Range.t -> Range.t

(** [compare_by_distance ~viewport left right] orders ranges nearest to
    [viewport] first, breaking equal-distance ties by lower bounds and then
    upper bounds. Intersecting and directly adjacent ranges have distance
    zero. *)
val compare_by_distance : viewport:Range.t -> Range.t -> Range.t -> int
