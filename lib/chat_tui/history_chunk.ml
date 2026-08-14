open Core

module Range = struct
  type t =
    { first : int
    ; past : int
    }
  [@@deriving sexp_of]

  let create_exn ~first ~past =
    if first < 0 || past < first then invalid_arg "History_chunk.Range.create_exn";
    { first; past }
  ;;

  let length t = t.past - t.first
  let is_empty t = Int.equal t.first t.past

  let clamp t ~row_count =
    if row_count < 0 then invalid_arg "History_chunk.Range.clamp";
    { first = Int.min t.first row_count; past = Int.min t.past row_count }
  ;;
end

let canonical_size = 64
let foreground_batch_size = 16

let index_of_row_exn ~block_size ~row_index =
  if row_index < 0 then invalid_arg "History_chunk.index_of_row_exn";
  row_index / block_size
;;

let count ~block_size ~row_count =
  if row_count < 0 then invalid_arg "History_chunk.count";
  if Int.equal row_count 0 then 0 else ((row_count - 1) / block_size) + 1
;;

let block_range ~block_size ~row_count ~block_index =
  if row_count < 0 then invalid_arg "History_chunk.block_range";
  if block_index < 0 || block_index >= count ~block_size ~row_count
  then None
  else (
    let first = block_index * block_size in
    let length = Int.min block_size (row_count - first) in
    Some (Range.create_exn ~first ~past:(first + length)))
;;

let indices_intersecting ~block_size ~row_count range =
  let range = Range.clamp range ~row_count in
  if Range.is_empty range
  then []
  else (
    let first = range.first / block_size in
    let last = (range.past - 1) / block_size in
    List.init (last - first + 1) ~f:(fun offset -> first + offset))
;;

let canonical_index_of_row_exn ~row_index =
  index_of_row_exn ~block_size:canonical_size ~row_index
;;

let foreground_batch_index_of_row_exn ~row_index =
  index_of_row_exn ~block_size:foreground_batch_size ~row_index
;;

let canonical_count ~row_count = count ~block_size:canonical_size ~row_count
let foreground_batch_count ~row_count = count ~block_size:foreground_batch_size ~row_count

let canonical_range ~row_count ~chunk_index =
  block_range ~block_size:canonical_size ~row_count ~block_index:chunk_index
;;

let foreground_batch_range ~row_count ~batch_index =
  block_range ~block_size:foreground_batch_size ~row_count ~block_index:batch_index
;;

let canonical_indices_intersecting ~row_count range =
  indices_intersecting ~block_size:canonical_size ~row_count range
;;

let foreground_batch_indices_intersecting ~row_count range =
  indices_intersecting ~block_size:foreground_batch_size ~row_count range
;;

let expand_to_foreground_batches ~row_count range =
  let range = Range.clamp range ~row_count in
  if Range.is_empty range
  then range
  else (
    let first =
      foreground_batch_index_of_row_exn ~row_index:range.first * foreground_batch_size
    in
    let last = foreground_batch_index_of_row_exn ~row_index:(range.past - 1) in
    let past = Int.min row_count ((last + 1) * foreground_batch_size) in
    Range.create_exn ~first ~past)
;;

let distance_from viewport range =
  if range.Range.past <= viewport.Range.first
  then viewport.first - range.past
  else if viewport.past <= range.first
  then range.first - viewport.past
  else 0
;;

let compare_by_distance ~viewport left right =
  match Int.compare (distance_from viewport left) (distance_from viewport right) with
  | 0 ->
    (match Int.compare left.first right.first with
     | 0 -> Int.compare left.past right.past
     | result -> result)
  | result -> result
;;
