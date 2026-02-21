(** Token-bounded slicing for *generic* Markdown files (README, docs, etc.).

    The implementation is largely copied from {!Odoc_snippet} with
    minimal changes:

    • We split on thematic breaks ([---], [***], or [___]) in addition
      to headings, blank lines, code fences, and tables.
    • The metadata record uses an [index] field instead of [pkg].

    The public API mirrors the [Odoc] counterpart so that callers can
    reuse the same plumbing (load BPE file once, pass it in).  See the
    [.mli] for full contract. *)

open! Core

(**************************************************************************)
(* Metadata                                                                *)
(**************************************************************************)

module Meta = struct
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

(**************************************************************************)
(* Block chunking                                                          *)
(**************************************************************************)

module Chunker = struct
  let is_heading line =
    let trimmed = String.lstrip line in
    String.is_prefix trimmed ~prefix:"# "
    || String.is_prefix trimmed ~prefix:"## "
    || String.is_prefix trimmed ~prefix:"### "
  ;;

  let is_thematic_break line =
    let trimmed = String.strip line in
    if String.length trimmed < 3
    then false
    else (
      let rec loop i =
        if i >= String.length trimmed
        then true
        else (
          match trimmed.[i] with
          | '-' | '*' | '_' -> loop (i + 1)
          | _ -> false)
      in
      loop 0)
  ;;

  let chunk_by_heading_or_blank (lines : string list) : string list =
    let flush buf acc =
      match buf with
      | [] -> acc
      | _ -> String.concat ~sep:"\n" (List.rev buf) :: acc
    in
    let rec loop acc buf in_code = function
      | [] -> List.rev (flush buf acc)
      | line :: rest ->
        let opening_fence = String.is_prefix (String.strip line) ~prefix:"```" in
        (match in_code, opening_fence with
         | true, true ->
           (* close code block *)
           loop (flush (line :: buf) acc) [] false rest
         | false, true ->
           (* open code block *)
           loop acc (line :: buf) true rest
         | _ ->
           let blank = String.equal (String.strip line) "" in
           let thematic = (not in_code) && is_thematic_break line in
           let is_table_row =
             (not in_code) && String.is_prefix (String.strip line) ~prefix:"|"
           in
           if is_table_row
           then (
             let rec gather rows rem =
               match rem with
               | next :: tl when String.is_prefix (String.strip next) ~prefix:"|" ->
                 gather (next :: rows) tl
               | tl -> List.rev rows, tl
             in
             let table_rows, rest' = gather [ line ] rest in
             let acc = flush buf acc in
             let buf' = [ String.concat ~sep:"\n" table_rows ] in
             loop acc buf' false rest')
           else if (not in_code) && (blank || thematic || is_heading line)
           then (
             let acc = flush buf acc in
             let buf' = if blank || thematic then [] else [ line ] in
             loop acc buf' false rest)
           else loop acc (line :: buf) in_code rest)
    in
    loop [] [] false lines
  ;;
end

(**************************************************************************)
(* Token counting with LRU cache                                          *)
(**************************************************************************)

module Str_key = struct
  type t = string [@@deriving hash, sexp, compare]

  let invariant _ = ()
end

module Cache = Lru_cache.Make (Str_key)

let tok_cache : int Cache.t = Cache.create ~max_size:5000 ()
let cache_mu = Eio.Mutex.create ()

let heuristic_token_count text =
  String.split_on_chars text ~on:[ ' '; '\n'; '\t'; '\r' ]
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.length
;;

let chunk_text_by_token_limit ~codec ~limit text =
  if limit <= 0 then invalid_arg "chunk_text_by_token_limit: limit must be > 0";
  let tokens_list = Tikitoken.encode ~codec ~text in
  let tokens = Array.of_list tokens_list in
  let n = Array.length tokens in
  if n <= limit
  then [ text, n ]
  else (
    let rec loop acc i =
      if i >= n
      then List.rev acc
      else (
        let len = Int.min limit (n - i) in
        let tok_slice = Array.sub tokens ~pos:i ~len |> Array.to_list in
        let bytes = Tikitoken.decode ~codec ~encoded:tok_slice in
        let chunk = Bytes.to_string bytes, len in
        loop (chunk :: acc) (i + len))
    in
    loop [] 0)
;;

let token_count ~codec text =
  Eio.Mutex.lock cache_mu;
  let cached = Cache.find tok_cache text in
  match cached with
  | Some n ->
    Eio.Mutex.unlock cache_mu;
    n
  | None ->
    Eio.Mutex.unlock cache_mu;
    let n =
      match codec with
      | Some c ->
        (try List.length (Tikitoken.encode ~codec:c ~text) with
         | _ -> heuristic_token_count text)
      | None -> heuristic_token_count text
    in
    if String.length text < 10_000
    then (
      Eio.Mutex.lock cache_mu;
      Cache.set tok_cache ~key:text ~data:n;
      Eio.Mutex.unlock cache_mu);
    n
;;

(**************************************************************************)
(* Title helper                                                            *)
(**************************************************************************)

let find_title lines =
  List.find_map lines ~f:(fun line ->
    let trimmed = String.strip line in
    if String.is_prefix trimmed ~prefix:"# "
    then Some (String.strip (String.drop_prefix trimmed 2))
    else if String.is_prefix trimmed ~prefix:"## "
    then Some (String.strip (String.drop_prefix trimmed 3))
    else None)
;;

(**************************************************************************)
(* Slice main                                                              *)
(**************************************************************************)

let slice ~index_name ~doc_path ~markdown ~tiki_token_bpe () : (Meta.t * string) list =
  let codec = Tikitoken.create_codec tiki_token_bpe in
  let min_tokens = 64 in
  let max_tokens = 800 in
  let overlap_tokens = 64 in
  let block_strings =
    markdown |> String.split_lines |> Chunker.chunk_by_heading_or_blank
  in
  let blocks =
    List.map block_strings ~f:(fun b ->
      let len =
        (* heuristic to optimize use of token counting. we are limited to ~8400 tokens per vector so ~32000 characters. If
        the text is way shorter than that we can use the the aproximate method of counting tokens.larges ones we need to make sure
        we are accuarte and dont go over the limit and cause errors*)
        if String.length b < 20000
        then String.length b / 4
        else token_count ~codec:(Some codec) b
      in
      match len > max_tokens with
      | true -> chunk_text_by_token_limit ~codec ~limit:max_tokens b
      | false -> [ b, token_count ~codec:(Some codec) b ])
    |> List.concat
  in
  let title = find_title block_strings in
  let lower_path = String.lowercase doc_path in
  let is_readme =
    (String.is_suffix lower_path ~suffix:".md"
     || String.is_suffix lower_path ~suffix:".markdown")
    && String.is_substring lower_path ~substring:"readme"
  in
  let make_header line_start line_end =
    if is_readme
    then
      Printf.sprintf
        "(** Package:%s Doc:README Lines:%d-%d *)\n\n"
        index_name
        line_start
        line_end
    else
      Printf.sprintf
        "(** Package:%s Doc:%s Lines:%d-%d *)\n\n"
        index_name
        doc_path
        line_start
        line_end
  in
  let flush (blocks_rev : string list) start_idx end_idx acc =
    let body = String.concat ~sep:"\n" (List.rev blocks_rev) in
    let header = make_header start_idx end_idx in
    let text = header ^ body in
    let id = Doc.hash_string_md5 text in
    let meta =
      Meta.
        { id
        ; index = index_name
        ; doc_path
        ; title
        ; line_start = start_idx
        ; line_end = end_idx
        }
    in
    (meta, text) :: acc
  in
  let rec take_overlap rev_blocks token_budget acc_tokens acc_blocks =
    match rev_blocks with
    | [] -> acc_blocks
    | ((_, tok) as blk) :: rest ->
      if acc_tokens + tok >= token_budget
      then acc_blocks
      else take_overlap rest token_budget (acc_tokens + tok) (blk :: acc_blocks)
  in
  let rec loop idx blocks_rev tok_acc start_idx acc remaining =
    match remaining with
    | [] ->
      if tok_acc = 0
      then List.rev acc
      else List.rev (flush (List.map blocks_rev ~f:fst) start_idx (idx - 1) acc)
    | (blk_str, blk_tok) :: rest ->
      if tok_acc + blk_tok > max_tokens && tok_acc >= min_tokens
      then (
        let acc' = flush (List.map blocks_rev ~f:fst) start_idx (idx - 1) acc in
        let overlap_blocks = take_overlap blocks_rev overlap_tokens 0 [] in
        let overlap_rev = List.rev overlap_blocks in
        let overlap_tokens_cnt =
          List.fold overlap_blocks ~init:0 ~f:(fun n (_, t) -> n + t)
        in
        loop
          (idx + 1)
          overlap_rev
          overlap_tokens_cnt
          (idx - List.length overlap_blocks)
          acc'
          ((blk_str, blk_tok) :: rest))
      else
        loop
          (idx + 1)
          ((blk_str, blk_tok) :: blocks_rev)
          (tok_acc + blk_tok)
          start_idx
          acc
          rest
  in
  loop 1 [] 0 1 [] blocks
;;
