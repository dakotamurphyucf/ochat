open Core

(* ---------------- Rolling hash over a Bytes.t (mod 2^64 via Int64 wrap) ---------------- *)

module Rolling_hash = struct
  let base : int64 = 1315423911L
  let add = Int64.( + )
  let mul = Int64.( * )
  let sub = Int64.( - )

  type t =
    { pref : int64 array (* length n+1, for the slice *)
    ; power : int64 array (* length n+1 *)
    }

  let of_slice (buf : Bytes.t) ~(off : int) ~(len : int) : t =
    let pref = Array.create ~len:(len + 1) 0L in
    let power = Array.create ~len:(len + 1) 0L in
    power.(0) <- 1L;
    for i = 0 to len - 1 do
      let x = Char.to_int (Bytes.get buf (off + i)) + 1 in
      pref.(i + 1) <- add (mul pref.(i) base) (Int64.of_int x);
      power.(i + 1) <- mul power.(i) base
    done;
    { pref; power }
  ;;

  let range_hash (t : t) ~(start : int) ~(len : int) : int64 =
    sub t.pref.(start + len) (mul t.pref.(start) t.power.(len))
  ;;
end

let bytes_equal_slice (buf : Bytes.t) ~(base : int) ~(start : int) (tok : Bytes.t) : bool =
  let len = Bytes.length tok in
  let rec loop i =
    if i = len
    then true
    else if Char.(Bytes.get buf (base + start + i) <> Bytes.get tok i)
    then false
    else loop (i + 1)
  in
  loop 0
;;

(* ---------------- Vocab hash index: hash -> candidates ---------------- *)

type vocab_index = (int64, (Bytes.t * int) list) Hashtbl.t

let hash_full_bytes (b : Bytes.t) : int64 =
  let add = Int64.( + ) in
  let mul = Int64.( * ) in
  let h = ref 0L in
  for i = 0 to Bytes.length b - 1 do
    let x = Char.to_int (Bytes.get b i) + 1 in
    h := Int64.(add (mul !h Rolling_hash.base) (of_int x))
  done;
  !h
;;

let build_vocab_index (encoder : (Bytes.t, int) Hashtbl.t) : vocab_index =
  let idx = Hashtbl.create (module Int64) in
  Hashtbl.iteri encoder ~f:(fun ~key:tok ~data:token_id ->
    let h = hash_full_bytes tok in
    Hashtbl.add_multi idx ~key:h ~data:(tok, token_id));
  idx
;;

let lookup_token_id
      (idx : vocab_index)
      (rh : Rolling_hash.t)
      (buf : Bytes.t)
      ~(base : int)
      ~(start : int)
      ~(len : int)
  : int option
  =
  let h = Rolling_hash.range_hash rh ~start ~len in
  match Hashtbl.find idx h with
  | None -> None
  | Some candidates ->
    List.find_map candidates ~f:(fun (tok, token_id) ->
      if Bytes.length tok = len && bytes_equal_slice buf ~base ~start tok
      then Some token_id
      else None)
;;

(* ---------------- Fast BPE merge using a min-heap of adjacent pairs ---------------- *)

module Pair = struct
  type t =
    { rank : int
    ; left : int
    ; right : int
    }

  let compare a b =
    let c = Int.compare a.rank b.rank in
    if c <> 0
    then c
    else (
      (* tie-break to stabilize heap behavior *)
      let c2 = Int.compare a.left b.left in
      if c2 <> 0 then c2 else Int.compare a.right b.right)
  ;;
end

module Min_heap (X : sig
    type t

    val compare : t -> t -> int
  end) =
struct
  type t =
    { mutable a : X.t array
    ; mutable size : int
    ; dummy : X.t
    }

  let create ?(capacity = 64) ~dummy () =
    { a = Array.create ~len:capacity dummy; size = 0; dummy }
  ;;

  let is_empty h = h.size = 0
  let parent i = (i - 1) / 2
  let left i = (2 * i) + 1
  let right i = (2 * i) + 2

  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  ;;

  let ensure_capacity h =
    if h.size >= Array.length h.a
    then (
      let a' = Array.create ~len:(2 * Array.length h.a) h.dummy in
      Array.blit ~src:h.a ~src_pos:0 ~dst:a' ~dst_pos:0 ~len:h.size;
      h.a <- a')
  ;;

  let sift_up h i0 =
    let rec loop i =
      if i = 0
      then ()
      else (
        let p = parent i in
        if X.compare h.a.(i) h.a.(p) < 0
        then (
          swap h.a i p;
          loop p)
        else ())
    in
    loop i0
  ;;

  let sift_down h i0 =
    let rec loop i =
      let l = left i in
      if l >= h.size
      then ()
      else (
        let r = right i in
        let smallest = if r < h.size && X.compare h.a.(r) h.a.(l) < 0 then r else l in
        if X.compare h.a.(smallest) h.a.(i) < 0
        then (
          swap h.a smallest i;
          loop smallest)
        else ())
    in
    loop i0
  ;;

  let add h x =
    ensure_capacity h;
    h.a.(h.size) <- x;
    sift_up h h.size;
    h.size <- h.size + 1
  ;;

  let pop h =
    if h.size = 0
    then None
    else (
      let x = h.a.(0) in
      h.size <- h.size - 1;
      if h.size > 0
      then (
        h.a.(0) <- h.a.(h.size);
        sift_down h 0);
      Some x)
  ;;
end

module Heap = Min_heap (Pair)

let byte_pair_encode_fast_slice
      ~(vocab_idx : vocab_index)
      (buf : Bytes.t)
      ~(off : int)
      ~(len : int)
  : int list
  =
  if len = 0
  then []
  else (
    let rh = Rolling_hash.of_slice buf ~off ~len in
    (* Fast path: whole piece is a token *)
    match lookup_token_id vocab_idx rh buf ~base:off ~start:0 ~len with
    | Some tok -> [ tok ]
    | None ->
      let n = len in
      let max_nodes = (2 * n) + 1 in
      let start_pos = Array.create ~len:max_nodes 0 in
      let end_pos = Array.create ~len:max_nodes 0 in
      let prev = Array.create ~len:max_nodes (-1) in
      let next = Array.create ~len:max_nodes (-1) in
      let alive = Array.create ~len:max_nodes false in
      for i = 0 to n - 1 do
        start_pos.(i) <- i;
        end_pos.(i) <- i + 1;
        prev.(i) <- i - 1;
        next.(i) <- (if i = n - 1 then -1 else i + 1);
        alive.(i) <- true
      done;
      let head = ref 0 in
      let next_free = ref n in
      let dummy = { Pair.rank = Int.max_value; left = 0; right = 0 } in
      let heap = Heap.create ~dummy ~capacity:(max 64 (n * 2)) () in
      let push_pair left =
        let right = next.(left) in
        if right <> -1 && alive.(left) && alive.(right)
        then (
          let s = start_pos.(left) in
          let e = end_pos.(right) in
          let l = e - s in
          match lookup_token_id vocab_idx rh buf ~base:off ~start:s ~len:l with
          | None -> ()
          | Some rank -> Heap.add heap { Pair.rank; left; right })
      in
      for i = 0 to n - 2 do
        push_pair i
      done;
      let rec merge_loop () =
        match Heap.pop heap with
        | None -> ()
        | Some { Pair.rank = _; left; right } ->
          if not (alive.(left) && alive.(right) && next.(left) = right)
          then merge_loop ()
          else (
            let k = !next_free in
            incr next_free;
            let p = prev.(left) in
            let q = next.(right) in
            start_pos.(k) <- start_pos.(left);
            end_pos.(k) <- end_pos.(right);
            prev.(k) <- p;
            next.(k) <- q;
            alive.(k) <- true;
            alive.(left) <- false;
            alive.(right) <- false;
            if p <> -1 then next.(p) <- k else head := k;
            if q <> -1 then prev.(q) <- k;
            if p <> -1 then push_pair p;
            push_pair k;
            merge_loop ())
      in
      merge_loop ();
      let rec emit acc i =
        if i = -1
        then List.rev acc
        else (
          let s = start_pos.(i) in
          let l = end_pos.(i) - s in
          match lookup_token_id vocab_idx rh buf ~base:off ~start:s ~len:l with
          | Some token_id -> emit (token_id :: acc) next.(i)
          | None ->
            failwith
              (Printf.sprintf
                 "final segment not in vocab: off=%d start=%d len=%d"
                 off
                 s
                 l))
      in
      emit [] !head)
;;

type codec =
  { encoder : (bytes, int) Hashtbl.t
  ; decoder : (int, bytes) Hashtbl.t
  ; vocab_idx : vocab_index
  }

let encode_ordinary (regex : Pcre.regexp) (codec : codec) (text : string) : int list =
  let buf = Bytes.unsafe_of_string_promise_no_mutation text in
  let mats = Pcre.exec_all ~rex:regex text in
  let acc = ref [] in
  Array.iter mats ~f:(fun m ->
    let s, e = Pcre.get_substring_ofs m 0 in
    let toks =
      byte_pair_encode_fast_slice ~vocab_idx:codec.vocab_idx buf ~off:s ~len:(e - s)
    in
    acc := List.rev_append toks !acc);
  List.rev !acc
;;

module Bytes_hashable : Hashtbl.Key with type t = bytes = struct
  include Bytes

  let hash = Hashtbl.hash
end

let parse_tiktoken_bpe contents =
  let tbl = Hashtbl.create (module Bytes_hashable) in
  String.split_lines contents
  |> List.iter ~f:(fun line ->
    let line = String.strip line in
    if not (String.is_empty line)
    then (
      let fields =
        String.split_on_chars line ~on:[ ' '; '\t' ]
        |> List.filter ~f:(Fn.non String.is_empty)
      in
      match fields with
      | [ token; rank ] ->
        Hashtbl.set
          tbl
          ~key:(Bytes.of_string (Base64.decode_exn token))
          ~data:(Int.of_string rank)
      | _ -> failwith ("Bad bpe line: " ^ line)));
  tbl
;;

let decode_native decoder tokens =
  let token_bytes_list =
    List.map
      ~f:(fun token ->
        match Hashtbl.find decoder token with
        | Some bytes -> bytes
        | None -> Bytes.create 0)
      tokens
  in
  Stdlib.Bytes.concat (Bytes.create 0) token_bytes_list
;;

let regex =
  let pattern =
    {|[^\r\n\p{L}\p{N}]?[\p{Lu}\p{Lt}\p{Lm}\p{Lo}\p{M}]*[\p{Ll}\p{Lm}\p{Lo}\p{M}]+(?i:'s|'t|'re|'ve|'m|'ll|'d)?|[^\r\n\p{L}\p{N}]?[\p{Lu}\p{Lt}\p{Lm}\p{Lo}\p{M}]+[\p{Ll}\p{Lm}\p{Lo}\p{M}]*(?i:'s|'t|'re|'ve|'m|'ll|'d)?|\p{N}{1,3}| ?[^\s\p{L}\p{N}]+[\r\n/]*|\s*[\r\n]+|\s+(?!\S)|\s+|}
  in
  Pcre.regexp ~study:true ~jit_compile:true ~flags:[ `UTF8 ] pattern
;;

let create_codec tiktoken_bpe =
  let encoder = parse_tiktoken_bpe tiktoken_bpe in
  let decoder =
    Hashtbl.of_alist_exn
      (module Int)
      (List.map ~f:(fun (k, v) -> v, k) (Hashtbl.to_alist encoder))
  in
  let vocab_idx = build_vocab_index encoder in
  { encoder; decoder; vocab_idx }
;;

let decode ~codec ~encoded =
  let { decoder; _ } = codec in
  let decoded = decode_native decoder encoded in
  decoded
;;

let encode ~codec ~text =
  (* let { encoder; _ } = codec in *)
  (* Test encoding and decoding *)
  let encoded = encode_ordinary regex codec text in
  (* Printf.printf "tokens: %i\n" (List.length encoded);
  Printf.printf
  "Encoded: %s\n"
  (String.concat ~sep:", " (List.map ~f:string_of_int encoded)); *)
  encoded
;;
