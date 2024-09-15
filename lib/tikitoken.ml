open Core


let byte_pair_merge (piece : Bytes.t) (ranks : (Bytes.t, int) Hashtbl.t)
  : (int * int) array
  =
  let len = Bytes.length piece in
  let parts = Array.init len ~f:(fun i -> i, i + 1) in
  let rec merge_parts parts =
    match Array.length parts with
    | 0 | 1 -> parts
    | _ ->
      let min_rank, min_index =
        let sub = Array.sub parts ~pos:0 ~len:(Array.length parts - 1) in
        let pairs = Array.mapi ~f:(fun i part -> part, parts.(i + 1)) sub in
        let f i (min_rank, min_index) ((start1, _end1), (_start2, end2)) =
          let combined = Bytes.sub piece ~pos:start1 ~len:(end2 - start1) in
          match Hashtbl.find ranks combined with
          | None -> min_rank, min_index
          | Some rank ->
            (match min_rank with
             | None -> Some rank, Some i
             | Some min_rank ->
               if rank < min_rank then Some rank, Some i else Some min_rank, min_index)
        in
        Array.foldi ~f ~init:(None, None) pairs
      in
      (match min_rank, min_index with
       | None, _ | _, None -> parts
       | Some _, Some i ->
         let merged_part = fst parts.(i), snd parts.(i + 1) in
         let new_parts =
           Array.concat
             [ Array.sub parts ~pos:0 ~len:i
             ; [| merged_part |]
             ; Array.sub parts ~pos:(i + 2) ~len:(Array.length parts - i - 2)
             ]
         in
         merge_parts new_parts)
  in
  merge_parts parts
;;

let byte_pair_encode (piece : Bytes.t) (ranks : (Bytes.t, int) Hashtbl.t) : int list =
  let len = Bytes.length piece in
  if len = 1
  then (
    match Hashtbl.find ranks piece with
    | Some rank -> [ rank ]
    | None -> [])
  else (
    let merged_parts = byte_pair_merge piece ranks in
    Array.to_list
      (Array.map
         ~f:(fun (start_pos, end_pos) ->
           let segment = Bytes.sub piece ~pos:start_pos ~len:(end_pos - start_pos) in
           match Hashtbl.find ranks segment with
           | Some rank -> rank
           | None -> -1)
         merged_parts))
;;

let encode_ordinary regex encoder text =
  let matches = Pcre.exec_all ~rex:regex text |> Array.to_list in
  (* Printf.printf "matches: %i\n" (List.length matches); *)
  List.fold_left
    ~f:(fun acc mat ->
      let piece = Pcre.get_substring mat 0 in
      (* Printf.printf "piece: %s\n" piece; *)
      match Hashtbl.find encoder (Bytes.of_string piece) with
      | Some token ->
        (* Printf.printf "token: %i\n" token; *)
        token :: acc
      | None -> List.rev_append (byte_pair_encode (Bytes.of_string piece) encoder) acc)
    ~init:[]
    matches
  |> List.rev
;;


module Bytes_hashable : Hashtbl.Key with type t = bytes = struct
  include Bytes

  let hash = Hashtbl.hash
end

let parse_tiktoken_bpe contents =
  let lines = String.split ~on:'\n' contents in
  let tbl = Hashtbl.create ~size:(List.length lines) (module Bytes_hashable) in
  List.iter
    ~f:(fun line ->
      match String.split ~on:' ' line with
      | [ token; rank ] ->
        Hashtbl.add_exn
          tbl
          ~key:(Bytes.of_string @@ Base64.decode_exn token)
          ~data:(int_of_string rank)
      | _ -> ())
    lines;
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
    {|(?i:'s|'t|'re|'ve|'m|'ll|'d)|[^\r\n\p{L}\p{N}]?\p{L}+|\p{N}{1,3}| ?[^\s\p{L}\p{N}]+[\r\n]*|\s*[\r\n]+|\s+(?!\S)|\s+|}
  in
  Pcre.regexp pattern
;;

type codec =
  { encoder : (bytes, int) Hashtbl.t
  ; decoder : (int, bytes) Hashtbl.t
  }

let create_codec tiktoken_bpe =
  let encoder = parse_tiktoken_bpe tiktoken_bpe in
  (* Create a core_bpe instance *)
  let decoder =
    Hashtbl.of_alist_exn
      (module Int)
      (List.map ~f:(fun (k, v) -> v, k) (Hashtbl.to_alist encoder))
  in
  { encoder; decoder }
;;


let decode ~codec ~encoded =
  let { decoder; _ } = codec in
  let decoded = decode_native decoder encoded in
  Printf.printf "Decoded: %s\n" (Bytes.to_string decoded);
  decoded
;;

let encode ~codec ~text =
  let { encoder; _ } = codec in
  (* Test encoding and decoding *)
  let encoded = encode_ordinary regex encoder text in
  (* Printf.printf "tokens: %i\n" (List.length encoded);
  Printf.printf
  "Encoded: %s\n"
  (String.concat ~sep:", " (List.map ~f:string_of_int encoded)); *)
  
  encoded
;;
