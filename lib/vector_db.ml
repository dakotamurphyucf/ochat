open Core
open Owl

(** In-memory dense vector index.

    The corpus is a {e column-major} Owl matrix holding one L2-normalised
    embedding per document.  The auxiliary [index] table maps the column
    number back to the (hashed) on-disk document id and its token
    length.  See [vector_db.doc.md] for a narrative overview. *)
type t =
  { corpus : Mat.mat
  ; index : (int, string * int) Hashtbl.t (** idx -> (doc_id, token_len) *)
  }

type path = Eio.Fs.dir_ty Eio.Path.t

module Vec = struct
  module Float_array = struct
    type t = float array [@@deriving compare, bin_io, sexp]

    let hash_fold_t hash_state t =
      Array.fold t ~init:hash_state ~f:(fun hs elem -> Float.hash_fold_t hs elem)
    ;;

    let hash = Hash.of_fold hash_fold_t
  end

  type vector = Float_array.t [@@deriving hash, compare, bin_io, sexp]

  module T = struct
    (** Serialisable on-disk representation – mirrors
        {!module-vector_db.Vec} in the interface.  [vector] is the raw
        {i unnormalised} embedding.  Normalisation happens when building
        the corpus. *)
    type t =
      { id : string (** hashed filename *)
      ; len : int (** token length of the snippet *)
      ; vector : vector
      }
    [@@deriving compare, hash, sexp, bin_io]
  end

  include T
  module Io = Bin_prot_utils_eio.With_file_methods (T)

  (** [write_vectors_to_disk vecs path] serialises [vecs] to [path]
      using {!Bin_prot_utils_eio}. *)
  let write_vectors_to_disk vectors label =
    Io.File.write_all label @@ Array.to_list vectors
  ;;

  (** [read_vectors_from_disk path] deserialises the array previously
      written by {!write_vectors_to_disk}. *)
  let read_vectors_from_disk label = Array.of_list (Io.File.read_all label)
end

let normalize doc =
  let vec = Owl.Mat.of_array doc (Array.length doc) 1 in
  let l2norm = Mat.vecnorm' vec in
  Mat.map (fun x -> x /. l2norm) vec
;;

(** [create_corpus docs] builds a snapshot from raw embeddings.

    Guarantees:

    • every column of the resulting matrix has unit L2-norm;
    • [Hashtbl.length index = Array.length docs]. *)
let create_corpus docs' =
  let docs = Array.map ~f:(fun doc -> normalize doc.Vec.vector) docs' in
  let corpus = Mat.of_cols docs in
  let index = Hashtbl.create (module Int) ~size:(Array.length docs) in
  Array.iteri ~f:(fun i doc -> Hashtbl.add_exn index ~key:i ~data:(doc.id, doc.len)) docs';
  { corpus; index }
;;

(* Applies a simple length-penalty so that vectors representing chunks far
   from the target window (≈ 192 tokens) are slightly down-weighted. The
   penalty factor is linear with slope [alpha]. *)
let apply_length_penalty ~target ~alpha original_score chunk_len =
  let penalty =
    1.0
    -. (alpha
        *. (Float.abs (Float.of_int chunk_len -. Float.of_int target)
            /. Float.of_int target))
  in
  original_score *. Float.max 0.0 penalty
;;

(** [query t embedding k] returns the indices of the [k] nearest
    neighbours (cosine similarity).

    [embedding] must be L2-normalised (`d × 1`) and share the same
    dimensionality as the corpus.  If [k] exceeds `Mat.col_num t.corpus`
    the output is truncated. *)
let query t doc k =
  (* Compute cosine similarities *)
  let vec = Mat.transpose doc in
  let sims = Mat.(vec *@ t.corpus) in
  (* Convert to (idx, score) list *)
  let n_cols = Mat.col_num t.corpus in
  let scores =
    List.init n_cols ~f:(fun i ->
      let s = Mat.get sims 0 i in
      let chunk_len =
        match Hashtbl.find t.index i with
        | Some (_id, len) -> len
        | None -> 0
      in
      let _s' = apply_length_penalty ~target:192 ~alpha:0.3 s chunk_len in
      i, s)
  in
  let top =
    scores
    |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
    |> fun l -> List.take l (Int.min k n_cols) |> List.map ~f:fst |> Array.of_list
  in
  top
;;

let add_doc corpus doc =
  Mat.of_cols @@ Array.concat [ Mat.to_cols corpus; Mat.to_cols (normalize doc) ]
;;

(*────────────────────────  Hybrid retrieval  ─────────────────────────*)

let query_hybrid t ~bm25 ~beta ~embedding ~text ~k =
  (* 1. cosine similarities for all docs *)
  let sims = Mat.(Mat.transpose embedding *@ t.corpus) in
  let n_cols = Mat.col_num t.corpus in
  let cos_ranked =
    List.init n_cols ~f:(fun i -> i, Mat.get sims 0 i)
    |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
  in
  let shortlist_n = Int.min n_cols (k * 20) in
  let shortlist = List.take cos_ranked shortlist_n in
  (* 2. BM25 scores (only need doc_ids) *)
  let bm25_scores = Bm25.query bm25 ~text ~k:shortlist_n in
  let bm25_tbl = Hashtbl.of_alist_exn (module Int) bm25_scores in
  let bm25_max =
    List.fold shortlist ~init:0.0 ~f:(fun acc (idx, _) ->
      match Hashtbl.find bm25_tbl idx with
      | Some s -> Float.max acc s
      | None -> acc)
  in
  let merged =
    List.map shortlist ~f:(fun (idx, cos) ->
      let bm25_raw = Hashtbl.find bm25_tbl idx |> Option.value ~default:0.0 in
      let bm25_norm =
        if Float.equal bm25_max 0.0 then 0.0 else Float.(bm25_raw / bm25_max)
      in
      let score = ((1. -. beta) *. cos) +. (beta *. bm25_norm) in
      idx, score)
  in
  let final_ranked =
    merged |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
  in
  List.take final_ranked k |> List.map ~f:fst |> Array.of_list
;;

(** [initialize path] reads a previously serialised array of {!Vec.t}
    from [path] and calls {!create_corpus}. *)
let initialize file =
  let docs' = Vec.read_vectors_from_disk file in
  create_corpus docs'
;;

(** [get_docs dir t idxs] synchronously loads the raw text of the
    documents referenced by [idxs].  The result order matches the input
    indices. *)
let get_docs dir t indexs =
  Eio.Fiber.List.map
    (fun idx ->
       let file_path, _len = Hashtbl.find_exn t.index idx in
       Io.load_doc ~dir file_path)
    (Array.to_list indexs)
;;
