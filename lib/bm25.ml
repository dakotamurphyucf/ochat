open Core

(** A very small, self-contained BM25 implementation.  It is **not** optimised
    for large corpora; it is meant to complement the semantic vector search
    with fast literal matches on < 50 k snippets. *)

type doc =
  { id : int
  ; text : string
  }

type t =
  { index : (string, (int * int) list) Hashtbl.t (** term -> (doc_id, tf) list *)
  ; doc_len : (int, int) Hashtbl.t (** doc_id -> |tokens|           *)
  ; n_docs : int
  ; avgdl : float
  }

(* --------------------------------------------------------------------------- *)
(* Stop-word handling                                                           *)
(* A very small English stop-word list; adjust or disable according to your     *)
(* corpus. It mainly helps when comments / docstrings are present.               *)

let stop_words : String.Set.t = String.Set.of_list []

let tokenize s =
  String.split_on_chars
    s
    ~on:[ ' '; '\n'; '\t'; '\r'; '('; ')'; '{'; '}'; '['; ']'; ','; ';' ]
  |> List.filter ~f:(fun tok -> not (String.is_empty tok))
  |> List.map ~f:String.lowercase
  |> List.filter ~f:(fun tok -> not (Set.mem stop_words tok))
;;

let create (docs : doc list) : t =
  let index = Hashtbl.create (module String) in
  let doc_len = Hashtbl.create (module Int) in
  List.iter docs ~f:(fun { id; text } ->
    let tokens = tokenize text in
    Hashtbl.set doc_len ~key:id ~data:(List.length tokens);
    List.fold tokens ~init:String.Map.empty ~f:(fun acc tok ->
      Map.update acc tok ~f:(function
        | None -> 1
        | Some n -> n + 1))
    |> Map.iteri ~f:(fun ~key:tok ~data:tf ->
      Hashtbl.update index tok ~f:(function
        | None -> [ id, tf ]
        | Some l -> (id, tf) :: l)));
  let n_docs = List.length docs in
  let avgdl =
    Float.of_int (Hashtbl.fold doc_len ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data))
    /. Float.of_int n_docs
  in
  { index; doc_len; n_docs; avgdl }
;;

(* Standard BM25 with k1 = 1.5, b = 0.75 *)
let bm25_score ~n ~df ~tf ~doc_len ~avgdl =
  let k1 = 1.5
  and b = 0.75 in
  let idf =
    Float.log (((Float.of_int (n - df) +. 0.5) /. (Float.of_int df +. 0.5)) +. 1.)
  in
  let denom = tf +. (k1 *. (1. -. b +. (b *. Float.of_int doc_len /. avgdl))) in
  idf *. (tf *. (k1 +. 1.) /. denom)
;;

let query t ~text ~k : (int * float) list =
  let tokens = tokenize text |> List.dedup_and_sort ~compare:String.compare in
  let q_len = List.length tokens in
  let scores : (int, float) Hashtbl.t = Hashtbl.create (module Int) in
  let hits : (int, int) Hashtbl.t = Hashtbl.create (module Int) in
  List.iter tokens ~f:(fun tok ->
    match Hashtbl.find t.index tok with
    | None -> ()
    | Some postings ->
      let df = List.length postings in
      List.iter postings ~f:(fun (doc_id, tf) ->
        let doc_len = Hashtbl.find_exn t.doc_len doc_id in
        let s_prev = Option.value (Hashtbl.find scores doc_id) ~default:0.0 in
        let h_prev = Option.value (Hashtbl.find hits doc_id) ~default:0 in
        let s_add =
          bm25_score ~n:t.n_docs ~df ~tf:(Float.of_int tf) ~doc_len ~avgdl:t.avgdl
        in
        Hashtbl.set scores ~key:doc_id ~data:(s_prev +. s_add);
        Hashtbl.set hits ~key:doc_id ~data:(h_prev + 1)));
  (* apply coverage weighting *)
  let results =
    Hashtbl.fold scores ~init:[] ~f:(fun ~key:doc_id ~data:bm25 acc ->
      let covered = Option.value (Hashtbl.find hits doc_id) ~default:0 in
      let coverage =
        if q_len = 0 then 0. else Float.of_int covered /. Float.of_int q_len
      in
      (doc_id, bm25 *. coverage) :: acc)
  in
  results
  |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
  |> fun l -> List.take l k
;;

let dump_debug t = printf "BM25 index: %d terms\n" (Hashtbl.length t.index)

(*────────────────────────  Persistence  ─────────────────────────*)

module Snapshot = struct
  type t =
    { index : (string * (int * int) list) list
    ; doc_len : (int * int) list
    ; n_docs : int
    ; avgdl : float
    }
  [@@deriving bin_io]
end

let to_snapshot (t : t) : Snapshot.t =
  { index = Hashtbl.to_alist t.index
  ; doc_len = Hashtbl.to_alist t.doc_len
  ; n_docs = t.n_docs
  ; avgdl = t.avgdl
  }
;;

let of_snapshot (s : Snapshot.t) : t =
  { index = Hashtbl.of_alist_exn (module String) s.index
  ; doc_len = Hashtbl.of_alist_exn (module Int) s.doc_len
  ; n_docs = s.n_docs
  ; avgdl = s.avgdl
  }
;;

module Io = Bin_prot_utils_eio.With_file_methods (Snapshot)

let write_to_disk path t = Io.File.write_all path [ to_snapshot t ]

let read_from_disk path : t =
  match Io.File.read_all path with
  | [ snapshot ] -> of_snapshot snapshot
  | _ -> failwith "Invalid BM25 snapshot file"
;;
