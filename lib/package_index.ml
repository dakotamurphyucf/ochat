open Core
open Eio
open Owl

module Entry = struct
  type t =
    { pkg : string
    ; vector : float array
    }
  [@@deriving bin_io, sexp]

  let normalize v =
    let vec = Mat.of_array v (Array.length v) 1 in
    let l2 = Mat.vecnorm' vec in
    Array.map v ~f:(fun x -> x /. l2)
  ;;
end

type t = Entry.t array [@@deriving bin_io, sexp]
type path = Eio.Fs.dir_ty Eio.Path.t

(**************************************************************************)
(* Build                                                                    *)
(**************************************************************************)

let build ~(net : _ Eio.Net.t) ~(descriptions : (string * string) list) : t =
  let inputs = List.map descriptions ~f:(fun (pkg, blurb) -> pkg ^ " - " ^ blurb) in
  let resp = Openai.Embeddings.post_openai_embeddings net ~input:inputs in
  let vecs = Array.of_list resp.data in
  Array.mapi vecs ~f:(fun idx item ->
    let pkg, _ = List.nth_exn descriptions idx in
    let v = Array.of_list item.embedding |> Entry.normalize in
    Entry.{ pkg; vector = v })
;;

(**************************************************************************)
(* Query                                                                    *)
(**************************************************************************)

let query (t : t) ~(embedding : float array) ~(k : int) : string list =
  let q_vec = Entry.normalize embedding in
  let scores =
    Array.mapi t ~f:(fun idx e ->
      let dot =
        Array.fold2_exn e.vector q_vec ~init:0.0 ~f:(fun acc a b -> acc +. (a *. b))
      in
      idx, dot)
    |> Array.to_list
    |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
    |> Fn.flip List.take (Int.min k (Array.length t))
  in
  List.map scores ~f:(fun (idx, _) -> (Array.get t idx).pkg)
;;

(**************************************************************************)
(* Persistence                                                              *)
(**************************************************************************)

module Io = Bin_prot_utils_eio.With_file_methods (struct
    type nonrec t = t [@@deriving bin_io]
  end)

let file_name = "package_index.binio"
let save ~(dir : path) (t : t) = Io.File.write Path.(dir / file_name) t

let load ~(dir : path) : t option =
  match Or_error.try_with (fun () -> Io.File.read Path.(dir / file_name)) with
  | Ok idx -> Some idx
  | Error _ -> None
;;

let build_and_save ~net ~descriptions ~dir =
  let idx = build ~net ~descriptions in
  save ~dir idx;
  idx
;;

(**************************************************************************)
let%expect_test "package index query" =
  (* let dummy = [ "eio", "effects based io library"; "core", "Jane Street stdlib" ] in *)
  let idx =
    (* fake embeddings: identity *)
    [| Entry.{ pkg = "eio"; vector = [| 1.0; 0. |] }
     ; Entry.{ pkg = "core"; vector = [| 0.; 1.0 |] } |]
  in
  let res = query idx ~embedding:[| 1.; 0. |] ~k:5 in
  print_s [%sexp (res : string list)];
  [%expect {| (eio) |}]
;;
