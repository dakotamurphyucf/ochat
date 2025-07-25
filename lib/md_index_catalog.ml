open Core
open Eio
open Owl

(** Persistent catalogue that maps Markdown index folders to the
    L2-normalised centroid embeddings of their snippets.

    The catalogue is stored as a [`Bin_prot`]-serialised array under
    {!file:md_index_catalog.binio} inside the directory that contains
    the individual indexes (defaults to {!".md_index"}).  All
    functions are non-blocking Eio operations and therefore must run
    within a fibre. *)

module Entry = struct
  type t =
    { name : string
    ; description : string
    ; vector : float array
    }
  [@@deriving bin_io, sexp]

  let normalize v =
    let vec = Mat.of_array v (Array.length v) 1 in
    let l2 = Mat.vecnorm' vec in
    if Float.equal l2 0. then v else Array.map v ~f:(fun x -> x /. l2)
  ;;
end

type t = Entry.t array [@@deriving bin_io, sexp]
type path = Eio.Fs.dir_ty Eio.Path.t

(**************************************************************************)
(* Persistence                                                             *)
(**************************************************************************)

module Io = Bin_prot_utils_eio.With_file_methods (struct
    type nonrec t = t [@@deriving bin_io]
  end)

let file_name = "md_index_catalog.binio"
let save ~(dir : path) (cat : t) = Io.File.write Path.(dir / file_name) cat

let load ~(dir : path) : t option =
  match Or_error.try_with (fun () -> Io.File.read Path.(dir / file_name)) with
  | Ok cat -> Some cat
  | Error _ -> None
;;

(**************************************************************************)
(* Updating                                                                *)
(**************************************************************************)

let add_or_update
      ~(dir : path)
      ~(name : string)
      ~(description : string)
      ~(vector : float array)
  : unit
  =
  let entry = Entry.{ name; description; vector = Entry.normalize vector } in
  let existing = Option.value (load ~dir) ~default:[||] |> Array.to_list in
  let without_name = List.filter existing ~f:(fun e -> not (String.equal e.name name)) in
  let updated = Array.of_list (entry :: without_name) in
  save ~dir updated
;;
