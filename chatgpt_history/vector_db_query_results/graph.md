Querying indexed OCaml code with text: **functions to create graphs and traverse them**
Using vector database data from folder: **./vector-ml**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "chatgpt.ml", line 14, characters 0-4343
Module Path: Chatgpt
OCaml Source: Implementation
*)

(**
 This module defines ast traversing functionality  *)
module Traverse = struct
  (** This module defines extract_docs_from_attributes *)
  let extract_docs_from_attributes attrs =
    List.fold_left
      (fun acc attr ->
        match attr.attr_name.txt with
        | "ocaml.doc" | "ocaml.text" ->
          (match attr.attr_payload with
           | PStr
               [ { pstr_desc =
                     Pstr_eval
                       ({ pexp_desc = Pexp_constant (Pconst_string (doc, _, _)); _ }, _)
                 ; _
                 }
               ] -> doc :: acc
           | _ -> acc)
        | _ -> acc)
      []
      attrs
  ;;

  let extract_docs_from_structure_item item =
    match item.pstr_desc with
    | Pstr_value (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pvb_attributes)
        []
        value_bindings
    | Pstr_primitive des -> extract_docs_from_attributes des.pval_attributes
    | Pstr_type (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.ptype_attributes)
        []
        value_bindings
    | Pstr_typext ext -> extract_docs_from_attributes ext.ptyext_attributes
    | Pstr_module bind -> extract_docs_from_attributes bind.pmb_attributes
    | Pstr_recmodule bindings ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pmb_attributes)
        []
        bindings
    | Pstr_modtype dec -> extract_docs_from_attributes dec.pmtd_attributes
    | _ -> []
  ;;

  let extract_docs_from_signiture_item item =
    match item.psig_desc with
    | Psig_value v -> extract_docs_from_attributes v.pval_attributes
    | Psig_type (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.ptype_attributes)
        []
        value_bindings
    | Psig_typext ext -> extract_docs_from_attributes ext.ptyext_attributes
    | Psig_module bind -> extract_docs_from_attributes bind.pmd_attributes
    | Psig_recmodule bindings ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pmd_attributes)
        []
        bindings
    | Psig_modtype dec -> extract_docs_from_attributes dec.pmtd_attributes
    | _ -> []
  ;;

  let s payload =
    let checker =
      object (self)
        inherit
          [string * (string * location * string list) list] Ast_traverse.fold as super

        method! structure items (path, loc) =
          (* Format.printf "%a\n" Astlib.Pprintast.structure  items; *)
          (* print_endline @@ Astlib.Pprintast.string_of_structure items; *)
          let a =
            List.filter_map
              (fun item ->
                match item.pstr_desc with
                | Pstr_attribute _
                | Pstr_extension _
                | Pstr_open _
                | Pstr_include _
                | Pstr_eval _ -> None
                | _ ->
                  (* print_endline "yoyo"; *)
                  (* let res = snd (self#structure_item item (path, [])) in *)
                  (* Format.printf "%a\n" Astlib.Pprintast.structure_item  item; *)
                  Some (snd (self#structure_item item (path, []))))
              items
          in
          path, loc @ List.flatten a

        method! structure_item item (path, loc) =
          (* print_loc item.pstr_loc; *)
          super#structure_item
            item
            (path, (path, item.pstr_loc, extract_docs_from_structure_item item) :: loc)

        method! signature items (path, loc) =
          let a =
            List.map (fun item -> snd (self#signature_item item (path, []))) items
          in
          path, loc @ List.flatten a

        method! signature_item item (path, loc) =
          (* print_loc item.pstr_loc; *)
          super#signature_item
            item
            (path, (path, item.psig_loc, extract_docs_from_signiture_item item) :: loc)

        method! module_binding mb (path, loc) =
          super#module_binding mb (Helpers.enter_opt mb.pmb_name.txt path, loc)

        method! module_declaration md (path, loc) =
          super#module_declaration md (Helpers.enter_opt md.pmd_name.txt path, loc)

        method! module_type_declaration mtd (path, loc) =
          super#module_type_declaration mtd (Helpers.enter mtd.pmtd_name.txt path, loc)
      end
    in
    checker#payload payload
  ;;
end
```

**Result 2:**
```ocaml
(** 
Location: File "chatgpt.ml", line 153, characters 0-739
Module Path: Chatgpt
OCaml Source: Implementation
*)


let traverse ~doc payload module_name ocaml_source = 
  let _, payload = Traverse.s payload (module_name, []) in
  List.map
    ~f:(fun (path, loc, docs) ->
      let contents =
        String.sub
          doc
          ~pos:loc.loc_start.pos_cnum
          ~len:(loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
      in
      let location t =
        Format.sprintf
          "File \"%s\", line %d, characters %d-%d"
          t.loc_start.pos_fname
          t.loc_start.pos_lnum
          (t.loc_start.pos_cnum - t.loc_start.pos_bol)
          (t.loc_end.pos_cnum - t.loc_start.pos_bol)
      in
      { location = location loc
      ; module_path = path
      ; comments = docs
      ; contents
      ; ocaml_source
      })
    payload
```

**Result 3:**
```ocaml
(** 
Location: File "chatgpt.ml", line 78, characters 2-2122
Module Path: Chatgpt.Traverse
OCaml Source: Implementation
*)


let s payload =
    let checker =
      object (self)
        inherit
          [string * (string * location * string list) list] Ast_traverse.fold as super

        method! structure items (path, loc) =
          (* Format.printf "%a\n" Astlib.Pprintast.structure  items; *)
          (* print_endline @@ Astlib.Pprintast.string_of_structure items; *)
          let a =
            List.filter_map
              (fun item ->
                match item.pstr_desc with
                | Pstr_attribute _
                | Pstr_extension _
                | Pstr_open _
                | Pstr_include _
                | Pstr_eval _ -> None
                | _ ->
                  (* print_endline "yoyo"; *)
                  (* let res = snd (self#structure_item item (path, [])) in *)
                  (* Format.printf "%a\n" Astlib.Pprintast.structure_item  item; *)
                  Some (snd (self#structure_item item (path, []))))
              items
          in
          path, loc @ List.flatten a

        method! structure_item item (path, loc) =
          (* print_loc item.pstr_loc; *)
          super#structure_item
            item
            (path, (path, item.pstr_loc, extract_docs_from_structure_item item) :: loc)

        method! signature items (path, loc) =
          let a =
            List.map (fun item -> snd (self#signature_item item (path, []))) items
          in
          path, loc @ List.flatten a

        method! signature_item item (path, loc) =
          (* print_loc item.pstr_loc; *)
          super#signature_item
            item
            (path, (path, item.psig_loc, extract_docs_from_signiture_item item) :: loc)

        method! module_binding mb (path, loc) =
          super#module_binding mb (Helpers.enter_opt mb.pmb_name.txt path, loc)

        method! module_declaration md (path, loc) =
          super#module_declaration md (Helpers.enter_opt md.pmd_name.txt path, loc)

        method! module_type_declaration mtd (path, loc) =
          super#module_type_declaration mtd (Helpers.enter mtd.pmtd_name.txt path, loc)
      end
    in
    checker#payload payload
```

**Result 4:**
```ocaml
(** 
Location: File "chatgpt.ml", line 3, characters 0-253
Module Path: Chatgpt
OCaml Source: Implementation
*)


module Helpers = struct
  let module_name = function
    | None -> "_"
    | Some name -> name
  ;;

  let enter name path = if String.length path = 0 then name else path ^ "." ^ name
  let enter_opt name_opt path = enter (module_name name_opt) path
end
```

**Result 5:**
```ocaml
(** 
Location: File "chatgpt.ml", line 35, characters 2-890
Module Path: Chatgpt.Traverse
OCaml Source: Implementation
*)


let extract_docs_from_structure_item item =
    match item.pstr_desc with
    | Pstr_value (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pvb_attributes)
        []
        value_bindings
    | Pstr_primitive des -> extract_docs_from_attributes des.pval_attributes
    | Pstr_type (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.ptype_attributes)
        []
        value_bindings
    | Pstr_typext ext -> extract_docs_from_attributes ext.ptyext_attributes
    | Pstr_module bind -> extract_docs_from_attributes bind.pmb_attributes
    | Pstr_recmodule bindings ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pmb_attributes)
        []
        bindings
    | Pstr_modtype dec -> extract_docs_from_attributes dec.pmtd_attributes
    | _ -> []
```

**Result 6:**
```ocaml
(** 
Location: File "chatgpt.ml", line 10, characters 2-65
Module Path: Chatgpt.Helpers
OCaml Source: Implementation
*)


let enter_opt name_opt path = enter (module_name name_opt) path
```

**Result 7:**
```ocaml
(** 
Location: File "chatgpt.ml", line 151, characters 0-26
Module Path: Chatgpt
OCaml Source: Implementation
*)


let ( / ) = Eio.Path.( / )
```

**Result 8:**
```ocaml
(** 
Location: File "chatgpt.ml", line 235, characters 0-140
Module Path: Chatgpt
OCaml Source: Implementation
*)


let is_dir path =
  Eio.Path.with_open_in path
  @@ fun file ->
  match (Eio.File.stat file).kind with
  | `Directory -> true
  | _ -> false
```

**Result 9:**
```ocaml
(** 
Location: File "chatgpt.ml", line 9, characters 2-82
Module Path: Chatgpt.Helpers
OCaml Source: Implementation
*)


let enter name path = if String.length path = 0 then name else path ^ "." ^ name
```

**Result 10:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 100, characters 2-355
Module Path: Bin_prot_utils.With_file_methods
OCaml Source: Implementation
*)


module File = struct
    let map ~f = map_bin_prot_list (module M) ~f
    let fold ~f = fold_bin_prot_list (module M) ~f
    let iter = iter_bin_prot_list (module M)
    let read_all = read_bin_prot_list (module M)
    let write_all = write_bin_prot_list (module M)
    let read = read_bin_prot (module M)
    let write = write_bin_prot (module M)
  end
```

**Result 11:**
```ocaml
(** 
Location: File "doc.ml", line 2, characters 0-26
Module Path: Doc
OCaml Source: Implementation
*)


let ( / ) = Eio.Path.( / )
```

**Result 12:**
```ocaml
(** 
Location: File "chatgpt.ml", line 16, characters 2-506
Module Path: Chatgpt.Traverse
OCaml Source: Implementation
*)

(**
 This module defines extract_docs_from_attributes  *)
let extract_docs_from_attributes attrs =
    List.fold_left
      (fun acc attr ->
        match attr.attr_name.txt with
        | "ocaml.doc" | "ocaml.text" ->
          (match attr.attr_payload with
           | PStr
               [ { pstr_desc =
                     Pstr_eval
                       ({ pexp_desc = Pexp_constant (Pconst_string (doc, _, _)); _ }, _)
                 ; _
                 }
               ] -> doc :: acc
           | _ -> acc)
        | _ -> acc)
      []
      attrs
```

**Result 13:**
```ocaml
(** 
Location: File "chatgpt.ml", line 59, characters 2-712
Module Path: Chatgpt.Traverse
OCaml Source: Implementation
*)


let extract_docs_from_signiture_item item =
    match item.psig_desc with
    | Psig_value v -> extract_docs_from_attributes v.pval_attributes
    | Psig_type (_, value_bindings) ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.ptype_attributes)
        []
        value_bindings
    | Psig_typext ext -> extract_docs_from_attributes ext.ptyext_attributes
    | Psig_module bind -> extract_docs_from_attributes bind.pmd_attributes
    | Psig_recmodule bindings ->
      List.fold_left
        (fun acc b -> acc @ extract_docs_from_attributes b.pmd_attributes)
        []
        bindings
    | Psig_modtype dec -> extract_docs_from_attributes dec.pmtd_attributes
    | _ -> []
```

**Result 14:**
```ocaml
(** 
Location: File "vector_db.ml", line 12, characters 0-1303
Module Path: Vector_db
OCaml Source: Implementation
*)


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
    (** this data type holds the vector representation of the underlying document 
      and the id field is the file path location for the doc that the vecctor represents *)
    type t =
      { id : string
      ; vector : vector
      }
    [@@deriving compare, hash, sexp, bin_io]
  end

  include T
  module Io = Bin_prot_utils.With_file_methods (T)

  (** Writes an array of vectors to disk using the Io.File module
      @param vectors The array of vectors to be written to disk
      @param label The label used as the file name for the output file *)
  let write_vectors_to_disk vectors label =
    Io.File.write_all label @@ Array.to_list vectors
  ;;

  (** Reads an array of vectors from disk using the Io.File module
  @param label The label used as the file name for the input file
  @return The array of vectors read from the file *)
  let read_vectors_from_disk label = Array.of_list (Io.File.read_all label)
end
```

**Result 15:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 97, characters 0-431
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


module With_file_methods (M : Bin_prot.Binable.S) = struct
  include M

  module File = struct
    let map ~f = map_bin_prot_list (module M) ~f
    let fold ~f = fold_bin_prot_list (module M) ~f
    let iter = iter_bin_prot_list (module M)
    let read_all = read_bin_prot_list (module M)
    let write_all = write_bin_prot_list (module M)
    let read = read_bin_prot (module M)
    let write = write_bin_prot (module M)
  end
end
```

**Result 16:**
```ocaml
(** 
Location: File "vector_db.ml", line 7, characters 0-69
Module Path: Vector_db
OCaml Source: Implementation
*)

(**
 This represents the vector db. corpus is the matrix of vector representations of the underlying docs. 
  the index is a hash table that maps the index of a doc in the matrix to the file path of the doc   
 *)
type t =
  { corpus : Mat.mat
  ; index : (int, string) Hashtbl.t
  }
```

**Result 17:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 61, characters 0-76
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


let iter_bin_file_list ~f = fold_bin_file_list ~init:() ~f:(fun () v -> f v)
```

**Result 18:**
```ocaml
(** 
Location: File "chatgpt.ml", line 141, characters 0-151
Module Path: Chatgpt
OCaml Source: Implementation
*)


type parse_result =
  { location : string
  ; module_path : string
  ; comments : string list
  ; contents : string
  ; ocaml_source : ocaml_source
  }
```

**Result 19:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 43, characters 0-524
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


let fold_bin_file_list file reader ~init ~f =
  let f fd =
    let channel = Core_unix.in_channel_of_descr fd in
    let size = In_channel.length channel in
    let read buf ~pos ~len = Bigstring_unix.really_input channel ~pos ~len buf in
    let rec aux acc =
      if Int64.(In_channel.pos channel = size)
      then acc
      else (
        let v = Bin_prot.Utils.bin_read_stream ~read reader in
        aux (f acc v))
    in
    aux init
  in
  Core_unix.with_file file ~mode:[ Core_unix.O_RDONLY; Core_unix.O_CREAT ] ~f
```

**Result 20:**
```ocaml
(** 
Location: File "vector_db.ml", line 13, characters 2-261
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


module Float_array = struct
    type t = float array [@@deriving compare, bin_io, sexp]

    let hash_fold_t hash_state t =
      Array.fold t ~init:hash_state ~f:(fun hs elem -> Float.hash_fold_t hs elem)
    ;;

    let hash = Hash.of_fold hash_fold_t
  end
```
