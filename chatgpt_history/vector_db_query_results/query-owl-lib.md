Querying indexed OCaml code with text: **functions using owl library**
Using vector database data from folder: **./vector-ml**
Returning top **35** results

**Result 1:**
```ocaml
(** 
Location: File "vector_db.ml", line 51, characters 0-148
Module Path: Vector_db
OCaml Source: Implementation
*)


let normalize doc =
  let vec = Owl.Mat.of_array doc (Array.length doc) 1 in
  let l2norm = Mat.vecnorm' vec in
  Mat.map (fun x -> x /. l2norm) vec
```

**Result 2:**
```ocaml
(** 
Location: File "doc.ml", line 2, characters 0-26
Module Path: Doc
OCaml Source: Implementation
*)


let ( / ) = Eio.Path.( / )
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
Location: File "chatgpt.ml", line 151, characters 0-26
Module Path: Chatgpt
OCaml Source: Implementation
*)


let ( / ) = Eio.Path.( / )
```

**Result 5:**
```ocaml
(** 
Location: File "vector_db.ml", line 36, characters 2-50
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


module Io = Bin_prot_utils.With_file_methods (T)
```

**Result 6:**
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

**Result 7:**
```ocaml
(** 
Location: File "vector_db.ml", line 25, characters 2-310
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


module T = struct
    (** this data type holds the vector representation of the underlying document 
      and the id field is the file path location for the doc that the vecctor represents *)
    type t =
      { id : string
      ; vector : vector
      }
    [@@deriving compare, hash, sexp, bin_io]
  end
```

**Result 8:**
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

**Result 9:**
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

**Result 12:**
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

**Result 13:**
```ocaml
(** 
Location: File "chatgpt.ml", line 207, characters 0-91
Module Path: Chatgpt
OCaml Source: Implementation
*)


type _ file_type =
  | Mli : mli file_type
  | Ml : ml file_type

and mli = MLI
and ml = ML
```

**Result 14:**
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

**Result 15:**
```ocaml
(** 
Location: File "chatgpt.ml", line 137, characters 0-52
Module Path: Chatgpt
OCaml Source: Implementation
*)


type ocaml_source =
  | Interface
  | Implementation
```

**Result 16:**
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

**Result 17:**
```ocaml
(** 
Location: File "vector_db.ml", line 87, characters 0-106
Module Path: Vector_db
OCaml Source: Implementation
*)


let add_doc corpus doc =
  Mat.of_cols @@ Array.concat [ Mat.to_cols corpus; Mat.to_cols (normalize doc) ]
```

**Result 18:**
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

**Result 19:**
```ocaml
(** 
Location: File "vector_db.ml", line 28, characters 4-109
Module Path: Vector_db.Vec.T
OCaml Source: Implementation
*)

(**
 this data type holds the vector representation of the underlying document 
      and the id field is the file path location for the doc that the vecctor represents  *)
type t =
      { id : string
      ; vector : vector
      }
    [@@deriving compare, hash, sexp, bin_io]
```

**Result 20:**
```ocaml
(** 
Location: File "doc.ml", line 19, characters 0-74
Module Path: Doc
OCaml Source: Implementation
*)


let load_prompt env file =
  let path = env / file in
  Eio.Path.load path
```

**Result 21:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 107, characters 4-41
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Implementation
*)


let write = write_bin_prot (module M)
```

**Result 22:**
```ocaml
(** 
Location: File "vector_db.ml", line 23, characters 2-70
Module Path: Vector_db.Vec
OCaml Source: Implementation
*)


type vector = Float_array.t [@@deriving hash, compare, bin_io, sexp]
```

**Result 23:**
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

**Result 24:**
```ocaml
(** 
Location: File "chatgpt.ml", line 4, characters 2-70
Module Path: Chatgpt.Helpers
OCaml Source: Implementation
*)


let module_name = function
    | None -> "_"
    | Some name -> name
```

**Result 25:**
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

**Result 26:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 106, characters 4-39
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Implementation
*)


let read = read_bin_prot (module M)
```

**Result 27:**
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

**Result 28:**
```ocaml
(** 
Location: File "chatgpt.ml", line 229, characters 0-132
Module Path: Chatgpt
OCaml Source: Implementation
*)

(**
 [module_info] is a record type representing the metadata of an OCaml module,
    combining the interface (mli) and implementation (ml) files.  *)
type module_info =
  { mli_file : mli file_info option
  ; ml_file : ml file_info option
  ; module_path : Eio.Fs.dir Eio.Path.t
  }
```

**Result 29:**
```ocaml
(** 
Location: File "openai.ml", line 27, characters 0-69
Module Path: Openai
OCaml Source: Implementation
*)

(**
 [api_key] is the API key for the OpenAI API.  *)
let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:""
```

**Result 30:**
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

**Result 31:**
```ocaml
(** 
Location: File "chatgpt.ml", line 197, characters 0-92
Module Path: Chatgpt
OCaml Source: Implementation
*)


let to_res f =
  try Ok (f ()) with
  | Eio.Io _ as ex -> Error (Fmt.str "%a" Eio.Exn.pp ex)
```

**Result 32:**
```ocaml
(** 
Location: File "chatgpt.ml", line 10, characters 2-65
Module Path: Chatgpt.Helpers
OCaml Source: Implementation
*)


let enter_opt name_opt path = enter (module_name name_opt) path
```

**Result 33:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 29, characters 0-401
Module Path: Bin_prot_utils
OCaml Source: Implementation
*)


let read_bin_prot' file reader =
  let f fd =
    let buf =
      Bin_prot.Common.create_buf (Int64.to_int_exn (Core_unix.fstat fd).st_size)
    in
    Bigstring_unix.really_read fd buf;
    let res = Bigstring_unix.read_bin_prot buf reader in
    match res with
    | Error err -> failwith (Error.to_string_hum err)
    | Ok (v, _) -> v
  in
  Core_unix.with_file file ~mode:[ Core_unix.O_RDONLY ] ~f
```

**Result 34:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 103, characters 4-44
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Implementation
*)


let iter = iter_bin_prot_list (module M)
```

**Result 35:**
```ocaml
(** 
Location: File "bin_prot_utils.ml", line 105, characters 4-50
Module Path: Bin_prot_utils.With_file_methods.File
OCaml Source: Implementation
*)


let write_all = write_bin_prot_list (module M)
```
Querying indexed OCaml code with text: **functions using owl library**
Using vector database data from folder: **./vector-core**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "blang.ml", line 320, characters 2-21
Module Path: Blang.O
OCaml Source: Implementation
*)


let ( || ) = orelse
```

**Result 2:**
```ocaml
(** 
Location: File "quickcheck.ml", line 80, characters 2-63
Module Path: Quickcheck.Observer
OCaml Source: Implementation
*)


let variant6 = Polymorphic_types.quickcheck_observer_variant6
```

**Result 3:**
```ocaml
(** 
Location: File "quickcheck.ml", line 85, characters 2-59
Module Path: Quickcheck.Observer
OCaml Source: Implementation
*)


let tuple6 = Polymorphic_types.quickcheck_observer_tuple6
```

**Result 4:**
```ocaml
(** 
Location: File "blang.ml", line 312, characters 0-189
Module Path: Blang
OCaml Source: Implementation
*)


module O = struct
  include T

  let not = not_
  let and_ = and_
  let or_ = or_
  let constant = constant
  let ( && ) = andalso
  let ( || ) = orelse
  let ( ==> ) a b = (not a) || b
end
```

**Result 5:**
```ocaml
(** 
Location: File "blang.ml", line 317, characters 2-15
Module Path: Blang.O
OCaml Source: Implementation
*)


let or_ = or_
```

**Result 6:**
```ocaml
(** 
Location: File "quickcheck.ml", line 78, characters 2-63
Module Path: Quickcheck.Observer
OCaml Source: Implementation
*)


let variant4 = Polymorphic_types.quickcheck_observer_variant4
```

**Result 7:**
```ocaml
(** 
Location: File "binable.ml", line 10, characters 0-65
Module Path: Binable
OCaml Source: Implementation
*)


module Of_binable1 = Of_binable1_without_uuid [@@alert "-legacy"]
```

**Result 8:**
```ocaml
(** 
Location: File "command_shape.ml", line 402, characters 2-343
Module Path: Command_shape.Anons
OCaml Source: Implementation
*)


module Grammar = struct
    type t = Stable.Anons.Grammar.Model.t =
      | Zero
      | One of string
      | Many of t
      | Maybe of t
      | Concat of t list
      | Ad_hoc of string
    [@@deriving bin_io, compare, sexp]

    let invariant = Stable.Anons.Grammar.Model.invariant
    let usage = Stable.Anons.Grammar.Model.usage
  end
```

**Result 9:**
```ocaml
(** 
Location: File "command_shape.ml", line 413, characters 4-48
Module Path: Command_shape.Anons.Grammar
OCaml Source: Implementation
*)


let usage = Stable.Anons.Grammar.Model.usage
```

**Result 10:**
```ocaml
(** 
Location: File "blang.ml", line 316, characters 2-17
Module Path: Blang.O
OCaml Source: Implementation
*)


let and_ = and_
```

**Result 11:**
```ocaml
(** 
Location: File "quickcheck.ml", line 69, characters 0-1514
Module Path: Quickcheck
OCaml Source: Implementation
*)


module Observer = struct
  include Observer

  let of_hash (type a) (module M : Deriving_hash with type t = a) =
    of_hash_fold M.hash_fold_t
  ;;

  let variant2 = Polymorphic_types.quickcheck_observer_variant2
  let variant3 = Polymorphic_types.quickcheck_observer_variant3
  let variant4 = Polymorphic_types.quickcheck_observer_variant4
  let variant5 = Polymorphic_types.quickcheck_observer_variant5
  let variant6 = Polymorphic_types.quickcheck_observer_variant6
  let tuple2 = Polymorphic_types.quickcheck_observer_tuple2
  let tuple3 = Polymorphic_types.quickcheck_observer_tuple3
  let tuple4 = Polymorphic_types.quickcheck_observer_tuple4
  let tuple5 = Polymorphic_types.quickcheck_observer_tuple5
  let tuple6 = Polymorphic_types.quickcheck_observer_tuple6
  let of_predicate a b ~f = unmap (variant2 a b) ~f:(fun x -> if f x then `A x else `B x)
  let singleton () = opaque
  let doubleton f = of_predicate (singleton ()) (singleton ()) ~f
  let enum _ ~f = unmap int ~f

  let of_list list ~equal =
    let f x =
      match List.findi list ~f:(fun _ y -> equal x y) with
      | None -> failwith "Quickcheck.Observer.of_list: value not found"
      | Some (i, _) -> i
    in
    enum (List.length list) ~f
  ;;

  let of_fun f = create (fun x ~size ~hash -> observe (f ()) x ~size ~hash)

  let comparison ~compare ~eq ~lt ~gt =
    unmap
      (variant3 lt (singleton ()) gt)
      ~f:(fun x ->
        let c = compare x eq in
        if c < 0 then `A x else if c > 0 then `C x else `B x)
  ;;
end
```

**Result 12:**
```ocaml
(** 
Location: File "command_shape.ml", line 5, characters 2-1990
Module Path: Command_shape.Stable
OCaml Source: Implementation
*)


module Anons = struct
    module Grammar = struct
      module V1 = struct
        type t =
          | Zero
          | One of string
          | Many of t
          | Maybe of t
          | Concat of t list
          | Ad_hoc of string
        [@@deriving bin_io, compare, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| a17fd34ec213e508db450f6469f7fe99 |}]
        ;;

        let rec invariant t =
          Base.Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
            match t with
            | Zero -> ()
            | One _ -> ()
            | Many Zero -> failwith "Many Zero should be just Zero"
            | Many t -> invariant t
            | Maybe Zero -> failwith "Maybe Zero should be just Zero"
            | Maybe t -> invariant t
            | Concat [] | Concat [ _ ] -> failwith "Flatten zero and one-element Concat"
            | Concat ts -> Base.List.iter ts ~f:invariant
            | Ad_hoc _ -> ())
        ;;

        let t_of_sexp sexp =
          let t = [%of_sexp: t] sexp in
          invariant t;
          t
        ;;

        let rec usage = function
          | Zero -> ""
          | One usage -> usage
          | Many Zero -> failwith "bug in command.ml"
          | Many (One _ as t) -> Base.Printf.sprintf "[%s ...]" (usage t)
          | Many t -> Base.Printf.sprintf "[(%s) ...]" (usage t)
          | Maybe Zero -> failwith "bug in command.ml"
          | Maybe t -> Base.Printf.sprintf "[%s]" (usage t)
          | Concat ts -> Base.String.concat ~sep:" " (Base.List.map ts ~f:usage)
          | Ad_hoc usage -> usage
        ;;
      end

      module Model = V1
    end

    module V2 = struct
      type t =
        | Usage of string
        | Grammar of Grammar.V1.t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 081d9ec167903f8f8c49cbf8e3fb3a66 |}]
      ;;
    end

    module Model = V2
  end
```

**Result 13:**
```ocaml
(** 
Location: File "ofday_ns.ml", line 220, characters 0-73
Module Path: Ofday_ns
OCaml Source: Implementation
*)


let t_sexp_grammar = Sexplib.Sexp_grammar.coerce Stable.V1.t_sexp_grammar
```

**Result 14:**
```ocaml
(** 
Location: File "quickcheck.ml", line 79, characters 2-63
Module Path: Quickcheck.Observer
OCaml Source: Implementation
*)


let variant5 = Polymorphic_types.quickcheck_observer_variant5
```

**Result 15:**
```ocaml
(** 
Location: File "only_in_test.ml", line 4, characters 0-23
Module Path: Only_in_test
OCaml Source: Implementation
*)


let of_thunk = from_fun
```

**Result 16:**
```ocaml
(** 
Location: File "univ_map.ml", line 4, characters 0-30
Module Path: Univ_map
OCaml Source: Implementation
*)


module Uid = Type_equal.Id.Uid
```

**Result 17:**
```ocaml
(** 
Location: File "quickcheck.ml", line 86, characters 2-89
Module Path: Quickcheck.Observer
OCaml Source: Implementation
*)


let of_predicate a b ~f = unmap (variant2 a b) ~f:(fun x -> if f x then `A x else `B x)
```

**Result 18:**
```ocaml
(** 
Location: File "quickcheck.ml", line 76, characters 2-63
Module Path: Quickcheck.Observer
OCaml Source: Implementation
*)


let variant2 = Polymorphic_types.quickcheck_observer_variant2
```

**Result 19:**
```ocaml
(** 
Location: File "command_shape.ml", line 58, characters 4-279
Module Path: Command_shape.Stable.Anons
OCaml Source: Implementation
*)


module V2 = struct
      type t =
        | Usage of string
        | Grammar of Grammar.V1.t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 081d9ec167903f8f8c49cbf8e3fb3a66 |}]
      ;;
    end
```

**Result 20:**
```ocaml
(** 
Location: File "quickcheck.ml", line 188, characters 2-91
Module Path: Quickcheck.Let_syntax
OCaml Source: Implementation
*)


module Let_syntax = struct
    include Generator
    module Open_on_rhs = Generator
  end
```
