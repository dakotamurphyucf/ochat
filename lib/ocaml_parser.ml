open Ppxlib

module Helpers = struct
  let module_name = function
    | None -> "_"
    | Some name -> name
  ;;

  let enter name path = if String.length path = 0 then name else path ^ "." ^ name
  let enter_opt name_opt path = enter (module_name name_opt) path
end

(** This module defines ast traversing functionality *)
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
            List.filter_map
              (fun item ->
                 match item.psig_desc with
                 | Psig_open _ | Psig_include _ -> None
                 | _ ->
                   (* print_endline "yoyo"; *)
                   (* let res = snd (self#structure_item item (path, [])) in *)
                   (* Format.printf "%a\n" Astlib.Pprintast.structure_item  item; *)
                   Some (snd (self#signature_item item (path, []))))
              items
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

type ocaml_source =
  | Interface
  | Implementation

type parse_result =
  { location : string
  ; module_path : string
  ; comments : string list
  ; contents : string
  ; ocaml_source : ocaml_source
  }

open Core
open Io

type traverse_input =
  { doc : string
  ; payload : payload
  ; module_name : string
  ; ocaml_source : ocaml_source
  }

let traverse traverse_input =
  let { doc; payload; module_name; ocaml_source } = traverse_input in
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
;;

let ( / ) p1 p2 =
  match p1, p2 with
  | p1, "" -> Filename.concat p1 p2
  | _, p2 when not (Filename.is_relative p2) -> p2
  | ".", p2 -> p2
  | p1, p2 -> Filename.concat p1 p2
;;

(** [parse file file_type module_name] parses the given [file] with the specified [file_type] (either [Interface] or [Implementation]) and [module_name]. It returns a list of [parse_result] records containing the location, module path, comments, contents, and file type for each parsed item.

    @param file The file to be parsed.
    @param ocaml_source The type of the file, either [Interface] or [Implementation].
    @param module_name The name of the module being parsed.
    @return A list of [parse_result] records containing information about the parsed items. *)
let parse dir file ocaml_source module_name =
  let doc = load_doc ~dir file in
  let lexbuf = Lexing.from_string doc in
  Lexing.set_filename lexbuf file;
  let payload =
    match ocaml_source with
    | Interface -> PSig (Parse.interface lexbuf)
    | Implementation -> PStr (Parse.implementation lexbuf)
  in
  { doc; payload; module_name; ocaml_source }
;;

(** [dir  path] reads the directoryZ at the given [path]
    
  @returns a string list of all the file and path names in the directory*)
let directory dir path = Eio.Path.read_dir Eio.Path.(dir / path)

type _ file_type =
  | Mli : mli file_type
  | Ml : ml file_type

and mli = MLI
and ml = ML

(** [file_info] is a record type that contains the file_type and file_name. *)
type 'a file_info =
  { file_type : 'a file_type
  ; file_name : string
  }
(** Now, the file_type is encoded in the type system, and you can create file_info values with specific file types:

    {[
      let mli_file = mli file_info { file_type = Mli; file_name = "example.mli" }
      let ml_file : ml file_info = { file_type = Ml; file_name = "example.ml" }
    ]}
*)

(** [module_info] is a record type representing the metadata of an OCaml module,
    combining the interface (mli) and implementation (ml) files. *)
type module_info =
  { mli_file : mli file_info option
  ; ml_file : ml file_info option
  ; module_path : string
  }

(** [separate_ocaml_files entries] separates OCaml source files into three categories: implementation files (.ml), interface files (.mli), and other files.

    @param entries is a list of file names.
    @return a tuple containing three lists:
            - a list of pairs (name, entry) for .ml files, where [name] is the file name without the .ml extension and [entry] is the original file name.
            - a list of pairs (name, entry) for .mli files, where [name] is the file name without the .mli extension and [entry] is the original file name.
            - a list of other file names that are not .ml or .mli files.
*)
let separate_ocaml_files entries =
  List.fold_left
    ~f:(fun (ml_files, mli_files, others) entry ->
      let ( = ) = String.( = ) in
      match Filename.split_extension entry with
      | name, Some ext when ext = "ml" -> (name, entry) :: ml_files, mli_files, others
      | name, Some ext when ext = "mli" -> ml_files, (name, entry) :: mli_files, others
      | _ -> ml_files, mli_files, entry :: others)
    ~init:([], [], [])
    entries
;;

(** [group_ocaml_files path ml_files mli_files] groups OCaml source files into a list of [module_info] records, each representing the metadata of an OCaml module.

    @param path is the directory path containing the OCaml source files.
    @param ml_files is a list of pairs (name, entry) for .ml files, where [name] is the file name without the .ml extension and [entry] is the original file name.
    @param mli_files is a list of pairs (name, entry) for .mli files, where [name] is the file name without the .mli extension and [entry] is the original file name.
    @return a list of [module_info] records, each containing the metadata of an OCaml module, including the interface (mli) and implementation (ml) files and the module path.
*)
let group_ocaml_files path ml_files mli_files : module_info list =
  let ml_map = String.Map.of_alist_exn ml_files in
  let mli_map = String.Map.of_alist_exn mli_files in
  let modules = String.Set.union_list [ Map.key_set ml_map; Map.key_set mli_map ] in
  Set.fold
    ~f:(fun acc module_name ->
      let ml_file = Map.find ml_map module_name in
      let mli_file = Map.find mli_map module_name in
      { mli_file =
          Option.map ~f:(fun file -> { file_type = Mli; file_name = file }) mli_file
      ; ml_file = Option.map ~f:(fun file -> { file_type = Ml; file_name = file }) ml_file
      ; module_path = path (* This needs to be updated to the correct path *)
      }
      :: acc)
    ~init:[]
    modules
;;

(** [collect_ocaml_files dir path] recursively collects OCaml source files from the directory specified by [path] and its subdirectories.

    @param dir is the directory in which the function is executed.
    @param path is the directory path containing the OCaml source files.
    @return a [Result.t] containing a list of [module_info] records, each representing the metadata of an OCaml module, including the interface (mli) and implementation (ml) files and the module path, or an error message if there was an issue reading the directory.
*)
let collect_ocaml_files dir path =
  let rec collect_files paths acc =
    match paths with
    | [] -> Ok acc
    | path :: paths ->
      (match to_res (fun () -> directory dir path) with
       | Error e -> Error (Format.sprintf "Error reading directory: %s" e)
       | Ok entries ->
         let ml_files, mli_files, others = separate_ocaml_files entries in
         let ocaml_files = group_ocaml_files path ml_files mli_files in
         let subdirs =
           List.filter_map
             ~f:(fun entry ->
               if is_dir ~dir (path / entry) then Some (path / entry) else None)
             others
         in
         collect_files (paths @ subdirs) (acc @ ocaml_files))
  in
  collect_files [ path ] []
;;

(** [parse_file_info env file_info] parses the given [file_info] in the environment [env]. It returns a list of [parse_result] records containing the location, module path, comments, contents, and file type for each parsed item.

    @param env The environment in which the function is executed.
    @param file_info The file information to be parsed.
    @param unit
    @return A list of [parse_result] records containing information about the parsed items. *)
let parse_file_info (type a) env (file_info : a file_info) =
  let module_name =
    let name_without_ext, _ = Filename.split_extension file_info.file_name in
    String.capitalize name_without_ext
  in
  (* traverse ~doc payload module_name ocaml_source *)
  match file_info.file_type with
  | Mli -> parse env file_info.file_name Interface module_name
  | Ml -> parse env file_info.file_name Implementation module_name
;;

(** [parse_module_info module_info] parses the given [module_info]. It returns a list of [parse_result] records containing the location, module path, comments, contents, and file type for each parsed item.

    @param module_info The module information to be parsed.
    @return A pair of (unit -> parse_result list) option * (unit -> parse_result list) option thunks containing information about the parsed items. This is done to seperate use of the non thread safe lexer code with the thread safe traverse function*)
let parse_module_info dir module_info =
  let mli_results =
    match module_info.mli_file with
    | Some mli_file_info ->
      let path = module_info.module_path in
      Printf.printf "mli %s" path;
      Some (parse_file_info Eio.Path.(dir / module_info.module_path) mli_file_info)
    | None -> None
  in
  let ml_results =
    match module_info.ml_file with
    | Some ml_file_info ->
      Some (parse_file_info Eio.Path.(dir / module_info.module_path) ml_file_info)
    | None -> None
  in
  mli_results, ml_results
;;

(** [format_parse_result parse_result] formats the given [parse_result] into a string.

    @param parse_result The parse result to be formatted.
    @return A formatted string containing the parse result information. *)
let format_parse_result (parse_result : parse_result) : string * string =
  let ocaml_source_str =
    match parse_result.ocaml_source with
    | Interface -> "Interface"
    | Implementation -> "Implementation"
  in
  let metadata =
    Printf.sprintf
      "(** \nLocation: %s\nModule Path: %s\nOCaml Source: %s\n*)\n\n"
      parse_result.location
      parse_result.module_path
      ocaml_source_str
  in
  let comments =
    if not @@ List.is_empty parse_result.comments
    then "(**\n" ^ String.concat ~sep:"\n" parse_result.comments ^ " *)"
    else ""
  in
  metadata, comments ^ "\n" ^ parse_result.contents
;;
