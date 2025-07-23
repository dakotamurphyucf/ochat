(* Generate CSV listing of every module in workspace (libraries and executables).
   The output is written to [out/modules.csv] with columns:
   file,module,owner_kind,owner_name,has_intf
   where owner_kind ∈ {library,executable}.
   The [file] column uses the source path relative to the repository root
   ("_build/default/" prefix stripped).

   Implementation strategy: we invoke [dune describe workspace] with
   [--lang=0.1 --format=sexp] and parse the resulting S-expression using
   Sexplib.  The structure contains repeated stanzas whose first atom is
   either "library" or "executables".  We only need a subset of the
   information: the owner name and, inside the nested association list,
   the list of modules plus their [impl]/[intf] paths.

   The parser is intentionally tolerant – it pattern-matches on the
   minimal shapes required and ignores everything else.  This keeps the
   implementation independent from dune’s internal representation details.
*)

open Sexplib

let run cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do Buffer.add_channel buf ic 1024 done with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Buffer.contents buf
  | _ -> failwith ("command failed: " ^ cmd)

module Csv = struct
  let sep = ','

  let escape s =
    if String.contains s sep || String.contains s '"' then
      "\"" ^ String.escaped s ^ "\""
    else
      s

  let row cells oc =
    let line = String.concat (String.make 1 sep) (List.map escape cells) in
    output_string oc line; output_char oc '\n'
end

(* Extract [(name <id>)] association from an alist *)
let rec find_in_alist key = function
  | [] -> None
  | Sexp.List (Sexp.Atom k :: v :: _) :: _ when k = key -> Some v
  | _ :: tl -> find_in_alist key tl

let get_name alist =
  match find_in_alist "name" alist with
  | Some (Atom n) -> n
  | _ -> "<unknown>"

let get_names alist =
  match find_in_alist "names" alist with
  | Some (List lst) -> List.filter_map (function Sexp.Atom s -> Some s | _ -> None) lst
  | _ -> []

let parse_modules mods : (string * string option) list =
  (* [mods] is a list whose elements represent module records.  We extract
     the [impl] path (mandatory) and optional [intf] path. *)
  List.filter_map
    (function
      | Sexp.List ((_ :: fields) as _m) ->
          let impl =
            match find_in_alist "impl" fields with
            | Some (Sexp.List [ Sexp.Atom path ]) -> Some path
            | Some (Sexp.Atom path) -> Some path
            | _ -> None
          in
          let intf =
            match find_in_alist "intf" fields with
            | Some (Sexp.List [ Sexp.Atom path ]) -> Some path
            | Some (Sexp.Atom path) -> Some path
            | _ -> None
          in
          Option.map (fun p -> (p, intf)) impl
      | _ -> None)
    mods

let strip_build_prefix path =
  let prefix = "_build/default/" in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else
    path

let () =
  let sexp_str = run "dune describe workspace --lang=0.1 --format=sexp" in
  let sexp = Sexp.of_string ("(" ^ sexp_str ^ ")") in
  let oc =
    let out_dir = Filename.concat (Sys.getcwd ()) "out" in
    (try Unix.mkdir out_dir 0o755 with Unix.Unix_error (Unix.EEXIST,_,_) -> ());
    open_out (Filename.concat out_dir "modules.csv")
  in
  Csv.row [ "file"; "module"; "owner_kind"; "owner"; "has_intf" ] oc;
  let modules_written = ref 0 in

  let rec traverse = function
    | [] -> ()
    | Sexp.List (Atom "library" :: List alist :: _) :: tl ->
        let lib_name = get_name alist in
        (* find modules list inside this stanza *)
        (match find_in_alist "modules" alist with
         | Some (Sexp.List mods) ->
             parse_modules mods
             |> List.iter (fun (impl, intf_opt) ->
                    let mod_name = Filename.(basename impl |> chop_extension |> String.capitalize_ascii) in
                    Csv.row
                      [ strip_build_prefix impl;
                        mod_name;
                        "library";
                        lib_name;
                        (string_of_bool (Option.is_some intf_opt)) ]
                      oc;
                    incr modules_written)
         | Some _ -> ()
         | None -> ());
        traverse tl
    | Sexp.List (Atom "executables" :: List alist :: _) :: tl ->
        let exe_names = get_names alist |> String.concat "+" in
        (match find_in_alist "modules" alist with
         | Some (Sexp.List mods) ->
             parse_modules mods
             |> List.iter (fun (impl, intf_opt) ->
                    let mod_name = Filename.(basename impl |> chop_extension |> String.capitalize_ascii) in
                    Csv.row
                      [ strip_build_prefix impl;
                        mod_name;
                        "executable";
                        exe_names;
                        (string_of_bool (Option.is_some intf_opt)) ]
                      oc;
                    incr modules_written)
         | Some _ -> ()
         | None -> ());
        traverse tl
    | _ :: tl -> traverse tl
  in
  (match sexp with List lst -> traverse lst | _ -> ());

  (* If catalogue is still empty (only header written), fall back to a
     simplistic git-based scan so that the CSV is never empty. *)
  if !modules_written = 0 then begin
    Printf.printf "Sexp parse yielded no modules – falling back to git ls-files.\n";
    let files =
      run "git ls-files '*.ml' '*.mli'"
      |> String.split_on_char '\n'
      |> List.filter (fun s -> s <> "")
    in
    let ml_files = List.filter (fun f -> Filename.extension f = ".ml") files in
    List.iter
      (fun ml ->
        let base = Filename.chop_extension ml in
        let mli = base ^ ".mli" in
        let has_intf = List.mem mli files in
        let mod_name = Filename.basename base |> String.capitalize_ascii in
        Csv.row [ ml; mod_name; ""; ""; string_of_bool has_intf ] oc;
        incr modules_written)
      ml_files;
    ()
  end;

  close_out oc;
  Printf.printf "Module catalogue written to out/modules.csv\n";

