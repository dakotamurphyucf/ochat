open Core
open Jsonaf.Export
module Csexp = Csexp.Make (Sexp)

(* Module for handling output from dune describe external-lib-deps *)
module Deps = struct
  type deps_item =
    | Library of deps_library
    | Executables of deps_executable

  and deps = Default of deps_item list

  and deps_library =
    { names : string list
    ; extensions : string list
    ; package : string option
    ; source_dir : string
    ; external_deps : (string * string) list
    ; internal_deps : (string * string) list
    }

  and deps_executable =
    { names : string list
    ; extensions : string list
    ; package : string option
    ; source_dir : string
    ; external_deps : (string * string) list
    ; internal_deps : (string * string) list
    }
  [@@deriving sexp, jsonaf]

  type deps_project_info =
    { libraries : deps_library String.Map.t
    ; executables : deps_executable list
    }

  let extract_deps_project_info (items : deps_item list) : deps_project_info =
    let libraries = ref String.Map.empty in
    let executables = ref [] in
    List.iter items ~f:(function
      | Library lib ->
        let uid = List.hd_exn lib.names in
        libraries := Map.set !libraries ~key:uid ~data:lib
      | Executables exe -> executables := exe :: !executables);
    { libraries = !libraries; executables = !executables }
  ;;
end

(* Module for handling output from dune describe *)
module Item = struct
  type item =
    | Library of library
    | Root of string
    | Build_context of string
    | Executables of executable

  and library =
    { name : string
    ; uid : uid
    ; requires : uid list
    ; local : bool
    ; source_dir : string
    ; modules : module_ list
    ; include_dirs : string list
    }

  and uid = string

  and module_ =
    { name : string
    ; impl : string option
    ; intf : string option
    ; cmt : string option
    ; cmti : string option
    }

  and executable =
    { names : string list
    ; requires : uid list
    ; modules : module_ list
    ; include_dirs : string list
    }
  [@@deriving sexp, jsonaf]

  type project_info =
    { libraries : library String.Map.t
    ; local_libraries : uid list
    ; external_libraries : uid list
    ; root : string
    ; build_context : string
    ; executables : executable list
    }

  let extract_project_info (items : item list) : project_info =
    let libraries = ref String.Map.empty in
    let local_libraries = ref [] in
    let external_libraries = ref [] in
    let root = ref None in
    let build_context = ref None in
    let executables = ref [] in
    List.iter items ~f:(function
      | Library lib ->
        libraries := Map.set !libraries ~key:lib.uid ~data:lib;
        if lib.local
        then local_libraries := lib.uid :: !local_libraries
        else external_libraries := lib.uid :: !external_libraries
      | Root r -> root := Some r
      | Build_context bc -> build_context := Some bc
      | Executables exe -> executables := exe :: !executables);
    let root =
      match !root with
      | Some r -> r
      | None -> failwith "No root found in item list"
    in
    let build_context =
      match !build_context with
      | Some bc -> bc
      | None -> failwith "No build context found in item list"
    in
    { libraries = !libraries
    ; local_libraries = !local_libraries
    ; external_libraries = !external_libraries
    ; root
    ; build_context
    ; executables = !executables
    }
  ;;
end

(* Define a record type to hold information about each module *)
type module_info =
  { module_name : string
  ; impl : string option
  ; intf : string option
  }
[@@deriving sexp, jsonaf]

(* Define a record type to hold information about each executable *)
type executable_info =
  { names : string list
  ; local_dependencies : string list
  ; external_dependencies : string list
  ; required_external_deps : string list (* New field *)
  ; modules : module_info list
  }
[@@deriving sexp, jsonaf]

(* Define a record type to hold information about each local library *)
type local_lib_info =
  { name : string
  ; local_dependencies : string list
  ; external_dependencies : string list
  ; required_external_deps : string list (* New field *)
  ; modules : module_info list
  ; source_dir : string
  }
[@@deriving sexp, jsonaf]

(* Define a record type to hold combined information about the project *)
type project_details =
  { local_libraries : local_lib_info list
  ; executables : executable_info list
  }
[@@deriving sexp, jsonaf]

(* Function to get the names of all local libraries and their dependencies *)
let get_local_libs_and_dependencies
  (project_info : Item.project_info)
  (deps_info : Deps.deps_project_info)
  : local_lib_info list
  =
  let { Item.libraries; local_libraries; root; build_context; _ } = project_info in
  let replace_build_context path =
    String.substr_replace_all path ~pattern:build_context ~with_:root
  in
  List.map local_libraries ~f:(fun local_uid ->
    match Map.find libraries local_uid with
    | Some local_lib ->
      let local_name = local_lib.name in
      let local_deps, external_deps =
        List.partition_map local_lib.requires ~f:(fun dep_uid ->
          match Map.find libraries dep_uid with
          | Some dep_lib when dep_lib.local -> Either.first dep_lib.name
          | Some dep_lib -> Either.second dep_lib.name
          | None -> Either.second dep_uid (* Assume external if not found *))
      in
      let required_external_deps =
        match Map.find deps_info.Deps.libraries local_name with
        | Some deps_lib -> List.map ~f:fst deps_lib.Deps.external_deps
        | None -> []
      in
      let modules =
        List.map local_lib.modules ~f:(fun m ->
          { module_name = m.Item.name
          ; impl = Option.map m.Item.impl ~f:replace_build_context
          ; intf = Option.map m.Item.intf ~f:replace_build_context
          })
      in
      { name = local_name
      ; local_dependencies = local_deps
      ; external_dependencies = external_deps
      ; required_external_deps
      ; modules
      ; source_dir = replace_build_context local_lib.source_dir
      }
    | None -> failwith ("Local library with UID " ^ local_uid ^ " not found"))
;;

(* Function to get the names of all executables and their dependencies *)
let get_executables_and_dependencies
  (project_info : Item.project_info)
  (deps_info : Deps.deps_project_info)
  : executable_info list
  =
  let { Item.libraries; executables; root; build_context; _ } = project_info in
  let replace_build_context path =
    String.substr_replace_all path ~pattern:build_context ~with_:root
  in
  List.map executables ~f:(fun exe ->
    let local_deps, external_deps =
      List.partition_map exe.Item.requires ~f:(fun dep_uid ->
        match Map.find libraries dep_uid with
        | Some dep_lib when dep_lib.Item.local -> Either.first dep_lib.Item.name
        | Some dep_lib -> Either.second dep_lib.Item.name
        | None -> Either.second dep_uid (* Assume external if not found *))
    in
    let required_external_deps =
      List.concat_map exe.Item.names ~f:(fun name ->
        match
          List.find deps_info.Deps.executables ~f:(fun deps_exe ->
            List.mem deps_exe.Deps.names name ~equal:String.equal)
        with
        | Some deps_exe -> List.map ~f:fst deps_exe.Deps.external_deps
        | None -> [])
    in
    let modules =
      List.map exe.Item.modules ~f:(fun m ->
        { module_name = m.Item.name
        ; impl = Option.map m.Item.impl ~f:replace_build_context
        ; intf = Option.map m.Item.intf ~f:replace_build_context
        })
    in
    { names = exe.Item.names
    ; local_dependencies = local_deps
    ; external_dependencies = external_deps
    ; required_external_deps
    ; modules
    })
;;

(* Function to get combined project details *)
let get_project_details (project_info : Item.project_info) dep_project_info
  : project_details
  =
  let local_libs_info = get_local_libs_and_dependencies project_info dep_project_info in
  let executables_info = get_executables_and_dependencies project_info dep_project_info in
  { local_libraries = local_libs_info; executables = executables_info }
;;

(* Function to run the dune describe command and get project details *)
let run env =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let output =
    Eio.Process.parse_out
      proc_mgr
      Eio.Buf_read.take_all
      [ "dune"; "describe"; "external-lib-deps"; "--format"; "csexp" ]
  in
  match Csexp.parse_string output with
  | Error (_, msg) -> failwith msg
  | Ok x ->
    let (Deps.Default items) = [%of_sexp: Deps.deps] x in
    let deps_info = Deps.extract_deps_project_info items in
    let output =
      Eio.Process.parse_out
        proc_mgr
        Eio.Buf_read.take_all
        [ "dune"; "describe"; "--format"; "csexp" ]
    in
    (match Csexp.parse_string output with
     | Error (_, msg) -> failwith msg
     | Ok x ->
       let items = [%of_sexp: Item.item list] x in
       let project_info = Item.extract_project_info items in
       let project_details = get_project_details project_info deps_info in
       project_details)
;;
