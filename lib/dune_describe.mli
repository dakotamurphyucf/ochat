(** Query Dune build metadata using [dune describe].

    The functions in this module run the following two Dune commands and turn
    their Canonical‐S-expression (csexp) output into strongly-typed OCaml
    values:

    {v
      dune describe external-lib-deps --format csexp
      dune describe --format csexp
    v}

    The high-level entry-point is {!val:run}.  It returns a
    {!type:project_details} value that contains, for every local library and
    executable in the current workspace:
    • the direct local and external library dependencies;
    • the modules that make up the component and their source locations;
    • the external opam packages required at link-time.

    All paths returned by the API point to the source tree.  Dune’s internal
    _build context prefix is removed for convenience.

    If you need the raw, low-level representation emitted by Dune you can use
    the nested {!module-Deps} and {!module-Item} sub-modules, but most users
    should find {!type:project_details} sufficient.

    {1 Example}

    {[
      Eio_main.run @@ fun env ->
      let details = Dune_describe.run env in
      List.iter details.local_libraries ~f:(fun lib ->
        Format.printf "library %s depends on %a\n"
          lib.name
          (Format.pp_print_list Format.pp_print_string)
          lib.external_dependencies)
    ]}
*)

(** {1 Raw representations}
    The sub-modules below mirror the exact structure returned by the two Dune
    commands so that nothing is lost in translation.  They are mainly intended
    for advanced use-cases. *)

open Core

module Deps : sig
  (** Result of [dune describe external-lib-deps]. *)

  type deps_library = private
    { names : string list
    ; extensions : string list
    ; package : string option
    ; source_dir : string
    ; external_deps : (string * string) list
    ; internal_deps : (string * string) list
    }

  type deps_executable = private
    { names : string list
    ; extensions : string list
    ; package : string option
    ; source_dir : string
    ; external_deps : (string * string) list
    ; internal_deps : (string * string) list
    }

  type deps_item =
    | Library of deps_library
    | Executables of deps_executable

  type deps = Default of deps_item list

  type deps_project_info = private
    { libraries : deps_library String.Map.t
    ; executables : deps_executable list
    }

  (** Summarise the flat [deps_item] list into convenient lookup tables. *)
  val extract_deps_project_info : deps_item list -> deps_project_info
end

module Item : sig
  (** Result of [dune describe]. *)

  type module_ = private
    { name : string
    ; impl : string option
    ; intf : string option
    ; cmt : string option
    ; cmti : string option
    }

  type library = private
    { name : string
    ; uid : string
    ; requires : string list
    ; local : bool
    ; source_dir : string
    ; modules : module_ list
    ; include_dirs : string list
    }

  type executable = private
    { names : string list
    ; requires : string list
    ; modules : module_ list
    ; include_dirs : string list
    }

  type item =
    | Library of library
    | Root of string
    | Build_context of string
    | Executables of executable

  type project_info = private
    { libraries : library String.Map.t
    ; local_libraries : string list
    ; external_libraries : string list
    ; root : string
    ; build_context : string
    ; executables : executable list
    }

  (** Aggregate the flat [item] list into structured project data. *)
  val extract_project_info : item list -> project_info
end

(** {1 High-level API} *)

type module_info =
  { module_name : string
  ; impl : string option
  ; intf : string option
  }

(** A compiled executable with its direct dependencies. *)

type executable_info =
  { names : string list (** All executable names produced by the stanza. *)
  ; local_dependencies : string list (** Depends-on local libraries. *)
  ; external_dependencies : string list (** Depends-on external libraries. *)
  ; required_external_deps : string list
    (** External libraries that must be present at link-time, as reported by
            [dune describe external-lib-deps]. *)
  ; modules : module_info list (** Source modules making up the executable. *)
  }

(** A local library defined in the workspace. *)

type local_lib_info =
  { name : string
  ; local_dependencies : string list (** Local libraries depended on. *)
  ; external_dependencies : string list (** External library dependencies. *)
  ; required_external_deps : string list (** External deps required at link-time. *)
  ; modules : module_info list (** Implementation and interface files. *)
  ; source_dir : string (** Absolute path to the library’s source directory. *)
  }

(** Consolidated information for the whole workspace. *)

type project_details =
  { local_libraries : local_lib_info list
  ; executables : executable_info list
  }

(** See {!Eio.Stdenv.process_mgr}.  We demand an object that at least exposes a
    [process_mgr] method; any extra capabilities are ignored. *)

(** [run env] spawns the two Dune commands under [env] and returns a
    {!project_details} value.  An exception is raised if either command exits
    with a non-zero status or if their output cannot be parsed. *)
val run
  :  < process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ; .. >
  -> project_details
