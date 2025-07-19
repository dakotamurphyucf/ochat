(** The [file_type] and [file_info] types are used to represent OCaml source files,
    and the [module_info] type represents the metadata of an OCaml module.
    The [collect_ocaml_files] function is used to collect OCaml source files from a directory and its subdirectories. *)

type ocaml_source =
  | Interface
  | Implementation

type parse_result =
  { location : string
  ; file : string
  ; module_path : string
  ; comments : string list
  ; contents : string
  ; ocaml_source : ocaml_source
  ; line_start : int
  ; char_start : int
  ; line_end : int
  ; char_end : int
  }

type traverse_input

val traverse : traverse_input -> parse_result list

(** [parse dir file ocaml_source module_name] parses the given [file] with
    the specified [ocaml_source] (either [Interface] or [Implementation]) and module name [module_name].
    It returns a list of [parse_result] records containing the
    location, module path, comments, contents, and file type for each parsed item.

    @param dir is the directory in which the function is executed.
    @param file The file to be parsed.
    @param ocaml_source The type of the file, either [Interface] or [Implementation].
    @param module_name The name of the module being parsed.
    @return
      A list of [parse_result] records containing information about the parsed items. *)
val parse : Eio.Fs.dir_ty Eio.Path.t -> string -> ocaml_source -> string -> traverse_input

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
    ]} *)

(** [module_info] is a record type representing the metadata of an OCaml module,
    combining the interface (mli) and implementation (ml) files. *)

type module_info =
  { mli_file : mli file_info option
  ; ml_file : ml file_info option
  ; module_path : string
  }

(** [parse_module_info module_info] parses the given [module_info]. It returns a list of [parse_result] records containing the location, module path, comments, contents, and file type for each parsed item.

    @param module_info The module information to be parsed.
    @return
      A pair of traverse_input option * traverse_input option thunks containing information about the parsed items. *)
val parse_module_info
  :  Eio.Fs.dir_ty Eio.Path.t
  -> module_info
  -> traverse_input option * traverse_input option

(** [collect_ocaml_files dir path] recursively collects OCaml source files from the directory specified by [path] and its subdirectories.

    @param dir is the root directory in which the function is executed.
    @param path is path containing the OCaml source files.
    @return
      a [Result.t] containing a list of [module_info] records, each representing the metadata of an OCaml module, including the interface (mli) and implementation (ml) files and the module path, or an error message if there was an issue reading the directory. *)

val collect_ocaml_files
  :  Eio.Fs.dir_ty Eio.Path.t
  -> string
  -> (module_info list, string) result

(** [format_parse_result parse_result] formats the given [parse_result] into a string.
    @param parse_result The parse result to be formatted.
    @return A formatted string containing the parse result information. *)
val format_parse_result : parse_result -> string * string
