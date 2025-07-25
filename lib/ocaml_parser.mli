(** OCaml source‐code inspector.

    {e Ocaml_parser} offers a minimal, effect-free API to lex, parse and
    traverse *.ml* / *.mli* files.  The goal is to extract, for every
    top-level item, both its syntactical representation and the
    documentation comments attached to it.  The resulting records are
    convenient building blocks for downstream consumers such as search
    indices, documentation generators, or chat-bot context windows.

    A typical workflow is:

    • Use {!val:collect_ocaml_files} to discover OCaml files in a source
      tree.
    • Feed each {!type:module_info} to {!val:parse_module_info}.  The
      function returns {!type:traverse_input} thunks – one for the
      implementation file, one for the interface – so that the expensive
      lexing step can be isolated from the cheaper AST traversal.
    • Call {!val:traverse} on each input to obtain a list of
      {!type:parse_result} describing every top-level binding.

    Internally the module relies on {e ppxlib}'s {!module:Ppxlib.Parse}
    parser and {!module:Ppxlib.Ast_traverse} visitors but all
    complexity is hidden behind the straightforward record types
    defined below. *)

type ocaml_source =
  | Interface
  | Implementation
  (** Kind of OCaml source file: [.mli] interface or [.ml] implementation. *)

(** Metadata produced by {!val:traverse}.  All offsets are 1-based and
    relative to the beginning of [file].

    • [location]  – Human-readable “File … line … character …” string
      mirroring the compiler style.
    • [module_path] – Dotted path of nested modules (e.g. "Foo.Bar").
    • [comments] – List of docstrings (`(** … *)`) associated with the
      item.
    • [contents] – Raw source code snippet for the item.
    • [line_start]/[char_start]/[line_end]/[char_end] – Precise span of
      [contents] inside [file].
    • [ocaml_source] – Whether the snippet comes from an interface or an
      implementation file. *)
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

(** Bundle of data required by {!val:traverse}.  A value of this type is
    {b not} expensive to allocate – the heavyweight parsing work has
    already happened.  See {!val:parse} and {!val:parse_file_info} for
    constructors. *)
type traverse_input

(** [traverse input] walks the AST contained in [input] and returns every
    top-level binding as a {!type:parse_result}.

    The function is {e deterministic} and does not allocate global
    resources – it can safely be called in parallel across domains. *)
val traverse : traverse_input -> parse_result list

(** [parse dir file kind module_name] lexes and parses [file] (looked up
    relative to [dir]), yielding a {!type:traverse_input} that can later
    be given to {!val:traverse}.

    The function reads the whole file in memory – callers should avoid
    feeding multi-megabyte artefacts. *)
val parse : Eio.Fs.dir_ty Eio.Path.t -> string -> ocaml_source -> string -> traverse_input

type _ file_type =
  | Mli : mli file_type
  | Ml : ml file_type

and mli = MLI

and ml =
  | ML
  (** Phantom types used to tag interface / implementation files at the
    type level. *)

(** A source file bundled with its kind at the type level. *)
type 'a file_info =
  { file_type : 'a file_type
  ; file_name : string
  }

(** All the files backing a single compilation unit. *)
type module_info =
  { mli_file : mli file_info option
  ; ml_file : ml file_info option
  ; module_path : string
  }

(** [parse_module_info dir t] parses [t.ml] and/or [t.mli] (when present)
    and returns a pair of optional {!type:traverse_input}.  The parsing
    step is performed immediately so that the returned thunks are cheap
    to compute in parallel. *)
val parse_module_info
  :  Eio.Fs.dir_ty Eio.Path.t
  -> module_info
  -> traverse_input option * traverse_input option

(** [collect_ocaml_files dir subdir] recursively explores [subdir] and
    returns metadata for every module found (.ml / .mli files).

    The traversal continues in sub-directories that are not OCaml source
    files.  Errors (permission denied, broken symlinks…) are turned into
    [Error msg]. *)
val collect_ocaml_files
  :  Eio.Fs.dir_ty Eio.Path.t
  -> string
  -> (module_info list, string) result

(** [format_parse_result parse_result] formats the given [parse_result] into a string.
    @param parse_result The parse result to be formatted.
    @return A formatted string containing the parse result information. *)
val format_parse_result : parse_result -> string * string
(** [format_parse_result r] converts [r] into a pair 
    [(header, body)] ready to be saved to disk or passed to downstream
    systems.  [header] contains metadata in an OCaml comment while
    [body] concatenates the original doc-comments and code snippet. *)
