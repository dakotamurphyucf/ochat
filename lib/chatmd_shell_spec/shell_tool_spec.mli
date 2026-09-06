open! Core

(** Serializable declarations for tools backed by a named shell runtime. *)

type stdin_mode =
  | No_stdin
  | Optional_stdin
  | Required_stdin
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type rationale_mode =
  | No_rationale
  | Optional_rationale
  | Required_rationale
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type result_format =
  | Combined
  | Stdout
  | Structured_result
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type stream_mode =
  | Finalized
  | Sanitized
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type nonzero =
  | Result
  | Error
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type arguments_mode =
  | No_arguments
  | Optional_arguments
  | Required_arguments
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type arguments =
  { mode : arguments_mode
  ; min_count : int option
  ; max_count : int option
  ; max_item_bytes : int option
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type fixed_argument =
  | Literal of string
  | Secret_env of
      { name : string
      ; prefix : string
      }
  | Path of Path_expr.t
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type fixed_command =
  | Compact of string
  | Argv of
      { program : string option
      ; executable_ref : string option
      ; arguments : fixed_argument list
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type verification_mode = Verify_sha256
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type raw_shell =
  { executable : Path_expr.t
  ; arguments_before_script : string list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type script_file =
  { script : Path_expr.t
  ; interpreter : Path_expr.t option
  ; executable : bool
  ; fixed_arguments : string list
  ; verification : verification_mode
  ; max_source_bytes : int
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type mode =
  | Fixed of
      { command : fixed_command
      ; model_arguments : arguments
      }
  | Structured
  | Chain
  | Raw of raw_shell
  | Script_file of script_file
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { name : string
  ; description : string option
  ; runtime : string
  ; mode : mode
  ; stdin : stdin_mode
  ; rationale : rationale_mode
  ; result : result_format
  ; stream : stream_mode
  ; nonzero : nonzero
  ; source : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [parse_compact_attributes ~source attributes] converts attributes already
    parsed by the ChatMD parser into a compact or childless shell tool. It
    rejects duplicate, unknown, and mode-incompatible attributes.

    The ChatMD parser remains responsible for recognizing [tool] elements and
    parsing long-form children. The ChatMD shell declaration adapter converts
    those children directly into {!fixed_command} and {!arguments} values; it
    does not invoke a second XML parser. *)
val parse_compact_attributes
  :  source:Source_ref.t
  -> (string * string) list
  -> (t, Diagnostic.t list) result

(** [parse_declaration ~source ~attributes ~command ~arguments] converts a
    shell tool node already parsed by ChatMD. [command] and [arguments] are the
    optional typed long-form children. They are valid only for fixed mode. *)
val parse_declaration
  :  source:Source_ref.t
  -> attributes:(string * string) list
  -> command:fixed_command option
  -> arguments:arguments option
  -> (t, Diagnostic.t list) result
