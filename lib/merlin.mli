(** High-level wrapper around the {!ocamlmerlin}(1) CLI.

    The module offers just enough surface to build editor-like features
    – *find occurrences* and *auto-completion* – while delegating all
    heavyweight analysis to Merlin itself.

    All I/O is performed with {!Eio.Process}, hence every call must run
    inside an Eio fibre:
    {[
      Eio_main.run @@ fun env ->
        let merlin = Merlin.create () in
        (* … *)
    ]} *)

(** Handle to a Merlin session (see {!create}). *)
type t

(** [create ?server ?bin_path ?dot_merlin ()] prepares a new session.

    @param server     when [true] (default), use the persistent
                       `ocamlmerlin server` mode.
    @param bin_path   path to the executable (default "ocamlmerlin").
    @param dot_merlin override the name of the configuration file. *)
val create : ?server:bool -> ?bin_path:string -> ?dot_merlin:string -> unit -> t

(** [add_context t code] appends [code] (plus a " ;; " terminator) to
    the session context so that subsequent queries see previous
    definitions. *)
val add_context : t -> string -> unit

(* -------------------------------------------------------------------------
   Identifier occurrences
   ------------------------------------------------------------------------- *)

type ident_position =
  { id_line : int (** 1-based *)
  ; id_col : int (** 0-based *)
  }

type ident_reply =
  { id_start : ident_position
  ; id_end : ident_position
  }

(** [occurrences env ~pos t code] returns the ranges of every occurrence
    of the identifier at byte offset [pos] in [code]. *)
val occurrences
  :  < process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ; .. >
  -> pos:int
  -> t
  -> string
  -> ident_reply list

(** [abs_position code p] converts [p] into a 0-based absolute byte
    index inside [code]. *)
val abs_position : string -> ident_position -> int

(* -------------------------------------------------------------------------
   Completion
   ------------------------------------------------------------------------- *)

type kind =
  | CMPL_VALUE
  | CMPL_VARIANT
  | CMPL_CONSTR
  | CMPL_LABEL
  | CMPL_MODULE
  | CMPL_SIG
  | CMPL_TYPE
  | CMPL_METHOD
  | CMPL_METHOD_CALL
  | CMPL_EXN
  | CMPL_CLASS

type candidate =
  { cmpl_name : string
  ; cmpl_kind : kind
  ; cmpl_type : string
  ; cmpl_doc : string
  }

type reply =
  { cmpl_candidates : candidate list
  ; cmpl_start : int
  ; cmpl_end : int
  }

(** Empty reply – returned when Merlin cannot offer any suggestion. *)
val empty : reply

(** [complete env ?doc ?types ~pos t code] requests auto-completion at
    the byte offset [pos] in [code].

    @param doc   include documentation strings (default [false]).
    @param types include rich type information   (default [false]). *)
val complete
  :  < process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ; .. >
  -> ?doc:bool
  -> ?types:bool
  -> pos:int
  -> t
  -> string
  -> reply
