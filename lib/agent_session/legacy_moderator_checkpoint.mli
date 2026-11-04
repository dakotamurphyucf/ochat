open! Core

(** Compatibility disposition only. [No_migration] does not validate a checkpoint
    or grant source, runtime or execution authority. Ordinary exact identity and
    compiler admission still apply. *)
type t =
  | No_migration
  | Rebuild_required
[@@deriving equal, sexp_of]

(** Recognize an actual prior Single checkpoint against the final legacy script
    in this restored immutable revision. Recognition requires its exact script ID
    and MD5 of retained source bytes, with no configured project or parser,
    program or content custody. A potential historical identity that does not
    match returns [Invalid_state]; it never becomes a current source identity.

    This performs no I/O, compilation, initialization or raw checkpoint rewriting.
    Inputs retain their existing artifact/checkpoint admission bounds. [poll]
    runs during script selection and around hashing; its exceptions, including
    cancellation, propagate. The caller must use a genuinely restored revision;
    constructing this value is not an authenticity or authorization proof. *)
val classify
  :  Prompt_revision.t
  -> checkpoint:Jsonaf.t option
  -> poll:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t
