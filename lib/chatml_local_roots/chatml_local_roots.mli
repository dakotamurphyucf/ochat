open Core
module Local = Chatml_local_source

(** Host-anchor dependency directory selection. Native IO belongs to Local;
    locator normalization is pure. No project decoder, compiler, registry,
    process/network authority or global filesystem fallback. *)
module Limits : sig
  type t

  val create
    :  max_locator_bytes:int
    -> max_locator_components:int
    -> max_anchors:int
    -> max_retained_roots:int
    -> max_work_steps:int
    -> t Or_error.t

  (** Literal configured ceilings, not consumed accounting. Host composition
      uses these to construct smaller remaining allowances without resetting
      aggregate limits or raising any backend ceiling. *)
  val locator_bytes : t -> int

  val locator_components : t -> int
  val anchors : t -> int
  val retained_roots : t -> int
  val work_steps : t -> int
end

module Error : sig
  type code =
    | Invalid_locator
    | Invalid_anchor
    | Duplicate_anchor
    | Outside_grants
    | Foreign_cursor
    | Busy
    | Closed
    | Limit_exceeded
    | Accounting_overflow
    | Filesystem
  [@@deriving equal, sexp_of]

  type t

  val code : t -> code
  val filesystem : t -> Local.Error.t option

  (** At most 256 bytes of requested locator; never native exception/backtrace or
      a complete host grant inventory. Caller owns declaration diagnostic context. *)
  val locator_prefix : t -> string option
end

module Anchor : sig
  (** Explicit host mapping from an absolute lexical native-path prefix to an
      already-open admitted empty-path directory capability. This is host policy,
      not a source-authored permission grant or proof that a pathname still names
      that directory after rename. Prefix/root binding comes from trusted host
      configuration; no lstat/realpath precheck or reopening occurs here.
      [max_path_components] is explicit native source-member/subroot policy;
      locator normalization bounds do not silently select this ceiling. *)
  type t

  val create
    :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
    -> host_prefix:string
    -> max_path_components:int
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end

module Cursor : sig
  (** Affine to one live resolver attempt. Holds the selected lexical absolute
      location AND the retained native owner/identity. Relative dependencies are
      based on this declaring package location, not cwd or importer source scope.
      Cursor cannot escape its bracket as a usable host capability. *)
  type t

  val owner : t -> Local.t
  val identity : t -> Local.Directory_identity.t
end

module Accounting : sig
  type t

  (** Acquired descriptor owners, including anchors and roots whose later
      identity/cursor admission failed. All remain switch-owned until attempt close. *)
  val retained_roots : t -> int

  val work_steps : t -> int
end

type t

(** Acquire each explicit anchor via Local.open_root ownership and keep every
    acquired route owner alive until joined attempt cleanup. This prevents inode
    reuse while native identity is used for catalog alias comparison. Maximum
    retained roots counts descriptor owners, including anchors, even when two
    owners have the same directory identity. Exact-route reuse acquires no owner.
    Reserve bounded temporary component ownership before opening. No descriptor/anchor/cursor survives as a usable capability
    after callback return. Callback/checkpoint exceptions propagate unchanged.
    Anchor admission and acquisition traverse a bounded list without nesting
    an input-sized stack of with_root callbacks. Duplicate normalized prefixes
    reject before acquisition; longest component-prefix selection is deterministic.
    Resolver state is Eio-fiber confined, not shared across domains.
    Resolver methods run sequentially; concurrent/reentrant calls reject Busy,
    rather than holding a lock through a checkpoint or silently deadlocking. *)
val with_anchors
  :  Anchor.t list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> f:(t -> ('a, 'error) Result.t)
  -> ('a, [ `Roots of Error.t | `Callback of 'error ]) Result.t

(** Absolute locators select the longest explicitly granted component-prefix.
    Relative locators append to [base]'s own lexical location; None rejects
    relative input. Normalize slash components and '.'/'..' with bounds before IO.
    Input and every intermediate normalized locator obey byte/component caps.
    Parent components are erased lexically, never passed to openat. Attempts to
    traverse above the absolute namespace root reject. The normalized target must
    fall beneath a grant; a sibling in another explicitly admitted anchor is valid.
    No implicit ancestor/cwd/HOME/PATH/global-install authority is introduced.

    Cancelled lexical components are not opened; this is lexical locator semantics,
    not symlink-aware POSIX realpath behavior. Raw declaration spelling remains
    separately captured; normalization is not a source-ID/alias rewrite.
    Open only the selected anchor-relative validated components through Local;
    links/non-directories fail at kernel acquisition. Different anchor labels that
    reach the same retained directory compare through native identity, not strings.
    Declaration-member/environment/effective-namespace agreement is still required
    before catalog capture sharing. Repeated exact routes reuse an acquired owner
    within the attempt; fresh attempts acquire current target objects anew. *)
val resolve
  :  t
  -> base:Cursor.t option
  -> locator:string
  -> checkpoint:(unit -> unit)
  -> (Cursor.t, Error.t) Result.t

val accounting : t -> Accounting.t
