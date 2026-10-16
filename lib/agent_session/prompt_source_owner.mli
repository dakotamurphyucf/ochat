open Core
module Receipt = Agent_store.Prompt_source_receipt

module Limits : sig
  type t [@@deriving equal]

  val create : max_path_components:int -> max_runtime_leases:int -> t Or_error.t
end

module Error : sig
  type code =
    | Invalid_binding
    | Foreign_receipt
    | Missing_live_root
    | Busy
    | Revoked
    | Closed
    | Lease_limit
    | Filesystem
    | Capture
    | Foreign_capture
    | Project_preparation
    | Document_capture
  [@@deriving equal, sexp_of]

  type t

  val code : t -> code
  val filesystem : Chatml_local_source.Error.t -> t
  val missing_live_root : unit -> t
  val foreign_receipt : unit -> t
  val capture_diagnostics : t -> Prompt_revision_builder.Diagnostic.t list option
  val project_error : t -> Chatml_project_prepare.Error.t option
  val document_error : t -> Chatmd_content_capture.Error.t option
  val sexp_of_t : t -> Sexp.t
end

module Policy : sig
  (** Shared actual read-policy cell, including the runtime lease count. The
      catalog reuses this SAME cell for unchanged intent before opening reload
      candidates. Closing/abandoning one owner never revokes this cell. No native
      permission is acquired by constructing policy metadata. *)
  type t

  val create : Receipt.t -> limits:Limits.t -> t
  val receipt : t -> Receipt.t

  (** Immutable ceilings for this admitted intent; reload must not silently reuse
      different requested ceilings. Checking compatibility acquires no resource. *)
  val accepts_limits : t -> limits:Limits.t -> bool

  val revoke : t -> unit
  val check : t -> (unit, Error.t) Result.t
end

(** Owner, policy and scope state are confined to one Eio switch domain.
    Do not transfer them across OCaml domains. Only immutable Capture/Receipt
    values may cross domains; independent runtime switches live on same domain. *)
type t

(** Host supplies CURRENT operator-admitted already-open empty directory; never
    reopen a root from stored receipt metadata. Local independently duplicates the
    admitted directory into [sw], before publication; no extra Eio directory
    descriptor is retained. Runtime leases clone this same Local directory. [policy] is the
    catalog's exact matching original-intent policy. Native acquisition uses
    Local's joined operations/cleanup; no ambient filesystem fallback. *)
val open_owned
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> sw:Eio.Switch.t
  -> policy:Policy.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val receipt : t -> Receipt.t
val policy : t -> Policy.t
val check : t -> (unit, Error.t) Result.t

(** Close ONLY this owner's Local descriptor, joining native operations. Idempotent,
    safely repeated by switch cleanup. Explicit close unregisters its cleanup
    hooks so closed owners do not accumulate in long-lived reload switches. Does
    not revoke shared policy/runtime
    clones. Caller joins capture scopes before explicit close. *)
val close : t -> unit

(** Reserve shared finite runtime lease allowance BEFORE effects, duplicate SAME
    retained directory, recheck policy after yield, install cleanup before result.
    Lease close releases allowance; all clones observe later shared revocation. *)
val lease : t -> sw:Eio.Switch.t -> checkpoint:(unit -> unit) -> (t, Error.t) Result.t

module Capture : sig
  type t

  val value : t -> Prompt_revision_builder.Capture.t
  val receipt : t -> Receipt.t

  (** Immutable host contract copied from the genuine definition at capture.
      This metadata grants no native source or execution authority. *)
  val root_invocation
    :  t
    -> Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t

  (** Immutable compiler ABI copied from the SAME actual definition as
      [root_invocation], before source capture can yield. Not execution
      authority and never selected by looking up a mutable catalog later. *)
  val tool_contract : t -> Chatml_compilation.Tool_contract.t

  module Route : sig
    (** Positive original-root member of an actual input from this capture. No
        native authority; permits207 to supply genuine file-member relationship. *)
    type t

    val member : t -> Source.Portable_path.t

    (** Relative locator is resolved from this ACTUAL declaring input's parent,
        within the original admitted root; absolute locator intent is preserved.
        [project] remains a separate package-relative member. No native access. *)
    val local_root
      :  t
      -> locator:string
      -> project:Source.Portable_path.t
      -> max_locator_bytes:int
      -> checkpoint:(unit -> unit)
      -> Chatml_project_prepare.Root.t Or_error.t
  end

  (** Actual parser-input identity, not a path/byte guess. Foreign input rejects;
      repeated identical occurrence is harmless. No native/source loads. *)
  val route
    :  t
    -> Chatmd_parser_input.t
    -> checkpoint:(unit -> unit)
    -> Route.t Or_error.t
end

module Scope : sig
  type t

  module Content_revision : sig
    (** Parser/content view bound to this live original-source Scope. This is
        a read prerequisite, not proof that a saved artifact was selected by
        Store: the selected Target/Source_binding must establish that first. *)
    type t

    (** Previously admitted captured/builtin content costs from this exact
        restored revision. The enclosing child projector seeds its one shared
        ledger before any new declaration or caller content read. *)
    val captured_accounting : t -> Chatmd_prompt_content.Accounting.t option
  end

  module Content_read : sig
    type t

    val contents : t -> string
    val input_bytes : t -> int

    (** Actual charged parser lookup, route and native-read stage units from
        this call. The enclosing projector debits this against its ONE
        aggregate allowance before processing another item. *)
    val work_steps : t -> int
  end

  module Origin_read : sig
    type t

    val origin : t -> Prompt.Chat_markdown.Content_origin.t
    val work_steps : t -> int
  end

  val check : t -> (unit, Error.t) Result.t

  (** Bind the restored revision's exact parser and document projection to
      this same original-source receipt. Require attached parser custody,
      artifact/source receipt equality and the projection's SAME Parsed.
      Missing/foreign/revoked source rejects before native I/O. This does not
      make an arbitrary [Prompt_revision.create] value an authenticated saved
      selection; the genuine Target/Source_binding checks that separately.
      The result expires with this Scope. *)
  val bind_content_revision
    :  t
    -> revision:Prompt_revision.t
    -> (Content_revision.t, Error.t) result

  (** Verify one exact parser-owned Basic against the bound revision under the
      physical Scope. This comparison-only origin lets a higher admitted
      remote port use the authored URL; it grants neither local nor network
      access. Lookup shares the caller's remaining aggregate work allowance. *)
  val content_origin
    :  t
    -> content:Content_revision.t
    -> item:Prompt.Chat_markdown.basic_content_item
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> (Origin_read.t, Error.t) result

  (** Resolve one exact parser-owned LOCAL Basic image/document occurrence through
      this Scope's retained original Local. [Content_revision] supplies the
      Parsed/projection association; foreign or model-created Basic items have
      no origin. A captured/builtin document cannot be reread here. Require its
      Range.input to belong to that Parsed and its
      logical member path to pass artifact-portable admission. Factor the
      existing local_source route and charged no-follow read. The positive
      byte/work limits and checkpoint apply before traversal/native read;
      the SAME work allowance covers lookup, route and native stage. The caller
      must pass the aggregate remaining allowance and debit observed bytes/
      work after success; this operation never resets the caller's ledger.
      Policy/closure/cancellation are rechecked after yielding work. No
      ambient cwd, materialized tree or path fallback. *)
  val read_dynamic_content
    :  t
    -> content:Content_revision.t
    -> item:Prompt.Chat_markdown.basic_content_item
    -> max_bytes:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> on_acquire:(unit -> unit)
    -> (Content_read.t, Error.t) result

  (** Pure same-original-admission check, sharing [prepare_sources]'s affinity and
      receipt validation. No anchor/backend acquisition. Policy, closure and
      cancellation observation match [check]; foreign captures reject even when
      no selected project request is needed. *)
  val check_capture : t -> Capture.t -> (unit, Error.t) Result.t

  (** Same actual bounded Builder walker, via Local retained backend. No second
      scanner or source fallback. Env supplies transient shell inspection only.
      Definition must match admitted receipt before any read. Caller exceptions
      preserve object/backtrace; policy checks bracket bounded checkpoints. *)
  val capture
    :  t
    -> env:Eio_unix.Stdenv.base
    -> definition:Prompt_definition.t
    -> preprocess:(string -> string)
    -> limits:Prompt.Chat_markdown.Limits.t
    -> checkpoint:(unit -> unit)
    -> (Capture.t, Error.t) Result.t

  (** Capture an exact parser-owned selected member through this SAME original
      receipt and retained Local. No new root permission or native reopen.
      Shares [capture]'s bounded walker, current policy/cancellation and joined
      operation lifecycle. The resulting Capture shares this admission's exact
      affinity; genuine leases accept it, foreign native admissions reject.
      Original Definition policy stays unchanged; executable invocation selection
      is a separate host preparation responsibility. *)
  val capture_entry
    :  t
    -> entry:Prompt_source_entry.t
    -> env:Eio_unix.Stdenv.base
    -> preprocess:(string -> string)
    -> limits:Prompt.Chat_markdown.Limits.t
    -> checkpoint:(unit -> unit)
    -> (Capture.t, Error.t) Result.t

  (** Capture rendered documents through this SAME retained Local. Reject foreign
      capture admission/receipt before any read. Derive contexts from the actual
      immutable Builder capture; no root/Local getter or artifact-path reopen.
      Shared policy/cancellation checks bracket caller checkpoints and delivery.
      Complete native/conversion/metadata receipt is immutable and charged once. *)
  val capture_documents
    :  t
    -> capture:Capture.t
    -> limits:Chatmd_prompt_content.Limits.t
    -> checkpoint:(unit -> unit)
    -> (Chatmd_content_capture.t, Error.t) Result.t

  (** SAME retained original root makes the genuine host anchor privately; no
      root/Local/Host getter. Capture must belong to this exact native admission
      (leases share affinity; reload newly opened owners do not). Additional roots
      and Git grants are explicitly host admitted. Actual217 one-attempt preparation
      returns only immutable source custody, with no tool/initializer admission.
      Check current shared policy after every yielding checkpoint and before result. *)
  val prepare_sources
    :  t
    -> capture:Capture.t
    -> requests:Chatml_project_prepare.Request.t list
    -> additional_anchors:Chatml_local_roots.Anchor.t list
    -> git_store:Chatml_git_host.Store.t option
    -> git_grants:Chatml_git_host.Grant.t list
    -> root_limits:Chatml_local_roots.Limits.t
    -> max_grants:int
    -> max_grant_bytes:int
    -> policy:Chatml_project_prepare.Policy.t
    -> limits:Chatml_project_prepare.Limits.t
    -> clock:_ Eio.Time.Mono.t
    -> checkpoint:(unit -> unit)
    -> (Chatml_project_prepare.Source_batch.t, Error.t) Result.t
end

(** Sequential capture lifetime only; Local owns native operation joining. A
    scope-owned capture worker is joined on callback return/exception/cancellation,
    since parser/user checkpoints may suspend after native reads. Reject Busy and
    poison the scope; close admission BEFORE cancel/join. Check current caller
    cancellation before worker admission and after completion before delivering a
    result, even when completion promises are already resolved. No root/Local/Host
    getters; escaped Scope rejects. Original exceptions/backtraces propagate. *)
val with_scope : t -> f:(Scope.t -> ('a, Error.t) Result.t) -> ('a, Error.t) Result.t
