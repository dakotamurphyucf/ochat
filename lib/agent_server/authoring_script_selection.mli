open! Core

(** Shared private selected-script comparison policy for the semantic and early
    assistance siblings. This module neither captures imports nor parses a
    document. Current actor/source/root authority stays with the one authoring
    context owner, and a Selected value is never a capability. *)

module Ledger = Chatml_authoring_context
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration

module Limits : sig
  type t

  (** Positive limits on one contract admission. [max_identity_bytes] bounds
      encoded contract identity and conservative encoding allocation before
      either is built; [max_work_steps] bounds conversion/hash work through the
      supplied checkpoint. Host source/Build byte limits remain independent. *)
  val create : max_identity_bytes:int -> max_work_steps:int -> t Or_error.t
end

module Error : sig
  type t =
    | Identity_limit
    | Work_limit
    | Contract_mismatch

  val sexp_of_t : t -> Sexp.t
end

module Selected : sig
  type t

  (** Admit an ACTUAL host-selected Build and compiler contract before making
      them available to a nonyielding policy read. Standard contracts are
      rederived with the existing target selector; Native uses only the
      independently host-admitted actual contract. The canonical contract
      identity is bounded and checkpointed, then must match both Prepared's
      selected and required environment contract. Retain its fixed-size digest
      and Prepared fingerprint for cheap currentness comparison. This step may
      yield via [checkpoint] and must never run inside the final no-yield gate.
      Callback exceptions, including cancellation, preserve identity. *)
  val admit
    :  build:Chatml_project_build.t
    -> contract:Chatml.Chatml_compiler_contract.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) result

  val build : t -> Chatml_project_build.t
  val contract : t -> Chatml.Chatml_compiler_contract.t
  val prepared_fingerprint : t -> Chatml_source.Digest.t
  val compiler_contract : t -> Chatml_source.Digest.t

  (** Two fixed-size admitted digests. Source/environment/selection changes
      change Prepared fingerprint; a wire label or callback object identity
      alone cannot claim currentness. No serialization or source traversal. *)
  val equal_identity : t -> t -> bool
end

module Policy : sig
  type t

  (** One bounded, nonyielding current host lookup shared by both services.
      The arguments are current immutable Document Inputs, retained genuine
      Parsed value and an actual member of
      Parsed.declarations. The policy independently checks BOTH original and
      request actors against the current selected project/environment/catalog;
      it cannot fetch, parse, compose, compile, wait, or infer from a client ID
      or display path. [None] is genuinely no admitted selection. The semantic
      and assistance services still prove Parsed membership, selected entry
      body, source/grant currentness and their own result bounds. *)
  val create
    :  select_script:
         (original_actor:Operator_authorization.t
          -> actor:Operator_authorization.t
          -> current:Ledger.Document.Inputs.t
          -> parsed:Parsed.t
          -> declaration:Declaration.t
          -> Selected.t option)
    -> t

  val select
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> current:Ledger.Document.Inputs.t
    -> parsed:Parsed.t
    -> declaration:Declaration.t
    -> Selected.t option

  (** Repeat that same nonyielding host lookup after yielding work and compare
      the preadmitted fixed-size selection identity. No callback runs under the
      pure Core CAS gate. Missing/changed selection returns false; the caller
      rejects a stale result rather than silently downgrading selected facts. *)
  val same_now
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> current:Ledger.Document.Inputs.t
    -> parsed:Parsed.t
    -> declaration:Declaration.t
    -> selected:Selected.t
    -> bool
end
