open Core
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Graph = Chatml_dependency_graph
module Session = Chatml_dependency_session
module Check = Chatml_configured_check
module Parse = Chatml.Chatml_parse
module Recorder = Chatml_typechecker.Semantic

module Stage : sig
  type t =
    | Parse
    | Typecheck
  [@@deriving equal, sexp_of]
end

module Failure : sig
  (** Metadata rejection remains typed and separate from language failure. No
      partial facts survive either; cancellation/callback exceptions are raised
      unchanged. Diagnostics retain only their actual source sites. *)
  type kind =
    | Compiler of Stage.t
    | Sites of Chatml.Chatml_syntax_sites.Error.t
    | Recording of Recorder.Error.t

  type t

  val kind : t -> kind
  val diagnostic : t -> Chatml_diagnostic.t
end

module Unit : sig
  (** Compiler-domain ONLY. Borrowed raw schemes/weak cells, captured parser
      syntax and annotations; not frozen/domain-transfer/public query data.
      Contains no inference machine, require/checkpoint closure or recorder. *)
  type t

  val parsed : t -> Parse.Unit.t
  val interface : t -> Parse.Interface.t option
  val checked : t -> Check.Checked_unit.t
  val entrypoints : t -> Check.Entrypoint.t list
end

(** Successful actual graph/companion/selected-host checking boundary shared by
    Linked_program and semantic recording. One real checker; no scanner or
    provisional interfaces. Imported weak cells settle before success. *)
type t

(** Verifies the actual static contract fingerprint before parsing. Recording
    adds owned parser sites and bounded facts under the SAME checkpoint. None
    avoids semantic table/group/index/freeze work. Failure discards engine and
    recorder. Only successful completion permits Settled; this is not OCH212's
    certified immutable snapshot, nor does it promise successful resolver lowering. *)
val check
  :  ?recording:Recorder.Limits.t
  -> Prepared.t
  -> contract:Contract.t
  -> max_dependency_depth:int
  -> checkpoint:(unit -> unit)
  -> (t, Failure.t) Result.t

val graph : t -> Graph.t
val entry : t -> Unit.t
val checked : t -> Prepared.Module_id.t -> Unit.t option
val semantic_facts : t -> Recorder.Settled.t option
