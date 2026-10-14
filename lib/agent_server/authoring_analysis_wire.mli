open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Ledger = Chatml_authoring_context
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis

(** PRIVATE bounded projection of an actual checked result into a portable
    Analyze response. The compiler-owned result and native source sites remain
    in the lower cache; text that cannot be represented verbatim is visibly
    escaped, with any omitted tail reported by the wire truncation flags.
    Nothing here grants source access or checks an actor. *)
module Error : sig
  type t =
    | Invalid_stamp
    | Invalid_site
    | Wire_limit
    | Projection_limit
end

(** PRIVATE prefix visitor used by failed-analysis syntax projection. It stops
    at the first actual omitted site; no callback or work is spent traversing
    the remaining suffix after the cap. Empty units still cost checkpoints. *)
module Syntax_prefix : sig
  val collect
    :  max_sites:int
    -> units:'unit list
    -> sites:('unit -> 'site list)
    -> checkpoint:(unit -> unit)
    -> convert:('unit -> 'site -> ('row, 'error) result)
    -> ('row list * bool, 'error) result
end

(** Check the actual host's response byte ceiling before candidate publication;
    protocol construction has already enforced its own fixed aggregate cap. *)
val within_host_limit
  :  P.Authoring.Analysis.Analyze.t
  -> max_bytes:int
  -> (unit, Error.t) result

val project_capture
  :  expected:P.Authoring.Context.Expected.t
  -> stamp:Project.Project_stamp.t
  -> (A.Capture.t, Error.t) result

val document_capture
  :  contexts:Ledger.t
  -> expected:P.Authoring.Context.Expected.t
  -> source:Chatml_source.t
  -> script:Document.Script.t
  -> candidate:Document.Candidate.t
  -> (A.Capture.t, Error.t) result

val project
  :  contexts:Ledger.t
  -> expected:P.Authoring.Context.Expected.t
  -> stamp:Project.Project_stamp.t
  -> checked:Project.Checked.t
  -> snapshot:A.Snapshot_id.t option
  -> policy:Project.Policy.t
  -> checkpoint:(unit -> unit)
  -> (A.Analyze.t, Error.t) result

(** [input_sources] must be the actual OCH346 Parsed_capture association for
    this selected Script. Only physical parser-input membership permits an
    authored projection; external origins stay unresolved. *)
val document
  :  contexts:Ledger.t
  -> expected:P.Authoring.Context.Expected.t
  -> source:Chatml_source.t
  -> script:Document.Script.t
  -> candidate:Document.Candidate.t
  -> input_sources:(Chatmd_parser_input.t * Chatml_source.t) list
  -> snapshot:A.Snapshot_id.t option
  -> policy:Project.Policy.t
  -> checkpoint:(unit -> unit)
  -> (A.Analyze.t, Error.t) result
