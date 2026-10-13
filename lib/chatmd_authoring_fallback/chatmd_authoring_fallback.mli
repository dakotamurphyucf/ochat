open Core

(** Thin selected-ChatMD-script adapter for Chatml_authoring_fallback. Its
    bound parser extraction map is genuine; it never guesses authored locations
    from a path, suffix or displayed text. This module may link the selected
    script analysis library; the ChatML fallback core stays Eio-independent. *)
module Captured = Chatml_source

module Fallback = Chatml_authoring_fallback
module Script = Chatmd_authoring_analysis.Script
module Context = Chatml_authoring_context
module Input = Chatmd_parser_input

module Limits : sig
  type t

  (** Positive host ceilings for this request's selected document/input byte
      comparison, qualified source-ID bytes and association work. The host
      chooses them no higher than its already admitted document/parser caps.
      A separate core projection ledger later bounds the two map callbacks;
      total work is the sum of these finite adapter and core allowances. *)
  val create
    :  max_source_bytes:int
    -> max_identity_bytes:int
    -> max_work_steps:int
    -> t Or_error.t
end

module Error : sig
  type t =
    | Stale_document
    | Stale_selected_build
    | Foreign_authored_input
    | Work_limit

  val sexp_of_t : t -> Sexp.t
end

module Origin : sig
  type admission =
    | Ready of Fallback.Origin.t
    | Unavailable

  (** [current] is the host's actual current Document input, not a wire claim.
      The selected Script stamp must match its identity/generation/source, the
      separately selected contract's Prepared closure and exact entry source.
      [authored_input] must be the parser input containing the exact editable
      root bytes of [current]; imported declarations are unavailable for this
      request. A parser-owned inverse point lookup is exact only on one unique
      identity segment; forward projection of the entire replacement must
      return Exact_inline for that SAME input. All other outcomes return None
      through the trusted map and produce Source_map_unavailable. The exact
      byte comparisons poll every bounded chunk; [current]/parser input/source
      were already admitted under the host document/parser source-byte caps,
      so this step cannot traverse an unbounded native input. The host
      still maps the wire Expected context to [current], authorizes actor/source
      custody and rechecks currentness before response publication. *)
  val admit
    :  Script.t
    -> current:Context.Document.Inputs.t
    -> authored_input:Input.t
    -> authored_source:Captured.t
    -> selected:Fallback.Selected_contract.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (admission, Error.t) Result.t
end
