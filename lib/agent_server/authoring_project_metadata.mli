open! Core
module Ledger = Chatml_authoring_context
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Capture = Authoring_source_host.Document_context.Project_capture

(** Private, comparison-only current policy for one project declaration captured
    during an explicit authorized Analyze/rebuild and retained in the SAME Core
    Document capture head. It can inspect the authentic just-captured candidate
    before publication, or the same current receipt during assistance. No
    parallel registry, source acquisition, project decode, script Build or read
    grant is created here. *)
module Policy : sig
  type t

  (** [capture] is either the authentic just-captured candidate during explicit
      Analyze or the receipt borrowed from live Core.Current.project_capture.
      The caller proves [Project_capture.matches] against the actual Parsed and
      member Declaration, plus current Document Inputs/generation. This callback
      rechecks both actors, declaration intent and the owner-approved
      route/current root grant. It is bounded and nonyielding. Equal path or
      digest never establishes custody; no alternate capture may be returned. *)
  val create
    :  authorize_now:
         (original_actor:Operator_authorization.t
          -> actor:Operator_authorization.t
          -> current:Ledger.Document.Inputs.t
          -> parsed:Parsed.t
          -> declaration:Declaration.t
          -> capture:Capture.t
          -> bool)
    -> t

  (** Call once before using the decoded project and again after all yielding
      work. Core separately verifies actor/source/context generation, the
      retained Parsed head and its exact capture identity at publication. *)
  val authorize_now
    :  t
    -> original_actor:Operator_authorization.t
    -> actor:Operator_authorization.t
    -> current:Ledger.Document.Inputs.t
    -> parsed:Parsed.t
    -> declaration:Declaration.t
    -> capture:Capture.t
    -> bool
end
