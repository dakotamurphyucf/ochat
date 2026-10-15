open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Captured = Chatml_source
module Markdown = Prompt.Chat_markdown
module Selection = Authoring_script_selection

(** One immutable finite set of script evidence within the SAME retained
    Document capture head. It owns no source reader, callback registry or mutable
    cache. The sole Core owner publishes or retires the whole set with its parsed
    capture and lower analysis caches. A client script ID is lookup text, not a
    row key or an authority grant. *)

module Error : sig
  type t =
    | Foreign_capture
    | Invalid_row
    | Row_limit
    | Retained_byte_limit
    | Work_limit
  [@@deriving sexp_of]
end

type 'preparation_error failure =
  | Row_error of Error.t
  | Preparation_error of 'preparation_error

module Limits : sig
  type t

  (** Positive host-selected aggregate limits for ONE explicit Analyze/rebuild.
      The row count includes metadata-only and unavailable declarations. Source
      bytes count every retained Build source and decoded project source per row;
      canonical-input bytes count each selected Prepared input. Repeated source
      objects in different rows charge separately. These are measurable retained
      representation bounds, not an OCaml heap size or exact native-read count.
      The work allowance includes the preflight, row walks, comparison and the
      checkpoints passed into lower work. Each native Project attempt retains
      its own independently configured source/file/work cap. At most [max_rows]
      such attempts can occur, including failures. *)
  val create
    :  max_rows:int
    -> max_retained_source_bytes:int
    -> max_retained_canonical_input_bytes:int
    -> max_collection_work_steps:int
    -> t Or_error.t
end

module Row : sig
  type t

  type status =
    | Host_prepared of
        { source : Sources.Document_context.Script_capture.t
        ; selected : Selection.Selected.t
        }
    | Independently_selected of Selection.Selected.t
    | No_validated_build of P.Authoring.Analysis.Analyze.unavailable

  (** These constructors only assemble immutable candidates. [collect] checks
      the exact Parsed/declaration/declaring-source membership, the project
      receipt if present, and Host_prepared's physical script/source and
      Selected Build/contract association before admitting a row. Independent
      selection still requires its real host Policy proof and final same_now. *)
  val host_prepared
    :  parsed:Markdown.Parsed.t
    -> declaration:Markdown.Declaration.t
    -> declaring_source:Captured.t
    -> metadata:Sources.Document_context.Project_capture.t
    -> source:Sources.Document_context.Script_capture.t
    -> selected:Selection.Selected.t
    -> t

  val independently_selected
    :  parsed:Markdown.Parsed.t
    -> declaration:Markdown.Declaration.t
    -> declaring_source:Captured.t
    -> metadata:Sources.Document_context.Project_capture.t option
    -> selected:Selection.Selected.t
    -> t

  val no_validated_build
    :  parsed:Markdown.Parsed.t
    -> declaration:Markdown.Declaration.t
    -> declaring_source:Captured.t
    -> metadata:Sources.Document_context.Project_capture.t option
    -> reason:P.Authoring.Analysis.Analyze.unavailable
    -> t

  val parsed : t -> Markdown.Parsed.t
  val declaration : t -> Markdown.Declaration.t
  val declaring_source : t -> Captured.t
  val metadata : t -> Sources.Document_context.Project_capture.t option
  val status : t -> status
end

type t

val retained_count : t -> int

(** Exact current Document Inputs/source identity and physical capture used to
    construct the set. Core must check this before placing rows in its retained
    head; equal script IDs, digests or a later capture do not match. *)
val matches
  :  t
  -> inputs:Ledger.Document.Inputs.t
  -> capture:Sources.Document_context.Parsed_capture.t
  -> bool

(** Preflight the count and identity of ALL genuine Parsed declarations in the
    root and separately queued agent units before
    [prepare_one] can acquire any project source. Bind each declaring input to
    the exact captured [Parsed_capture.input_sources] association, then admit
    one typed row for each actual declaration under the SAME shared work and
    retained-byte ledgers. Row failure is independent only when [prepare_one]
    explicitly returns No_validated_build; an aggregate error returns no head.
    No silently truncated complete set or post-hoc path/ID join is admitted.
    Callback cancellation and unexpected exceptions preserve their original
    identity/backtrace. *)
val collect
  :  inputs:Ledger.Document.Inputs.t
  -> capture:Sources.Document_context.Parsed_capture.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> prepare_one:
       (Markdown.Declaration.t
        -> parsed:Markdown.Parsed.t
        -> declaring_source:Captured.t
        -> checkpoint:(unit -> unit)
        -> (Row.t, 'preparation_error) result)
  -> (t, 'preparation_error failure) result

(** Requires this physical capture and an actual declaration member. An absent
    row is unavailable until explicit Analyze/rebuild; this lookup never reads,
    parses or recomposes imports. *)
val find
  :  t
  -> capture:Sources.Document_context.Parsed_capture.t
  -> declaration:Markdown.Declaration.t
  -> checkpoint:(unit -> unit)
  -> (Row.t option, Error.t) result
