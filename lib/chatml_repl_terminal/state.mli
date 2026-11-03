open! Core
module Inferred_bindings = Inferred_bindings

(** Bounded, transport-free terminal presentation state. The portable client
    remains the owner of exact pending requests and retry handles. *)

module Limits : sig
  type t

  val create
    :  max_source_bytes:int
    -> max_source_lines:int
    -> max_history_entries:int
    -> max_history_bytes:int
    -> max_notice_bytes:int
    -> (t, Agent_protocol.Error.t) result

  val max_source_bytes : t -> int
  val max_source_lines : t -> int
  val max_notice_bytes : t -> int
end

module Editor : sig
  type t

  type motion =
    | Left
    | Right
    | Home
    | End
  [@@deriving equal, sexp_of]

  (** Inserted and pasted text is literal source, including newlines and
      leading colons. Cursor and deletion boundaries are UTF-8 grapheme
      boundaries. A rejected edit leaves the old editor unchanged. *)
  val insert : t -> limits:Limits.t -> string -> (t, Agent_protocol.Error.t) result

  val backspace : t -> t
  val delete : t -> t
  val move : t -> motion -> t
  val source : t -> string
  val cursor_byte : t -> int

  (** The final line and the preceding source are exposed separately so an
      own-line [:submit] is never sent as ChatML source. They do not mutate the
      editor or infer a server result. *)
  val final_line : t -> string

  val source_before_final_line : t -> string

  (** Removes a recognized command line while retaining the preceding source
      and its terminating newline. *)
  val without_final_line : t -> t
end

type keyed_operation =
  | Create
  | Submit
  | Run_task
  | Reset
  | Close
[@@deriving equal, sexp_of]

type operation =
  | Keyed of keyed_operation
  | Inspect_type
  | References
  | Observe
  | Cancel
[@@deriving equal, sexp_of]

type activity =
  | Ready
  | Busy of operation
  | Uncertain of keyed_operation
  | Needs_reset
  | Closed
[@@deriving equal, sexp_of]

type t

module Submission_source : sig
  (** Captured append request and exact aggregate bytes. Only [suffix] goes on
      the wire; [combined] is the server's acknowledged prefix plus that suffix.
      Both are immutable and remain valid for exact-key retries. *)
  type t

  val suffix : t -> string
  val combined : t -> string
end

(** Server-acknowledged incomplete bytes, displayed read-only. The editor owns
    only the unsent suffix. Reset and a terminal submission clear this prefix. *)
val continuation_source : t -> string option

(** Validate exact prefix+suffix UTF-8, byte and line bounds before any request.
    Does not mutate the acknowledged prefix or consume the editor. *)
val submission_source
  :  t
  -> suffix:string
  -> (Submission_source.t, Agent_protocol.Error.t) result

(** Acknowledgement transition for the captured request. Need_more stores its
    aggregate as the read-only prefix; Enter starts a new suffix with a newline.
    A terminal receipt records aggregate history/diagnostic bytes once and clears
    the prefix. On Need_more, an exact retry consumes the captured suffix from an
    append-only edited draft and preserves its unsent tail literally. Other
    rewritten drafts remain intact with review/reset guidance. The caller must
    remove its command line and block editing until acknowledgement. If the
    retained draft exceeds the remaining aggregate budget, admission fails until
    it is shortened or reset. Observation must not repeat this transition. *)
val acknowledge_source
  :  t
  -> source:Submission_source.t
  -> preserve_editor:bool
  -> continue_line:bool
  -> Agent_protocol.Developer_repl_wire.Submission.t
  -> t

val create : limits:Limits.t -> context:string -> (t, Agent_protocol.Error.t) result
val limits : t -> Limits.t
val context : t -> string
val editor : t -> Editor.t
val activity : t -> activity
val view : t -> Agent_protocol.Developer_repl_wire.View.t option
val submission : t -> Agent_protocol.Developer_repl_wire.Submission.t option
val submitted_source : t -> string option
val submitted_source_digest : t -> Chatml_source.Digest.t option
val inferred_bindings : t -> Inferred_bindings.t
val completion : t -> Completion.t option
val completion_notice : t -> string option
val with_session_completion : t -> names:string list -> notice:string option -> t
val dismiss_completion : t -> t
val complete : t -> t
val completion_next : t -> t
val completion_previous : t -> t
val accept_completion : t -> (t, Agent_protocol.Error.t) result

(** Search only starts from Ready, captures the exact draft, and never submits.
    Completion and search modes are mutually exclusive. *)
val history_search : t -> History_search.t option

val start_history_search : t -> t

val edit_history_search
  :  t
  -> f:(History_search.t -> History_search.t Or_error.t)
  -> t Or_error.t

val cancel_history_search : t -> t
val accept_history_search : t -> t
val current_revision : t -> Agent_protocol.Developer_repl_wire.Wire_id.Revision.t option
val reference_page : t -> Agent_protocol.Developer_repl_wire.Reference_page.t option
val task : t -> Agent_protocol.Developer_repl_wire.Task_view.t option
val pending : t -> Agent_client.Developer_repl_connection.pending
val history : t -> string list
val notice : t -> string option

(** A candidate editor may have been built under a different state's bounds;
    validate bytes, lines, UTF-8 and cursor cluster boundary before adoption. *)
val with_editor : t -> Editor.t -> (t, Agent_protocol.Error.t) result

(** Bounded history navigation preserves the current unsubmitted draft and
    restores it after the newest entry. Neither action submits source. *)
val history_previous : t -> t

val history_next : t -> t

(** A successful Create or Reset replaces the view and clears references,
    submission, task, notice and editor. Reset retains the same captured source; it
    cannot select a new project or environment. *)
val with_view : t -> Agent_protocol.Developer_repl_wire.View.t -> t

(** An accepted Submit must match the current epoch. Only its displayed
    references can be selected for an explicit Task action. *)
val with_submission
  :  t
  -> Agent_protocol.Developer_repl_wire.Submission.t
  -> (t, Agent_protocol.Error.t) result

(** The owning controller validates the exact captured instance, epoch,
    submission key and revision before applying these read results. Observation
    of the same immutable receipt preserves them; a new revision or reset clears
    them. *)
val with_inferred_bindings : t -> Inferred_bindings.t -> t

(** Replaces only the displayed reference page for the current epoch/revision.
    The controller checks the request's captured epoch/revision again after its
    yielding read before calling this function. *)
val with_reference_page : t -> Agent_protocol.Developer_repl_wire.Reference_page.t -> t

val task_reference : t -> index:int -> (string, Agent_protocol.Error.t) result
val with_task : t -> Agent_protocol.Developer_repl_wire.Task_view.t -> t
val with_activity : t -> activity -> t
val with_pending : t -> Agent_client.Developer_repl_connection.pending -> t
val with_notice : t -> string -> t

(** Exact acknowledged bytes for diagnostic source matching, independently of
    local history. Invalid or out-of-bounds source is omitted. Reset clears it.
    Ordinary observation must not replace these bytes with an editor draft. *)
val with_submitted_source : t -> source:string -> t

(** Records a source in bounded local history when it satisfies this state's
    editor limits; an unrelated oversized or malformed source is omitted.
    Clears the editor unless [preserve_editor] is set for an exact retry.
    An acknowledged submission also supersedes the previous Task and notice,
    including when its source was rejected or stopped. Ordinary observation
    of the same receipt does not perform this presentation transition.
    This is presentation history, never a retry journal. Abandoning portable
    client custody never proves this source did not execute. *)
val submitted : t -> source:string -> preserve_editor:bool -> t
