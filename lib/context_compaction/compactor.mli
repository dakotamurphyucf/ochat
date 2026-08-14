(** Conversation–history compactor.

    [Compactor] combines {!module:Context_compaction.Config} and
    {!module:Context_compaction.Summarizer}. Its purpose is to trim a
    potentially long chat transcript down to a size that comfortably fits
    within the LLM’s context window while keeping the essence of the
    conversation intact.

    The pipeline:

    {ol
    {- Prunes retained user-role compaction reminders to the newest ten.}
    {- Passes that history to {!Summarizer.summarise}.}
    {- Returns the original System and Developer items, the retained previous
       reminders, and one new user-role reminder.}}

    The pipeline returns errors explicitly so callers can retain the original
    history transactionally.

    {1 Typical usage}

    {[
      let compacted =
        Context_compaction.Compactor.compact_entries
          ~allocator
          ~env:(Some stdenv)
          ~history
      in
      Result.iter compacted ~f:(fun history ->
        send_to_llm (history @ new_user_messages))
    ]}
    *)

open! Core

(** [compact_entries ~allocator ~env ~history] returns a condensed replacement for
    [history], or an error without modifying [history].

    A successful result:
    {ul
    {- preserves original System and Developer items verbatim;}
    {- retains at most ten previous compaction reminders;}
    {- appends exactly one new compaction reminder.}}

    Parameters
    {ul
    {- [env] – optional {!Eio_unix.Stdenv.base}.  When [Some], the pipeline
       invokes the OpenAI API; when [None] it falls back to deterministic
       offline stubs.}
    {- [history] – full conversation transcript to be compacted.}}

    @raise Eio.Cancel.Cancelled if the operation is cancelled. *)
val compact_entries
  :  allocator:History_entry.Allocator.t
  -> env:Eio_unix.Stdenv.base option
  -> history:History_entry.t list
  -> (History_entry.t list, exn) result
(** [compact_entries ~allocator ~env ~history] transactionally compacts
    canonical history. Retained entries keep their IDs. Exactly one new
    reminder ID is allocated after summarization succeeds. *)

module For_testing : sig
  val process_current_entries
    :  History_entry.t list
    -> History_entry.t list * History_entry.t list * History_entry.t list

  val compact_entries_with
    :  summarise:
         (relevant_items:Openai.Responses.Item.t list
          -> env:Eio_unix.Stdenv.base option
          -> (string, exn) result)
    -> allocator:History_entry.Allocator.t
    -> env:Eio_unix.Stdenv.base option
    -> history:History_entry.t list
    -> (History_entry.t list, exn) result
end
