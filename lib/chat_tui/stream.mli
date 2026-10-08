(** Bounded neutral standalone drafts. No provider decoding and no canonical
    append: only the actual postcommit callback owns writable history. *)
type t

val create : unit -> t
val apply : t -> Transcript.Stream.t -> (t, string) result

(** Parent Chat rows include only actual Root scopes, including unknown evidence.
    Nested scopes remain in the underlying Draft; owned Agent activity presents
    their readable progress. This does not claim opaque nested evidence is
    rendered as activity text. *)
val rows : t -> Projected_message.t list

val rows_of_draft : Transcript.Draft.t -> Projected_message.t list

(** Retire only the actual committed root host occurrence; nested drafts remain
    presentation-only and cannot enter the parent canonical list. *)
val remove_committed : t -> History_entry.Id.t -> t
