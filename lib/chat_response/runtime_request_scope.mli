open Core

(** Lexical collection of native-origin runtime requests. Coalesces repeated
    requests using Runtime_semantics.collapse, retaining the first end reason.
    This is host plumbing, not authority to mutate a session. *)
type t

val collect : (unit -> 'a) -> 'a * Moderation.Runtime_request.t list

(** Capture for an owned domain handoff. Does not extend scope lifetime. *)
val capture : unit -> t option

(** Bind exactly the supplied context; None cannot borrow an unrelated ambient
    scope. Owned native borrows use this across Eio domains. *)
val with_context : t option -> (unit -> 'a) -> 'a

(** Rejects missing/closed scopes, including for an empty readiness probe.
    Concurrent emission/closure is atomic; collected data contains at most one
    request of each kind. The caller must consume it at its owned boundary. *)
val emit : Moderation.Runtime_request.t list -> (unit, string) result
