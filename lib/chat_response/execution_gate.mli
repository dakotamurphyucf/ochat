(** Process-local coordination for serialized moderator resources. No authority
    is granted here. All owners participating in a synchronous call chain must
    use this gate; work routed outside it cannot be included in cycle detection. *)
type t

type error =
  | Reentrant
  | Wait_cycle
  | Resource_limit

val create : unit -> t
val error_message : error -> string

(** Capture the current owner ancestry for a host-controlled execution handoff,
    such as [Eio.Domain_manager.run dm (inherit_context f)]. Eio domain workers do not
    automatically inherit fiber-local variables. The returned function combines
    captured and destination ownership; expired scopes are ignored. This carries
    coordination metadata only and grants no tool authority. *)
val inherit_context : (unit -> 'a) -> unit -> 'a

(** Run a host background-task launch without inheriting synchronous owner
    dependencies. Use only for independent asynchronous work whose completion
    the current owner does not synchronously await. Never use this to bypass a
    nested call's cycle detection or authority checks. *)
val without_context : (unit -> 'a) -> 'a

(** Independent callers wait using an Eio mutex, outside any session actor.
    Direct/indirect acquisition of a held ancestor fails before entering [f].
    Dependencies between separate call chains are checked before waiting, so a
    cross-owner cycle also fails before [f]. Exceptions/cancellation propagate
    after releasing graph entries and ownership. Fiber children inherit active
    ownership; inherited scopes that have ended no longer imply ownership.

    Eio is required when waiting. Uncontended synchronous legacy calls are
    supported using domain-local ancestry. The graph has a process-wide limit
    of 4096 active/waiting acquisitions and a 64-owner ancestry limit. These are
    host coordination ceilings, not script execution or authority budgets. *)
val with_access : t -> (unit -> 'a) -> ('a, error) result
