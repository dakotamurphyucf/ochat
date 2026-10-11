open Core

(** Actual parser-owned role requests, transient within one source preparation. *)
module Role : sig
  type t =
    | Root_ui
    | Headless
    | Delegated_ui
  [@@deriving equal]
end

type t

val parsed : t -> Prompt.Chat_markdown.Parsed.t
val role : t -> Role.t

(** Root UI first; only reachable emitted references add other roles. Join exact
    queued keys to the actual capture, polling key comparisons. Count before
    retaining requests; reference discovery runs once per physical Parsed. Root
    cycles reuse the original context. No source access or callback retention. *)
val collect
  :  Prompt_revision_builder.Capture.t
  -> max_context_roles:int
  -> max_reference_visits:int
  -> checkpoint:(unit -> unit)
  -> (t list, Prompt_revision_builder.Diagnostic.t list) Result.t
