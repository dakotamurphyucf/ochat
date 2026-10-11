open Core

(** Native legacy moderator contract for the actual host-selected surface.
    Keeps legacy caller-selected event types; it does not substitute the v1 event
    protocol. State is shared between initial_state and both on_event positions.
    Projection is pure and retains no runtime implementation. *)
val contract
  :  Chatml.Chatml_builtin_surface.surface
  -> checkpoint:(unit -> unit)
  -> Chatml.Chatml_compiler_contract.t

(** Select exact captured declaration and graph, admit its required tools, then
    compile without running any initializer. No single-file or filesystem fallback.
    Cancellation and unexpected failures propagate through the joined compiler. *)
val compile
  :  ?startup:Startup_compilation.t
  -> Chatml_prompt_programs.Restored.t
  -> script:Prompt.Chat_markdown.script
  -> surface:Chatml.Chatml_builtin_surface.surface
  -> capabilities:Tool_capability.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> limits:Chatml_compilation.limits
  -> max_dependency_depth:int
  -> (Chatml_linked_program.t, Chatml_compilation.error) Result.t
