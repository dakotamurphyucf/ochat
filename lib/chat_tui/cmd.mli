(** Elm-style command interpreter.

    {!Chat_tui} separates pure state transitions from impure effects.  The
    controller layer produces values of {!Chat_tui.Types.cmd} which describe
    {e what} needs to happen (persist the transcript, start a network
    request, …) without performing the effect.  This module is the single
    implementation point that turns those descriptions into real IO by
    executing the closures carried by each constructor.

    At the time of writing the command set is still small – we only persist
    the current session and manage the lifetime of a single streaming
    request – but additional constructors will be added as the UI grows.  No
    other module should perform side-effects directly. *)

open Types

(** [run c] executes the effect encoded by command [c].

    Each constructor is interpreted as follows:
    {ul
      {- [Persist_session f] – spawn [f] in the background fibre pool so the
         UI stays responsive while the transcript is written to disk;}
      {- [Start_streaming f] – start a new fibre that performs the OpenAI
         streaming request driven by [f];}
      {- [Cancel_streaming f] – cancel the running request by invoking [f].}}

    The exact implementation of each thunk is opaque to this module and may
    evolve independently. *)
val run : cmd -> unit

(** [run_all cmds] executes every command in [cmds] from left to right using
    {!run}.  It is a thin wrapper around [List.iter] and provides symmetry
    with the controller functions that return lists of commands. *)
val run_all : cmd list -> unit
