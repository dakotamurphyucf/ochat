(** Notty ⟷ Eio glue code.

    This module provides a thin wrapper around {!Notty_unix.Term} that
    plugs it into Eio’s structured-concurrency runtime.  It eliminates
    the need for explicit `Unix` file-descriptor handling and signal
    management when writing interactive text-user-interface (TUI)
    programs in Eio.

    The API mirrors {!Notty_unix.Term} closely but exposes it as a
    single abstract type {!type:Term.t} plus a handful of imperative
    helpers that operate on a running terminal session.

    {1 Quick start}

    {[
      open Eio_main

      let () =
        Eio_main.run @@ fun env ->
        let stdin  = Eio.Stdenv.stdin  env in
        let stdout = Eio.Stdenv.stdout env in

        Notty_eio.Term.run
          ~input:stdin ~output:stdout
          ~on_event:(function
            | `Key (`ASCII 'q', _) | `End -> raise Exit
            | `Resize | `Key _ | `Mouse _ -> ())
          (fun term ->
             Term.image term (Notty.I.string Notty.A.empty "Hello world");
             Eio.Fiber.await_cancel ())
    ]}

    Press {b q} to exit.  Window resizes are handled automatically.
*)

module Term : sig
  (** Full-screen terminal session bound to an Eio {!Eio.Switch.t}. *)

  (** Handle representing an exclusive, full-screen Notty session.

      Invariants:
      • The handle is valid only until {!release} (or the surrounding
        switch) finishes.
      • All operations are {i not} thread-safe; call them from the fiber
        that executed the {!run} callback. *)
  type t

  (** [run ?nosig ?mouse ?bpaste ~input ~output ~on_event fn] starts a
      full-screen Notty session and returns the result of [fn term].

      Internally this creates an Eio {!Eio.Switch.t} nursery, spawns a
      daemon fiber that continuously feeds decoded events to
      [on_event], and installs cleanup handlers (terminal attributes,
      SIGWINCH, etc.).  The resources are guaranteed to be released
      when the function exits, even if it raises an exception. *)
  val run
    :  ?nosig:bool
         (** Disable terminal signals such as ^Z (^C is still delivered
             as usual).  Defaults to [true] – i.e. signals are
             suppressed so that Notty can use the entire key space. *)
    -> ?mouse:bool
         (** Enable SGR mouse reporting (button-down, drag, wheel).
             Defaults to [true]. *)
    -> ?bpaste:bool
         (** Enable {i bracketed-paste} mode so that large pastes arrive
             as a pair of [`Paste `Start] / [`Paste `End] events.
             Defaults to [true]. *)
    -> input:_ Eio_unix.source
         (** Raw byte stream coming from the terminal – e.g.
             [Eio.Stdenv.stdin env]. *)
    -> output:_ Eio_unix.sink
         (** Writable flow connected to the terminal – typically
             [Eio.Stdenv.stdout env]. *)
    -> on_event:([ Notty.Unescape.event | `Resize ] -> unit)
         (** Callback invoked from a dedicated background fiber whenever
             a new input event is decoded or the terminal window
             changes size.  It {e must} be non-blocking; heavy work
             should be off-loaded to another fiber. *)
    -> (t -> 'a)
       (** User function executed with an active terminal handle.  It
             runs in the same fiber as the caller, so it {e may}
             perform blocking operations.  The session ends when this
             function returns (or raises). *)
    -> 'a

  (** [image t img] renders [img] and flushes it to the terminal.  Use
      this to draw a new frame. *)
  val image : t -> Notty.image -> unit

  (** [refresh t] forces a redraw with the last image supplied via
      {!image}.  This is useful after partial updates when re-using the
      previous frame. *)
  val refresh : t -> unit

  (** [cursor t pos] sets the cursor position.
      • [Some (x, y)] – make the cursor visible at [(x, y)].
      • [None]        – hide the cursor. *)
  val cursor : t -> (int * int) option -> unit

  (** Current terminal size as [(cols, rows)].  The value is updated
      internally on every [`Resize] event. *)
  val size : t -> int * int

  (** Manually terminate the session.  Usually you do not need to call
      this – it is executed automatically when the enclosing switch is
      released – but it can be handy for early shutdown. *)
  val release : t -> unit
end
