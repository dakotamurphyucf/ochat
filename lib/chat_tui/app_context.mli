(** Context records for the chat TUI app.

    The application threads a number of common resources through the reducer and
    background workers.  These records provide stable groupings so we do not
    have to pass long argument lists through multiple layers. *)

module Ui : sig
  type t =
    { term : Notty_eio.Term.t
    ; throttler : Redraw_throttle.t
    ; redraw : unit -> unit
    ; redraw_immediate : unit -> unit
    }
end

module Streams : sig
  type t =
    { input : App_events.input_event Eio.Stream.t
    ; internal : App_events.internal_event Eio.Stream.t
    ; system : string Eio.Stream.t
    }
end

module Services : sig
  type t =
    { env : Eio_unix.Stdenv.base
    ; ui_sw : Eio.Switch.t
    ; cwd : Eio.Fs.dir_ty Eio.Path.t
    ; cache : Chat_response.Cache.t
    ; datadir : Eio.Fs.dir_ty Eio.Path.t
    ; session : Session.t option
    }
end

module Resources : sig
  type t =
    { services : Services.t
    ; streams : Streams.t
    ; ui : Ui.t
    }
end

