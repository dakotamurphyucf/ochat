module Ui = struct
  type t =
    { term : Notty_eio.Term.t
    ; throttler : Redraw_throttle.t
    ; redraw : unit -> unit
    ; redraw_immediate : unit -> unit
    }
end

module Streams = struct
  type t =
    { input : App_events.input_event Eio.Stream.t
    ; internal : App_events.internal_event Eio.Stream.t
    ; system : string Eio.Stream.t
    }
end

module Services = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; ui_sw : Eio.Switch.t
    ; cwd : Eio.Fs.dir_ty Eio.Path.t
    ; cache : Chat_response.Cache.t
    ; datadir : Eio.Fs.dir_ty Eio.Path.t
    ; session : Session.t option
    }
end

module Resources = struct
  type t =
    { services : Services.t
    ; streams : Streams.t
    ; ui : Ui.t
    }
end

