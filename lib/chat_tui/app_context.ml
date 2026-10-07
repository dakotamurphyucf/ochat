module Ui = struct
  type t =
    { term : Notty_eio.Term.t
    ; size : unit -> int * int
    ; throttler : Redraw_throttle.t
    ; redraw : unit -> unit
    ; redraw_immediate : unit -> unit
    ; latest_frame_generation : unit -> int
    ; resize_and_redraw : size:int * int -> layout:Chat_page_layout.t -> unit
    ; render_current_with_layout : size:int * int -> layout:Chat_page_layout.t -> unit
    }
end

module Streams = struct
  type t =
    { input : App_events.input_event Eio.Stream.t
    ; internal : App_events.internal_event Eio.Stream.t
    ; redraw : unit Eio.Stream.t
    }
end

module Services = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; inference_context : Inference_runtime.Context.t
    ; inference_identity : Chat_response.Neutral_turn.Identity.t
    ; typeahead_inference : Inference_client.Execution.t option
    ; on_inference_attempt : Inference_runtime.Attempt.t -> unit
    ; on_inference_completion : Inference_client.Completion.t -> unit
    ; on_inference_observation : Inference.Observation.t -> unit
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
