(** Translate OpenAI streaming events into in-memory state updates.

    This module encapsulates all logic that handles
    [Openai.Responses.Response_stream.t] values arriving from the
    OpenAI streaming API.  For the time being we mutate the
    imperative fields inside [Model.t] directly – the surrounding
    code base still relies on interior mutability.  In a later step
    the implementation will switch to the pure *patch* based model
    described in the refactoring plan.

    The interface therefore exposes a single [handle_event] function
    which applies the necessary updates to the provided model.
*)

module Res = Openai.Responses
module Res_stream = Openai.Responses.Response_stream

val handle_fn_out : model:Model.t -> Res.Function_call_output.t -> Types.patch list

(** [handle_event ~model ev] inspects an incremental streaming event and
    returns the list of {!Types.patch} values required to update [model]
    accordingly.  The function itself performs {b no} mutations – callers
    are expected to pass the resulting patches to [Model.apply_patches]. *)

val handle_event : model:Model.t -> Res_stream.t -> Types.patch list
