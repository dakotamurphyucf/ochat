open! Core

(** Synthetic selected inference for existing offline DTO stream fixtures.
    Output is explicitly Reconstructed, never an original provider capture.
    EOF is the fixture's declared completion and usage is explicitly unknown.
    Only finalized DTO items become candidates; announcement is not admission.
    Observers, cancellation and unexpected mock exceptions propagate unchanged. *)
type post_stream =
  sw:Eio.Switch.t
  -> inputs:Openai.Responses.Item.t list
  -> Openai.Responses.Response_stream.t Seq.t

type t

(** Namespace must be fresh within the fixture's actual observation lifetime.
    Model omission uses only this explicitly supplied fixture model. No global
    provider selection, credentials, network driver or transport fallback. *)
val create : namespace:string -> default_model:string -> post_stream:post_stream -> t

val capture_config
  :  t
  -> Chat_response.Config.t
  -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t

val recapture_config
  :  t
  -> current:Inference.Request.Target.t
  -> Chat_response.Config.t
  -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t

val resolve : t -> Inference_runtime.resolver
val identity : t -> Chat_response.Neutral_turn.Identity.t

(** Convenience for callers whose selected fixture owns its context. Config
    omission inherits this explicit fixture's declared model/settings. No global
    provider default or effectful observer is installed. *)
val ctx
  :  t
  -> ?config:Chat_response.Config.t
  -> env:'env
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> tool_dir:Eio.Fs.dir_ty Eio.Path.t
  -> cache:Chat_response.Cache.t
  -> unit
  -> 'env Chat_response.Ctx.t
