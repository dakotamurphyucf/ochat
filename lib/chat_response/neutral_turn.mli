(** Shared selected dispatch used by the existing turn/tool owner. *)
include
  module type of Inference_client
  with module Error = Inference_client.Error
   and module Completion = Inference_client.Completion
   and module Identity = Inference_client.Identity
   and module Text = Inference_client.Text
   and module Execution = Inference_client.Execution
