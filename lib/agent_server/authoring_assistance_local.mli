open! Core
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Chatmd = Chatmd_authoring_assistance
module Chatml = Chatml_authoring_fallback
module Error = Authoring_context_service_core.Error

val classify : Chatmd.Error.t -> Error.t

(** Structural ChatMD help over one exact current authored source. This path
    deliberately has no Parsed, imported input, project Build, selected tool
    or host-root evidence. The Root parser input is only a byte-bound lexical
    owner and does not claim that parsing succeeded. Later genuine retained
    captures use a separate admitted Facts path. *)
val produce_chatmd
  :  request:Wire.Request.t
  -> authored:Chatml_source.t
  -> limits:Chatmd.Limits.t
  -> max_parser_input_bytes:int
  -> checkpoint:(unit -> unit)
  -> (Wire.Result.t, Error.t) result

(** ChatML syntax/lexer fallback without a selected Build. It never unions
    compiler contracts or claims a visible semantic binding. *)
val produce_chatml
  :  request:Wire.Request.t
  -> authored:Chatml_source.t
  -> limits:Chatml.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Result.t, Error.t) result
