open! Core
module P = Agent_protocol
module Tool = Chat_response.Tool_capability.Local_history_tool

type media =
  | Image
  | Document

type t =
  | Windows of P.Page.Request.t
  | Items of P.Recovery_history_request.t
  | Text of P.Recovery_text_range.t
  | Media of media * P.Recovery_asset_read.t

val decode : Tool.t -> source:P.Session_ref.t -> Jsonaf.t -> (t, P.Error.t) result
val decode_text_exn : string -> Jsonaf.t
val parameters : Tool.t -> Jsonaf.t
