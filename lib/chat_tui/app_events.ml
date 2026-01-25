open Eio.Std
module Res = Openai.Responses
module Res_stream = Res.Response_stream
module Res_item = Res.Item

type input_event = Notty.Unescape.event

type internal_event =
  [ `Resize
  | `Redraw
  | `Streaming_started of int * Switch.t
  | `Stream of int * Res_stream.t
  | `Stream_batch of int * Res_stream.t list
  | `Tool_output of int * Res_item.t
  | `Streaming_done of int * Res_item.t list
  | `Streaming_error of int * exn
  | `Submit_requested of App_runtime.submit_request
  | `Compact_requested
  | `Compaction_started of int * Switch.t
  | `Compaction_done of int * Res_item.t list
  | `Compaction_error of int * exn
  ]
