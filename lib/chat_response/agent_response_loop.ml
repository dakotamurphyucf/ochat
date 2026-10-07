open! Core
module Res = Openai.Responses

type observer =
  { on_event : Transcript.Stream.t -> unit
  ; on_tool_execution : Tool_execution_event.t -> unit
  }

type post_stream =
  sw:Eio.Switch.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> inputs:Res.Item.t list
  -> Res.Response_stream.t Seq.t

exception Openai_stream_idle_timeout of float

let run_entries
      ~(ctx : Eio_unix.Stdenv.base Ctx.t)
      ~allocator
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ?(fork_depth = 0)
      ?(history_compaction = false)
      ?response_dir
      ?source
      ?parent_call_id
      ?model
      ~tool_tbl
      ~observer
      ?post_stream
      history
  =
  if Option.is_some post_stream
  then
    invalid_arg
      "provider-shaped post_stream is retired; inject an explicit inference context";
  In_memory_stream.run_completion_stream_in_memory_entries
    ~env:(Ctx.env ctx)
    ~inference_context:ctx.inference_context
    ~inference_identity:ctx.inference_identity
    ~on_inference_attempt:ctx.on_inference_attempt
    ~on_inference_completion:ctx.on_inference_completion
    ~on_inference_observation:ctx.on_inference_observation
    ~inference_relation:ctx.inference_relation
    ?datadir:response_dir
    ~allocator
    ~history
    ~tools
    ~tool_tbl
    ?temperature
    ?max_output_tokens
    ?reasoning
    ~history_compaction
    ~fork_depth
    ~on_transcript_event:observer.on_event
    ?source
    ?parent_call_id
    ~on_tool_execution:observer.on_tool_execution
    ~parallel_tool_calls:false
    ?model
    ()
;;

let run
      ~ctx:_
      ?temperature:_
      ?max_output_tokens:_
      ?tools:_
      ?reasoning:_
      ?fork_depth:_
      ?history_compaction:_
      ?response_dir:_
      ?source:_
      ?parent_call_id:_
      ~model:_
      ~tool_tbl:_
      ~observer:_
      ?post_stream:_
      _
  =
  invalid_arg
    "provider DTO history result API is retired; use run_entries with an explicit \
     inference context"
;;
