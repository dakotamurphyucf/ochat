open! Core
module Res = Openai.Responses

type post =
  sw:Eio.Switch.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> inputs:Res.Item.t list
  -> Res.Response.t

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
      ?post
      ?model
      ~tool_tbl
      history
  =
  if Option.is_some post
  then invalid_arg "provider-shaped post is retired; inject an explicit inference context";
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
    ~parallel_tool_calls:false
    ?model
    ()
;;

module For_testing = struct
  let retry_request = In_memory_stream.For_testing.retry_request
end
