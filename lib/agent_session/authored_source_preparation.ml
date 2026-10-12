open Core

type t =
  { source : Authored_agent_source.t
  ; revision : Prompt_revision.t
  ; script_preparation : Runtime_script_preparation.t option
  }

let source t = t.source
let revision t = t.revision
let script_preparation t = t.script_preparation

let with_prepared
      ~startup
      ~script_preparation
      ~content_limits
      ~parent
      ~tool_name
      ~env
      ~checkpoint
      ~f
  =
  Runtime_script_preparation.with_authored_child
    ?startup
    script_preparation
    ~parent
    ~tool_name
    ~content_limits
    ~env
    ~checkpoint
    ~f:(fun startup ~source ~revision ->
      f startup { source; revision; script_preparation })
;;
