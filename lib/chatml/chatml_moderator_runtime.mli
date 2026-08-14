(** Compatibility facade for the generic ChatML host runtime. *)

include module type of Chatml_host_runtime
  with type compiled_script = Chatml_host_runtime.compiled_script
   and type session = Chatml_host_runtime.session
   and type pending_ui_request = Chatml_host_runtime.pending_ui_request
   and type log_level = Chatml_host_runtime.log_level
   and type turn_effect = Chatml_host_runtime.turn_effect
   and type tool_moderation = Chatml_host_runtime.tool_moderation
   and type local_effect = Chatml_host_runtime.local_effect
   and type prepare_commit = Chatml_host_runtime.prepare_commit
   and type op_kind = Chatml_host_runtime.op_kind
   and type op_def = Chatml_host_runtime.op_def
   and type runtime_config = Chatml_host_runtime.runtime_config
   and type execution_limits = Chatml_host_runtime.execution_limits
   and type compiled_entrypoints = Chatml_host_runtime.compiled_entrypoints
   and type default_handlers = Chatml_host_runtime.default_handlers
