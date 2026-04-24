open! Core

module Moderation = Chatml_moderation
module Res = Openai.Responses

module Registry = struct
  type artifact = Moderator_manager.Registry.artifact
  type t = Moderator_manager.Registry.t

  let empty = Moderator_manager.Registry.empty
  let artifact_count = Moderator_manager.Registry.artifact_count
  let compile_script t script = Moderator_manager.Registry.compile_script t script
  let of_elements t elements = Moderator_manager.Registry.of_elements t elements
  let script_id = Moderator_manager.Registry.script_id
  let source_hash = Moderator_manager.Registry.source_hash
end

type t = Moderator_manager.t

type pending_ui_request = Moderator_manager.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

let create = Moderator_manager.create
let handle_event = Moderator_manager.handle_event
let pending_ui_request = Moderator_manager.pending_ui_request
let resume_ui_request = Moderator_manager.resume_ui_request
let drain_internal_events = Moderator_manager.drain_internal_events
let effective_items = Moderator_manager.effective_items
let effective_history = Moderator_manager.effective_history
let snapshot = Moderator_manager.snapshot
let enqueue_internal_event = Moderator_manager.enqueue_internal_event
