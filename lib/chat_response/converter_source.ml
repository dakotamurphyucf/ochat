open! Core

type t =
  { image :
      Prompt.Chat_markdown.basic_content_item -> (string, Agent_protocol.Error.t) result
  ; document :
      Prompt.Chat_markdown.basic_content_item -> (string, Agent_protocol.Error.t) result
  ; agent : Prompt.Chat_markdown.agent_content -> (string, Agent_protocol.Error.t) result
  }

let create ~image ~document ~agent = { image; document; agent }
let image t item = t.image item
let document t item = t.document item
let agent t item = t.agent item
