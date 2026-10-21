open! Core
module Ledger = Chatml_authoring_context
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Capture = Authoring_source_host.Document_context.Project_capture

module Policy = struct
  type t =
    { authorize_now :
        original_actor:Operator_authorization.t
        -> actor:Operator_authorization.t
        -> current:Ledger.Document.Inputs.t
        -> parsed:Parsed.t
        -> declaration:Declaration.t
        -> capture:Capture.t
        -> bool
    }

  let create ~authorize_now = { authorize_now }

  let authorize_now t ~original_actor ~actor ~current ~parsed ~declaration ~capture =
    t.authorize_now ~original_actor ~actor ~current ~parsed ~declaration ~capture
  ;;
end
