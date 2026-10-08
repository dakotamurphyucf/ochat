open Core

module Invocation_id = struct
  type t = string

  let next = Atomic.make 0

  let create () =
    let sequence = Atomic.fetch_and_add next 1 in
    Printf.sprintf "fork-invocation-%d" sequence
  ;;

  let to_string t = t
end

let create_allocator ~parent_namespace invocation_id =
  History_entry.Allocator.create
    ~namespace:(parent_namespace ^ "/" ^ Invocation_id.to_string invocation_id)
    ~next_sequence:0
  |> Result.ok_or_failwith
;;

let allocator = create_allocator

let instruction_template : (string -> string -> string -> string, unit, string) format =
  {|SYSTEM MESSAGE – Forked Agent

You are an **isolated clone** of the main assistant. Your child history is not merged into the parent history. All new assistant-message text is returned to the parent as tool output, including both RESULT and PERSIST sections. PERSIST is a summary convention, not an extraction boundary. Live progress may also be visible to the user.

Primary task inside the fork
• Execute:
  command - `%s`
  arguments - `%s`

Use available tools within their granted authority, including recursive forks when available. Respect output and token limits.

Return exactly **one** assistant message in this template:

```
===RESULT===
<Report actions, outcomes, relevant evidence, validation, and unresolved issues.>

===PERSIST===
<Concise summary of facts, artefacts, follow-ups, or warnings for the parent. This section does not exclude the rest of your reply from the returned output.>
```

Best-practice reminders:
• Include concise explanations and evidence in RESULT.
• Perform a quick self-check before replying; note unresolved issues in PERSIST.
• Avoid filler phrases like “let’s think step-by-step”.  Just reason and write.

Call-ID: %s
|}
;;

let instruction_payload ~arguments ~call_id =
  let input = Definitions.Fork.input_of_string arguments in
  let arg_str = String.concat ~sep:" " input.arguments in
  let instruction_text =
    Printf.sprintf instruction_template input.command arg_str call_id
  in
  let module P = History_entry.Payload in
  P.Semantic.create
    (Result { relation = Unresolved; kind = Function; output = Text instruction_text })
    ~metadata:{ P.Metadata.empty with call_id = Value call_id }
  |> Result.ok_or_failwith
  |> P.authored
;;

let history_entries ~allocator ~history:entries ~arguments ~call_id =
  let instruction =
    History_entry.create ~allocator (instruction_payload ~arguments ~call_id)
    |> Result.ok_or_failwith
  in
  entries @ [ instruction ]
;;
