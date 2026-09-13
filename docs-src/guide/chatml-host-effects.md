# Logging, turn edits and tool effects

Use task composition to sequence host operations. These functions construct tasks;
their work happens when the runtime interprets them. Available compiler names,
installed host services and current permission are separate requirements. See
[task semantics](chatml-task-effects.md) and [execution contracts](chatml-authoring-runtime.md).

On all four extensibility surfaces, Log and Tool.call/Tool.spawn are available.
Turn and the four tool-decision operations are moderator-only. A delegated
moderator uses the same syntax within its inherited authority. None of these
operations grants new shell, file, network or session access.

## Diagnostic logging

| Call | Meaning |
|---|---|
| `Log.debug(message)` | Debug diagnostic; returns a unit task. |
| `Log.info(message)` | Informational diagnostic; returns a unit task. |
| `Log.warn(message)` | Warning diagnostic; returns a unit task. |
| `Log.error(message)` | Error-level diagnostic; returns a unit task, without itself failing the workflow. |

Messages are strings. The host selects the sink and may discard diagnostic output;
logging is not a model message or retained notification. Once interpreted,
diagnostics are not transactional: catch, handler failure or a rejected local
commit does not erase already emitted logs. Do not log secrets. Logging does not
call a model or request a follow-up turn.

## Edit the projected turn

| Call | Meaning |
|---|---|
| `Turn.prepend_system(text)` | Stage a prepended developer instruction; the compatibility name remains. |
| `Turn.append_item(item)` | Stage an appended item. |
| `Turn.replace_item(target_id, item)` | Stage a replacement of a projected item by its actual ID. |
| `Turn.delete_item(target_id)` | Stage deletion from the effective view. |
| `Turn.replace_or_append(optional_id, item)` | `Some(id)` selects replacement; `None` selects append. |
| `Turn.append_notice(text)` | Stage an appended developer notice. |
| `Turn.append_message(item)` | Alias of append_item. |
| `Turn.replace_message(target_id, item)` | Alias of replace_item. |
| `Turn.delete_message(target_id)` | Alias of delete_item. |
| `Turn.halt(reason)` | Stage a host overlay halt intent with a reason. |

Every operation returns a unit task. Use IDs from the current projected context,
not guessed provider IDs or an ID passed to a newly constructed Item. In the
identity-based host, inserts receive host IDs and replacements keep their target's
identity. Unknown or foreign replacement/deletion targets fail host validation.
`replace_or_append` does not check existence or fall back after failed replacement.

These operations update an overlay; they do not erase or rewrite canonical
history. Context is a snapshot for the handler: staged edits do not immediately
change its `ctx.items`. The host projects committed changes at safe boundaries.
A caught task failure discards the local edits staged inside that catch scope.
Whole-handler failure or rejected commit prevents its prospective state/edits
from installing. This does not undo arbitrary local mutation, an already completed
tool call or a diagnostic log.

`Turn.halt` does not interrupt the current task continuation. It records intent
for the host's halted overlay; it is not an immediate process exit or identical
to `Runtime.end_session`. Code after the task can still run, and a later failure
can prevent the halt intent from committing. See the
[overlay and safe-point model](chatml-moderator-runtime.md#history-and-overlay-model).

## Moderate or invoke tools

| Call | Meaning |
|---|---|
| `Tool.approve()` | Accept the pending tool proposal, subject to normal authorization. |
| `Tool.reject(reason)` | Reject that proposal and return the supplied reason as its synthetic result. |
| `Tool.rewrite_args(json)` | Replace the proposed arguments while retaining the tool name. |
| `Tool.redirect(name, json)` | Replace both proposed name and arguments. |
| `Tool.call(name, json)` | Invoke the selected tool synchronously, returning `Ok(json)` or `Error(string)` as a task result. |
| `Tool.spawn(name, json)` | On extensibility-v1 surfaces, stage the same owned job start as `Job.start_tool`, returning its job ID. |

Return at most one decision operation from a `Pre_tool_call` handler. Approve
plus rewrite, two approvals, or reject plus redirect is a conflicting decision,
not an ordered chain of transformations; the host's outcome validation rejects
multiple surviving decisions. Returning no decision leaves the original proposal
approved, subject to normal authorization. Decisions emitted for unrelated events are inappropriate;
the conversation driver rejects unexpected decisions outside its tool boundary.
Do not assume all embedding runtimes enforce this through compiler checks or
the same low-level phase registry.

Approval and redirection cannot bypass the normal tool registry or permission
checks. A rewrite must still satisfy the destination tool's contract. The pending
call keeps its correlation ID; rejecting a proposal does not execute its target.
These decisions are separate from resolving a moderator-handled `Tool_invoked`
request with `Invocation.resolve`.

`Tool.call` executes an external action rather than buffering it as a turn edit.
Inspect its result variant; an ordinary `Error` value is not automatically a task
failure. Missing services and host execution failures can also fail the task.
Catch and failed persistence cannot undo an external action already performed.
Native opaque results and managed invocation outcome envelopes are distinct;
follow the [invocation contracts](chatml-authoring-runtime.md) when decoding them.

The versioned `Tool.spawn` alias uses the owned job service in qualified one-off,
standalone and moderator execution. Launch is selected by the owning successful
commit; failed/discarded transactions do not launch staged jobs. Use job status,
result and cancellation operations, or moderator subscriptions, to manage later
completion. Returning an ID does not mean the work finished or that a model was
notified. The legacy, unversioned Tool.spawn uses its older asynchronous callback
contract. See [background work](chatml-authoring-background.md) for ownership,
acknowledgement, recovery and notification semantics.

## Checked moderator workflow

This example requires an allowed tool named `probe`. It illustrates decisions
using illustrative pending names `reject`, `rewrite` and `redirect`; redirected
`echo` must also be selected and authorized to execute. Load the script with the
versioned moderator declaration from the execution guide.

The offline fixture supplies only a fake probe callback. It runs both moderator
surfaces through the actual task runtime and host outcome decoder, checks
recovery, logs, rejected commit, conflicting decisions, aliases and halt intent.
It does not call a provider or claim a durable daemon commit. Separate actual
daemon tests exercise the versioned Tool.spawn alias and selected-tool job service.

<!-- ochat-authoring-example: {"id":"host-effects.workflow","surface":"moderator_v1","fixture":"test/chatml_extensibility_fixtures/authoring-effects/moderator.chatml","also_check":["delegated_moderator_v1"]} -->
```ocaml
let initial_state = 0
let on_event ctx state event =
  match event with
  | `Session_start ->
    let* () = Log.debug("begin") in
    let* () = Task.catch(
      (let* () = Turn.append_notice("discarded") in
       let* result = Tool.call("probe", `Null) in
       let* () = Log.error("probe already ran") in
       Task.fail("recover")),
      fun message -> Log.warn(message)) in
    let* () = Log.info("commit") in
    let* () = Turn.prepend_system("Use the selected tools within their permissions.") in
    let* () = Turn.append_notice("Ready") in
    Task.pure(state + 1)
  | `Pre_tool_call(call) ->
    let action =
      if Tool_call.is_named(call, "reject") then Tool.reject("Declined by moderator")
      else if Tool_call.is_named(call, "rewrite") then
        Tool.rewrite_args(Json.set_field(call.args, "mode", `String("safe")))
      else if Tool_call.is_named(call, "redirect") then Tool.redirect("echo", call.args)
      else if Tool_call.is_named(call, "conflict") then
        let* () = Tool.approve() in Tool.reject("second decision")
      else Tool.approve()
    in
    let* () = action in
    Task.pure(state + 1)
  | `Turn_start ->
    let target = match Context.last_assistant_item(ctx) with
      | `None -> Option.none()
      | `Some(item) -> Option.some(Item.id(item))
    in
    let* () = Turn.replace_or_append(target, Item.assistant_text("status", "Reviewed")) in
    Task.pure(state + 1)
  | `Internal_event(command) ->
    (match Json.as_string(command) with
     | `Some("edit") ->
       (match Context.last_assistant_item(ctx) with
        | `None -> Task.pure(state)
        | `Some(item) ->
          let target = Item.id(item) in
          let* () = Turn.replace_item(target, Item.assistant_text("draft", "Draft")) in
          let* () = Turn.replace_message(target, Item.assistant_text("final", "Final")) in
          let* () = Turn.delete_item(target) in
          let* () = (match Context.last_tool_result(ctx) with
            | `None -> Task.pure(())
            | `Some(result) -> Turn.delete_message(Item.id(result))) in
          let* () = Turn.append_item(Item.assistant_text("summary", "Summary")) in
          let* () = Turn.append_message(Item.notice("notice", "Review complete")) in
          Task.pure(state + 1))
     | `Some("fail") ->
       let* () = Turn.append_notice("must not commit") in
       Task.fail("whole handler failed")
     | `Some("halt") ->
       let* () = Turn.halt("Review finished") in
       let* () = Log.info("after halt construction") in
       Task.pure(state + 1)
     | _ -> Task.pure(state))
  | _ -> Task.pure(state)
```
