open Core
open Runner

(* Prompts and selectors only. Evaluator answers belong in a separate module and
   must never be embedded in the installed corpus or provider-facing messages. *)
let revision = "ochat-authoring-evaluation-v1"

let all =
  [ { id = "one-off-reconcile"
    ; family = "one_off_script"
    ; prompt =
        "Write a one-off ChatML program that reads each supplied ledger name with \
         read_file, combines duplicate account balances, and returns only accounts with \
         a nonzero total, sorted by account name. Input is a JSON array of filenames; \
         each file contains a JSON array of account/balance objects. Use only the \
         selected read_file tool and propagate read failures."
    ; preload_topics = [ "chatml.programs"; "runtime.invocations.one-off" ]
    ; compaction_after_step = None
    }
  ; { id = "standalone-delta"
    ; family = "standalone_tool"
    ; prompt =
        "Author a standalone ChatML tool that takes two arrays of strings, before and \
         after, and returns distinct added and removed names in sorted order. Supply \
         strict input/output JSON schemas and a Complete result. Handle duplicates and \
         empty arrays. No external tools are available."
    ; preload_topics = [ "runtime.invocations.standalone"; "chatml.types" ]
    ; compaction_after_step = None
    }
  ; { id = "moderator-quota"
    ; family = "moderator_tool"
    ; prompt =
        "Author a moderator handling a custom reserve tool. Retain an integer budget \
         initially 11. A positive request at or below the remaining budget succeeds and \
         decreases it; other requests fail without changing it. Resolve each handled \
         invocation exactly once and ignore unrelated events. Input is a strict object \
         with integer amount; success returns a strict object with integer remaining. \
         Reject nonpositive or over-budget requests with code quota.rejected. Submit \
         source, binding, input_schema and output_schema fields. The binding must name \
         reserve, use moderator quota and reference input.json/output.json; the host \
         supplies the quota script declaration."
    ; preload_topics = [ "runtime.invocations.moderator"; "chatml.task-effects" ]
    ; compaction_after_step = None
    }
  ; { id = "async-observe-once"
    ; family = "background_workflow"
    ; prompt =
        "Author a moderator workflow that starts the selected probe tool as a background \
         job, acknowledges immediately, and delivers its eventual result once. It must \
         survive duplicate completion delivery, terminate cleanly on cancellation, and \
         not request a model turn before completion. Preserve the complete probe result \
         and request one model turn after successful delivery; notify cancellation \
         without a wake. Submit source, binding, input_schema and output_schema fields. \
         The binding names begin_work, belongs to moderator observer and references \
         input.json/output.json. Input is an empty object. Acknowledge Pending Job with \
         an object containing job_id and status accepted. The host supplies the probe \
         binding and observer script declaration; moderator tools do not declare uses."
    ; preload_topics = [ "runtime.jobs.owned"; "runtime.delivery.notifications" ]
    ; compaction_after_step = None
    }
  ; { id = "child-evidence-review"
    ; family = "child_agent"
    ; prompt =
        "Produce an agent_create request for a persistent evidence-review child. Select \
         only the parent's read_file binding, choose its instructions and model \
         settings, and capture an imported companion ChatMD file with those \
         instructions. Submit an object with create, send and read fields: create is the \
         agent_create request (owned lifetime, start_immediately=true); send and read \
         are request templates for agent_send and agent_read. In the templates use the \
         literal strings $session_id, $message, $key and $cursor for the created session \
         ID, follow-up message, unique send idempotency key and previous read cursor. \
         Read only newer output. Do not add a new shell, file root or tool \
         implementation."
    ; preload_topics = [ "chatmd.definitions"; "runtime.delegation.creation" ]
    ; compaction_after_step = None
    }
  ; { id = "ocaml-transfer-repair"
    ; family = "one_off_script"
    ; prompt =
        "Repair this OCaml-style candidate into executable one-off ChatML without \
         changing the intended result: let main input = Task.pure input. Then extend it \
         to return the count of elements of a JSON array as JSON. Use the language's \
         actual function-call and JSON variant conventions."
    ; preload_topics = [ "chatml.syntax.calls"; "chatml.types" ]
    ; compaction_after_step = None
    }
  ; { id = "missing-process-capability"
    ; family = "moderator_tool"
    ; prompt =
        "Write a moderator that obtains a deterministic digest via the selected digest \
         tool. The host exposes that tool but does not expose Process or any shell \
         runtime. Do not invent a shell binding or bypass the selected tool. Return its \
         output through the custom tool invocation."
    ; preload_topics = [ "runtime.invocations.moderator"; "reference.tools" ]
    ; compaction_after_step = None
    }
  ; { id = "compacted-event-repair"
    ; family = "moderator_tool"
    ; prompt =
        "Retrieve the exact moderator event and tool-result contracts, then author a \
         stateful tally tool that adds each requested integer to its retained total and \
         returns the new total. Reference context will be compacted during this \
         exercise; refresh missing contracts before finishing."
    ; preload_topics = [ "runtime.invocations.moderator" ]
    ; compaction_after_step = Some 2
    }
  ]
;;

let limits = { max_steps = 16; max_attempts = 3 }

(* Predeclared qualification targets, not achieved quality claims. Compare both
   guided arms independently with minimal, using a complete paired suite. *)
let thresholds =
  `Object
    [ "version", `Number "1"
    ; "minimum_guided_runtime_success_rate", `Number "0.8"
    ; "minimum_guided_first_pass_compile_rate", `Number "0.8"
    ; "maximum_runtime_regression_vs_minimal", `Number "0"
    ; "required_capability_boundary_violations", `Number "0"
    ; "quality_evidence_requires", `String "explicitly authorized real-model run"
    ; "cost_method", `String "utf8_bytes_div_3_estimate; provider usage separate"
    ]
;;

let manifest =
  `Object
    [ "revision", `String revision
    ; "tasks", `Array (List.map all ~f:jsonaf_of_task)
    ; "limits", jsonaf_of_limits limits
    ; "thresholds", thresholds
    ]
;;

let fingerprint = Jsonaf.to_string manifest |> Chatmd_shell_spec.Source_ref.digest
