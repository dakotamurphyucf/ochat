open Core
module S = Chatml_builtin_spec
module Surface = Chatml_builtin_surface

let record fields = S.TRecord (TRow_extend (fields, TRow_empty))
let variant cases = S.TVariant (TRow_extend (cases, TRow_empty))
let option ty = variant [ "None", S.TUnit; "Some", ty ]
let work_ref_ty = variant [ "Job", S.TString; "Subscription", S.TString ]

let tool_error_ty =
  record
    [ "code", S.TString
    ; "message", S.TString
    ; "retryable", S.TBool
    ; "details", S.json_ty
    ]
;;

let tool_outcome_ty =
  variant
    [ "Complete", S.json_ty
    ; "Pending", S.TTuple [ work_ref_ty; S.json_ty ]
    ; "Fail", tool_error_ty
    ]
;;

let limits_ty =
  record
    (List.map
       [ "fuel"
       ; "max_tasks"
       ; "max_value_bytes"
       ; "max_output_bytes"
       ; "max_array_items"
       ; "max_depth"
       ; "max_nested_calls"
       ; "max_invocation_depth"
       ]
       ~f:(fun name -> name, S.TInt))
;;

let capability_ty =
  record
    [ "id", S.TString
    ; "name", S.TString
    ; "implementation_revision", S.TString
    ; "fingerprint", S.TString
    ; "input_schema", S.json_ty
    ]
;;

let origin_ty =
  variant
    (List.map
       [ "Model"; "Moderator"; "Script"; "Delegated_agent"; "External_adapter" ]
       ~f:(fun name -> name, S.TUnit))
;;

let tool_context_ty =
  record
    [ "version", S.TInt
    ; "invocation_id", S.TString
    ; "provider_call_id", option S.TString
    ; "session_id", S.TString
    ; "generation", S.TInt
    ; "origin", origin_ty
    ; "parent_invocation", option S.TString
    ; "parent_job", option S.TString
    ; "tool_name", S.TString
    ; "implementation_revision", S.TString
    ; "capability_fingerprint", S.TString
    ; "created_at_ms", S.TInt
    ; "deadline_ms", option S.TInt
    ; "limits", limits_ty
    ; "available_tools", S.TArray capability_ty
    ]
;;

let one_off_v1 =
  let modules =
    List.filter_map S.moderator_modules ~f:(fun (entry : S.builtin_module) ->
      match entry.name with
      | "Log" -> Some entry
      | "Tool" ->
        Some
          { entry with
            exports =
              List.filter entry.exports ~f:(fun value -> String.equal value.name "call")
          }
      | _ -> None)
  in
  Surface.merge
    { Surface.core_surface with
      globals =
        List.filter Surface.core_surface.globals ~f:(fun value ->
          not (String.equal value.name "print"))
    }
    { Surface.empty with modules }
;;

let tool_v1 =
  Surface.merge
    one_off_v1
    { Surface.empty with
      type_aliases =
        List.map
          [ "work_ref", work_ref_ty
          ; "tool_error", tool_error_ty
          ; "tool_outcome", tool_outcome_ty
          ; "tool_context", tool_context_ty
          ; "tool_limits", limits_ty
          ; "tool_capability", capability_ty
          ]
          ~f:(fun (name, body) -> Surface.{ name; body })
    }
;;

let one_off_entrypoints = [ "main", S.TFun ([ S.json_ty ], S.task_ty S.json_ty) ]

let tool_entrypoints =
  [ "run", S.TFun ([ tool_context_ty; S.json_ty ], S.task_ty tool_outcome_ty) ]
;;
