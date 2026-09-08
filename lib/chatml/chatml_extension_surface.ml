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

let invocation_event_ty =
  record [ "version", S.TInt; "context", tool_context_ty; "input", S.json_ty ]
;;

let completion_ty =
  variant
    [ "Succeeded", S.json_ty
    ; "Failed", tool_error_ty
    ; "Cancelled", S.TString
    ; "Expired", S.TUnit
    ]
;;

let work_completion_ty =
  record
    [ "version", S.TInt
    ; "work", work_ref_ty
    ; "originating_invocation", option S.TString
    ; "result", completion_ty
    ]
;;

let moderator_event_ty =
  variant
    [ "Session_start", S.TUnit
    ; "Session_resume", S.TUnit
    ; "Turn_start", S.TUnit
    ; "Item_appended", S.item_ty
    ; "Pre_tool_call", S.tool_call_ty
    ; "Post_tool_response", S.tool_result_ty
    ; "Turn_end", S.TUnit
    ; "Internal_event", S.json_ty
    ; "Tool_invoked", invocation_event_ty
    ; "Job_completed", work_completion_ty
    ; "Subscription_expired", work_completion_ty
    ]
;;

let task_builtin ~name ~op ~parameters ~result ~spawn : S.builtin =
  { name
  ; scheme = S.TFun (parameters, S.task_ty result)
  ; impl =
      (fun args ->
        if List.length args <> List.length parameters
        then failwith (op ^ ": invalid arity");
        let task_effect : Chatml_lang.eff = { op; args } in
        Chatml_lang.VTask (if spawn then TSpawn task_effect else TPerform task_effect))
  }
;;

let moderator_v1 =
  let invocation : S.builtin_module =
    { name = "Invocation"
    ; exports =
        [ task_builtin
            ~name:"resolve"
            ~op:"Invocation.resolve"
            ~parameters:[ S.TString; tool_outcome_ty ]
            ~result:S.TUnit
            ~spawn:false
        ]
    }
  in
  let overrides =
    List.filter_map S.moderator_modules ~f:(fun entry ->
      let replacement =
        match entry.name with
        | "Runtime" ->
          Some
            (task_builtin
               ~name:"emit"
               ~op:"Runtime.emit_json"
               ~parameters:[ S.json_ty ]
               ~result:S.TUnit
               ~spawn:false)
        | "Schedule" ->
          Some
            (task_builtin
               ~name:"after_ms"
               ~op:"Schedule.after_ms_json"
               ~parameters:[ S.TInt; S.json_ty ]
               ~result:S.TString
               ~spawn:true)
        | _ -> None
      in
      Option.map replacement ~f:(fun replacement ->
        { entry with
          exports =
            List.map entry.exports ~f:(fun value ->
              if String.equal value.name replacement.name then replacement else value)
        }))
  in
  Surface.merge
    { Surface.empty with
      modules = invocation :: overrides
    ; type_aliases =
        tool_v1.type_aliases
        @ List.map
            [ "tool_invocation", invocation_event_ty
            ; "completion", completion_ty
            ; "work_completion", work_completion_ty
            ; "moderator_event", moderator_event_ty
            ]
            ~f:(fun (name, body) -> Surface.{ name; body })
    }
    Surface.moderator_surface
;;

let moderator_entrypoints =
  [ "initial_state", S.TVar "state"
  ; ( "on_event"
    , S.TFun
        ([ S.context_ty; S.TVar "state"; moderator_event_ty ], S.task_ty (S.TVar "state"))
    )
  ]
;;

let delegated_moderator_v1 =
  { moderator_v1 with
    globals =
      List.filter moderator_v1.globals ~f:(fun b -> not (String.equal b.S.name "print"))
  ; modules =
      List.filter moderator_v1.modules ~f:(fun m ->
        not (List.mem [ "Model"; "Process" ] m.S.name ~equal:String.equal))
  }
;;
