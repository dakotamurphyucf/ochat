open! Core
module S = Chatml_builtin_spec
module L = Chatml_lang

let entrypoint = "on_context_rollover"
let record fields = S.TRecord (TRow_extend (fields, TRow_empty))
let variant cases = S.TVariant (TVariant_row_extend (cases, TVariant_row_empty))
let option ty = variant [ "None", []; "Some", [ ty ] ]

let window =
  record
    [ "server_id", S.TString
    ; "session_id", S.TString
    ; "generation", S.TInt
    ; "id", S.TString
    ]
;;

let checkpoint_quality =
  variant [ "Missing", []; "Checkpointed", [ S.TArray S.json_ty ]; "Archive_only", [] ]
;;

let continuation = variant [ "Continue", []; "Wait", []; "Finish", [] ]

let pressure =
  variant [ "Unknown", []; "Within_budget", []; "Near_limit", []; "Over_limit", [] ]
;;

let event =
  variant
    [ ( "Before_context_rollover"
      , [ record
            [ "intent_id", S.TString
            ; "window", window
            ; "expected_revision", S.TString
            ; "pressure", pressure
            ; "checkpoint", checkpoint_quality
            ]
        ] )
    ; ( "After_context_rollover"
      , [ record
            [ "intent_id", S.TString
            ; "previous", window
            ; "window", window
            ; "cut_revision", S.TString
            ; "checkpoint", checkpoint_quality
            ]
        ] )
    ]
;;

let before =
  variant
    [ "Veto", []
    ; ( "Proceed"
      , [ record
            [ "hints", option (S.TArray S.json_ty); "continuation", option continuation ]
        ] )
    ]
;;

let decision = variant [ "Before", [ before ]; "After", [ continuation ] ]

let scheme =
  let state = S.TVar "state" in
  S.TFun ([ S.context_ty; state; event ], S.task_ty (S.TTuple [ state; decision ]))
;;

let aliases =
  List.map
    [ "context_rollover_event", event
    ; "context_rollover_before", before
    ; "context_rollover_continuation", continuation
    ; "context_rollover_decision", decision
    ]
    ~f:(fun (name, body) -> Chatml_builtin_surface.{ name; body })
;;

let code = function
  | L.VVariant ("Continue", []) -> 0
  | VVariant ("Wait", []) -> 1
  | VVariant ("Finish", []) -> 2
  | _ -> failwith "Context_rollover.compose: invalid continuation"
;;

let builtin =
  S.
    { name = "Context_rollover"
    ; exports =
        [ { name = "compose"
          ; scheme = TFun ([ TArray continuation ], continuation)
          ; impl =
              (function
                | [ L.VArray values ] ->
                  let selected =
                    Array.fold values ~init:0 ~f:(fun selected value ->
                      Int.max selected (code value))
                  in
                  let name =
                    match selected with
                    | 0 -> "Continue"
                    | 1 -> "Wait"
                    | _ -> "Finish"
                  in
                  L.VVariant (name, [])
                | _ -> failwith "Context_rollover.compose: expected a continuation array")
          }
        ]
    }
;;
