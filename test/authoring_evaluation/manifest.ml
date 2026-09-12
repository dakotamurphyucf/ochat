(* Export prompts, policy selectors, fixed limits and predeclared thresholds.
   There is no provider or execution path in this command. *)
let () =
  let module Tasks = Authoring_evaluation.Tasks in
  print_endline
    (Jsonaf.to_string
       (`Object
           [ "suite_sha256", `String Tasks.fingerprint
           ; "manifest", Tasks.manifest
           ; "real_model_evaluation", `String "not_run"
           ]))
;;
