open Core
module C = Tool_capability
module M = Chatmd_shell_spec.Authoring_metadata

let helper_available capabilities helper =
  match C.find capabilities ~name:(M.helper_name helper) with
  | Ok binding -> Option.equal M.equal_helper (C.metadata binding).helper (Some helper)
  | Error _ -> false
;;

let describe ~host ~capabilities ~name ~description =
  match C.find capabilities ~name with
  | Error _ -> description
  | Ok binding ->
    (match (C.metadata binding).authoring with
     | None -> description
     | Some help ->
       let tasks =
         List.filter help.tasks ~f:(fun task ->
           Result.is_ok (Authoring_validation.task_surface host task)
           && Option.is_none (Authoring_validation.execution_unavailable_reason host task))
       in
       let reference =
         match helper_available capabilities M.Reference, tasks with
         | true, task :: _ ->
           Some
             ("Before authoring unfamiliar syntax or behavior, call "
              ^ M.helper_name Reference
              ^ " with version=1, operation=prepare, task="
              ^ M.task_id task
              ^ ", and query/topic_id/features/cursor/max_tokens all null. Use its \
                 feature "
              ^ "map to choose and read the relevant contracts and examples.")
         | _ -> None
       in
       let validation =
         Option.some_if
           (helper_available capabilities M.Validation && not (List.is_empty tasks))
           ("Check the completed source with "
            ^ M.helper_name Validation
            ^ " before execution; validation does not run the source.")
       in
       Some
         (String.concat
            ~sep:" "
            (List.filter_opt
               [ List.find_map
                   help.tasks
                   ~f:(Authoring_validation.execution_unavailable_reason host)
               ; description
               ; Some
                   ("Authoring reference package: "
                    ^ help.package
                    ^ ". Topics: "
                    ^ String.concat ~sep:", " help.topics
                    ^ ".")
               ; reference
               ; validation
               ])))
;;
