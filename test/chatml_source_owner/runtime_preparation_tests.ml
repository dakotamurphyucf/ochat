open Core
open Support
module Preparation = Agent_session.Runtime_script_preparation
module Revision = Agent_session.Prompt_revision

let policy ?(max_custody_work_steps = 1_000_000) () =
  let planning =
    Chat_response.Script_contract_plan.Limits.create
      ~max_programs:16
      ~max_metadata_bytes:100_000
      ~max_work_steps:1_000_000
    |> Or_error.ok_exn
  in
  let restoration =
    Chatml_prompt_programs.Limits.create
      ~max_programs:16
      ~max_parser_inputs:32
      ~max_logical_sources:64
      ~max_total_input_bytes:1_000_000
      ~project_work_steps:1_000_000
      ~document:Document_schema.Limits.default
    |> Or_error.ok_exn
  in
  Preparation.create
    ~compilation:Chatml_compilation.default_limits
    ~planning
    ~restoration
    ~max_custody_work_steps
  |> Or_error.ok_exn
;;

let%test_unit "owned revisions require explicit policy and actual parser attachments" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>owned revision</user>";
    Eio.Switch.run (fun sw ->
      let definition = definition root in
      let catalog = catalog env parent in
      let candidate = prepare catalog sw [ definition ] |> Or_error.ok_exn in
      Catalog.install_owned catalog candidate |> Or_error.ok_exn;
      let original = revision catalog definition in
      let detached =
        Revision.create
          ~definition:(Revision.definition original)
          ~artifact:(Revision.artifact original)
          ~materialized_tree:(Revision.materialized_tree original)
          ~elements:(Revision.elements original)
      in
      let generous = policy () in
      let exhausted = policy ~max_custody_work_steps:1 () in
      List.iter
        [ None, false; Some generous, true; Some exhausted, false ]
        ~f:(fun (policy, sufficient) ->
          List.iter [ false; true ] ~f:(fun attached ->
            let invoked = ref false in
            let revision = if attached then original else detached in
            let result =
              Preparation.with_revision
                policy
                revision
                ~env
                ~delegated_moderator:false
                ~checkpoint:ignore
                ~f:(fun _ ~compilation_limits:_ ~programs ->
                  assert (Option.is_none programs);
                  invoked := true;
                  Ok ())
            in
            let admitted = sufficient && attached in
            [%test_eq: bool] (Result.is_ok result) admitted;
            [%test_eq: bool] !invoked admitted));
      let identity = Failure "original runtime callback" in
      let observed =
        try
          ignore
            (Preparation.with_revision
               (Some generous)
               original
               ~env
               ~delegated_moderator:false
               ~checkpoint:ignore
               ~f:(fun _ ~compilation_limits:_ ~programs:_ -> raise identity));
          failwith "callback failure disappeared"
        with
        | exn -> exn
      in
      assert (phys_equal observed identity)))
;;
