open! Core
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Compilation = Chatml_compilation
module Overlay = Authoring_entry_recomposition

module Limits = struct
  type t =
    { max_example_bytes : int
    ; recompose : Build.Limits.t
    ; max_dependency_depth : int
    ; compile : Compilation.limits
    }

  let create
        ~max_example_bytes
        ~max_recomposition_work_steps
        ~max_recomposition_input_bytes
        ~max_dependency_depth
        ~compile
    =
    let open Or_error.Let_syntax in
    if
      max_example_bytes <= 0
      || max_example_bytes > 4096
      || max_recomposition_work_steps <= 0
      || max_recomposition_input_bytes <= 0
      || max_dependency_depth <= 0
    then Or_error.error_string "invalid checked-template limits"
    else (
      let%bind recompose =
        Build.Limits.create
          ~max_work_steps:max_recomposition_work_steps
          ~max_input_bytes:(Some max_recomposition_input_bytes)
          ~document:Document_schema.Limits.default
      in
      match Compilation.validate_limits compile with
      | Error _ -> Or_error.error_string "invalid checked-template compiler policy"
      | Ok () -> Ok { max_example_bytes; recompose; max_dependency_depth; compile })
  ;;

  let max_example_bytes t = t.max_example_bytes
end

module Error = struct
  type t =
    | Invalid_selected_entry
    | Incompatible_selected_contract
    | Recomposition of Build.Error.t
    | Compile_failure of Compilation.error
    | Budget_exceeded
end

module Witness = struct
  type t =
    { example : string
    ; target : Contract.Target.t
    ; base_fingerprint : Captured.Digest.t
    ; selected_contract : Captured.Digest.t
    ; environment : string
    ; original_entry_id : Captured.Id.t
    ; original_entry_digest : Captured.Digest.t
    ; example_digest : Captured.Digest.t
    ; checked_input_fingerprint : Captured.Digest.t
    }

  let example t = t.example
  let target t = t.target
  let base_fingerprint t = t.base_fingerprint
  let selected_contract t = t.selected_contract
  let environment t = t.environment
  let original_entry_id t = t.original_entry_id
  let original_entry_digest t = t.original_entry_digest
  let example_digest t = t.example_digest
  let checked_input_fingerprint t = t.checked_input_fingerprint
end

type outcome =
  | Checked of Witness.t
  | Inapplicable

let example = function
  | Contract.Target.One_off_v1 -> {|let main input = Task.pure(input)|}
  | Tool_v1 | Developer_tool_v1 -> {|let run ctx input = Task.pure(`Complete(input))|}
  | Moderator_v1 | Delegated_moderator_v1 ->
    {|let initial_state = 0
let on_event ctx state event = Task.pure(state)|}
;;

let required_entrypoints = function
  | Contract.Target.One_off_v1 -> [ "main" ]
  | Tool_v1 | Developer_tool_v1 -> [ "run" ]
  | Moderator_v1 | Delegated_moderator_v1 -> [ "initial_state"; "on_event" ]
;;

let prepare build ~contract ~limits ~env ~checkpoint =
  let open Result.Let_syntax in
  match Contract.kind contract with
  | Contract.Kind.Native -> Ok Inapplicable
  | Standard target ->
    let example = example target in
    if String.length example > limits.Limits.max_example_bytes
    then Error Error.Budget_exceeded
    else (
      let original = Build.prepared build in
      let entry = Prepared.entry original in
      let selected_contract = Contract.fingerprint contract ~checkpoint in
      if
        not
          (Captured.Digest.equal (Prepared.compiler_contract original) selected_contract)
      then Error Error.Incompatible_selected_contract
      else (
        let%bind original_entry =
          Prepared.find_source original entry
          |> Result.of_option ~error:Error.Invalid_selected_entry
        in
        let%bind candidate =
          Overlay.recompose_entry
            build
            ~contract
            ~contents:example
            ~max_body_bytes:limits.max_example_bytes
            ~limits:limits.recompose
            ~checkpoint
          |> Result.map_error ~f:(function
            | Overlay.Error.Invalid_selected_entry -> Error.Invalid_selected_entry
            | Body_limit | Lock_identity_unavailable -> Error.Budget_exceeded
            | Recomposition error -> Error.Recomposition error)
        in
        let prepared = Build.prepared candidate in
        let%bind compiled =
          Compilation.compile_prepared_with_contract_and_policy
            prepared
            ~contract
            ~policy:(Compilation.Bounded limits.compile)
            ~max_dependency_depth:limits.max_dependency_depth
            ~env
            ()
          |> Result.map_error ~f:(fun error -> Error.Compile_failure error)
        in
        let actual_names =
          Chatml_linked_program.entrypoints compiled
          |> List.map ~f:Chatml_linked_program.Entrypoint.name
        in
        if
          not
            (List.for_all (required_entrypoints target) ~f:(fun name ->
               List.mem actual_names name ~equal:String.equal))
        then Error Error.Invalid_selected_entry
        else (
          let example_digest = Captured.Digest.of_contents example ~checkpoint in
          let witness : Witness.t =
            { example
            ; target
            ; base_fingerprint = Prepared.fingerprint original
            ; selected_contract
            ; environment = Prepared.Environment.name (Prepared.environment original)
            ; original_entry_id = entry
            ; original_entry_digest = Captured.digest original_entry
            ; example_digest
            ; checked_input_fingerprint = Prepared.fingerprint prepared
            }
          in
          Ok (Checked witness))))
;;
