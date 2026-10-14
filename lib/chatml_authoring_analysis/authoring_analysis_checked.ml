open Core
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Semantics = Chatml_semantics
module Compilation = Chatml_compilation
module Captured = Chatml_source

module Policy = struct
  type t =
    { compilation : Compilation.limits
    ; semantic : Semantics.Limits.t
    ; max_dependency_depth : int
    ; max_results : int
    ; max_projected_diagnostics : int
    ; max_projected_sites : int
    ; max_projection_sources : int
    ; max_position_index_entries : int
    ; max_projection_source_bytes : int
    }

  let create
        ~compilation
        ~semantic
        ~max_dependency_depth
        ~max_results
        ~max_projected_diagnostics
        ~max_projected_sites
        ~max_projection_sources
        ~max_position_index_entries
        ~max_projection_source_bytes
    =
    match Compilation.validate_limits compilation with
    | Error _ -> Or_error.error_string "invalid authoring analysis compiler policy"
    | Ok () ->
      if
        List.exists
          [ max_dependency_depth
          ; max_results
          ; max_projected_diagnostics
          ; max_projected_sites
          ; max_projection_sources
          ; max_position_index_entries
          ; max_projection_source_bytes
          ]
          ~f:(fun value -> value <= 0)
      then Or_error.error_string "invalid authoring analysis policy"
      else
        Ok
          { compilation
          ; semantic
          ; max_dependency_depth
          ; max_results
          ; max_projected_diagnostics
          ; max_projected_sites
          ; max_projection_sources
          ; max_position_index_entries
          ; max_projection_source_bytes
          }
  ;;

  let max_results t = t.max_results
  let compilation t = t.compilation
  let semantic t = t.semantic
  let max_dependency_depth t = t.max_dependency_depth
end

module Error = struct
  type t =
    | Context of Chatml_authoring_context.Error.t
    | Compiler of Compilation.Analysis_error.t
    | Receipt_mismatch
    | Result_identity_mismatch
    | Stale_generation
    | Policy_mismatch
    | Invalid_occupancy
    | Retention_limit

  let sexp_of_t = function
    | Context _ -> Sexp.Atom "context"
    | Compiler (Compilation.Analysis_error.Policy _) -> Sexp.Atom "compiler_policy"
    | Compiler (Compilation.Analysis_error.Analysis _) -> Sexp.Atom "compiler_analysis"
    | Receipt_mismatch -> Sexp.Atom "receipt_mismatch"
    | Result_identity_mismatch -> Sexp.Atom "result_identity_mismatch"
    | Stale_generation -> Sexp.Atom "stale_generation"
    | Policy_mismatch -> Sexp.Atom "policy_mismatch"
    | Invalid_occupancy -> Sexp.Atom "invalid_occupancy"
    | Retention_limit -> Sexp.Atom "retention_limit"
  ;;
end

let same_environment left right =
  String.equal (Prepared.Environment.name left) (Prepared.Environment.name right)
  && Captured.Digest.equal
       (Prepared.Environment.required_contract left)
       (Prepared.Environment.required_contract right)
;;

module Checked = struct
  type status =
    | Complete of Semantics.Snapshot.t
    | Failed of Semantics.Partial.t

  type t =
    { status : status
    ; input_fingerprint : Captured.Digest.t
    ; required_contract : Captured.Digest.t
    ; selected_contract : Captured.Digest.t
    ; environment : Prepared.Environment.t
    }

  let status t = t.status
  let input_fingerprint t = t.input_fingerprint
  let required_contract t = t.required_contract
  let selected_contract t = t.selected_contract
  let environment t = t.environment

  let diagnostics t =
    match t.status with
    | Complete snapshot -> Semantics.Snapshot.diagnostics snapshot
    | Failed partial -> Semantics.Partial.diagnostics partial
  ;;

  let analyze_build build ~policy ~env ~target ~checkpoint =
    let prepared = Build.prepared build in
    checkpoint ();
    match
      Compilation.analyze_prepared
        prepared
        ~limits:policy.Policy.compilation
        ~semantic_limits:policy.semantic
        ~max_dependency_depth:policy.max_dependency_depth
        ~env
        ~target
        ()
    with
    | Error error -> Error (Error.Compiler error)
    | Ok semantics ->
      checkpoint ();
      let status, identity =
        match semantics with
        | Semantics.Complete snapshot ->
          Complete snapshot, Semantics.Snapshot.identity snapshot
        | Semantics.Failed partial -> Failed partial, Semantics.Partial.identity partial
      in
      let expected_fingerprint = Prepared.fingerprint prepared in
      let expected_contract = Prepared.compiler_contract prepared in
      let expected_environment = Prepared.environment prepared in
      if
        (not
           (Captured.Digest.equal
              (Semantics.Identity.input_fingerprint identity)
              expected_fingerprint))
        || (not
              (Captured.Digest.equal
                 (Semantics.Identity.required_contract identity)
                 expected_contract))
        || (not
              (Captured.Digest.equal
                 (Semantics.Identity.selected_contract identity)
                 expected_contract))
        || not
             (same_environment
                (Semantics.Identity.environment identity)
                expected_environment)
      then Error Error.Result_identity_mismatch
      else
        Ok
          { status
          ; input_fingerprint = expected_fingerprint
          ; required_contract = Semantics.Identity.required_contract identity
          ; selected_contract = Semantics.Identity.selected_contract identity
          ; environment = expected_environment
          }
  ;;
end
