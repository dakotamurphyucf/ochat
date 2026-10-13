open Core
module Captured = Chatml_source
module Frozen = Chatml_typechecker.Type_snapshot
module Sites = Chatml.Chatml_syntax_sites
module Identity = Chatml_semantic_identity
module Snapshot = Chatml_semantic_snapshot
module Partial = Chatml_semantic_partial
module Limits = Chatml_semantic_limits
module Error = Chatml_semantic_error
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Budget = Chatml_semantic_budget
module Publication = Chatml_semantic_publication
module Driver = Chatml_checked_graph
module Provisional_input = Chatml_semantic_provisional_input

type t =
  | Complete of Snapshot.t
  | Failed of Partial.t

module Provisional = struct
  type t =
    { full_input_fingerprint : Captured.Digest.t
    ; full_source_digest : Captured.Digest.t
    ; prefix_source_digest : Captured.Digest.t
    ; selected_source : Captured.Id.t
    ; end_byte : int
    ; snapshot : Snapshot.t
    }

  let full_input_fingerprint t = t.full_input_fingerprint
  let full_source_digest t = t.full_source_digest
  let prefix_source_digest t = t.prefix_source_digest
  let selected_source t = t.selected_source
  let end_byte t = t.end_byte
  let snapshot t = t.snapshot
end

module Prefix = struct
  type unavailable =
    | No_complete_statement
    | Unsupported_source_phase
    | Unchecked_source
    | Prefix_rejected of Partial.Invalidity.t
    | Invalid_rebased_metadata
  [@@deriving sexp_of]

  type t =
    | Provisional of Provisional.t
    | Unavailable of unavailable
end

let admit input ~contract ~max_dependency_depth ~limits ~checkpoint =
  checkpoint ();
  if
    Prepared.Accounting.input_bytes (Prepared.accounting input)
    > Limits.max_input_bytes limits
  then Budget.reject Input_limit;
  if max_dependency_depth <= 0 then Budget.reject Invalid_dependency_depth;
  let selected_contract = Contract.fingerprint contract ~checkpoint in
  if not (Captured.Digest.equal selected_contract (Prepared.compiler_contract input))
  then Budget.reject Incompatible_contract;
  selected_contract
;;

let analyze input ~contract ~max_dependency_depth ~limits ~checkpoint =
  let budget = Budget.create limits in
  Budget.run budget ~checkpoint ~f:(fun ~checkpoint ->
    let selected_contract =
      admit input ~contract ~max_dependency_depth ~limits ~checkpoint
    in
    let identity = Identity.create input ~selected_contract in
    match
      Driver.check
        input
        ~recording:(Limits.recording limits)
        ~contract
        ~max_dependency_depth
        ~checkpoint
    with
    | Ok checked ->
      let snapshot =
        Publication.publish input checked ~identity ~limits ~budget ~checkpoint
      in
      checkpoint ();
      Complete snapshot
    | Error failure ->
      let diagnostic = Driver.Failure.diagnostic failure in
      (match Driver.Failure.kind failure with
       | Sites error -> Budget.reject (Sites error)
       | Recording error -> Budget.reject (Recording error)
       | Admission -> Budget.reject (Invalid_input diagnostic)
       | Compiler _ ->
         let partial =
           Partial.create
             input
             ~identity
             ~invalidity:(Driver.Failure.invalidity failure |> Option.value_exn)
             ~parsed:(Driver.Failure.parsed failure)
             ~diagnostic
             ~budget
             ~checkpoint
         in
         checkpoint ();
         Failed partial))
;;

let analyze_prefix
      input
      ~source
      ~cursor_byte
      ~contract
      ~max_dependency_depth
      ~limits
      ~checkpoint
  =
  let budget = Budget.create limits in
  Budget.run budget ~checkpoint ~f:(fun ~checkpoint ->
    let selected_contract =
      admit input ~contract ~max_dependency_depth ~limits ~checkpoint
    in
    match Prepared.find_source input source with
    | None -> Prefix.Unavailable Unsupported_source_phase
    | Some original ->
      let is_implementation =
        Captured.Id.equal (Prepared.entry input) source
        || List.exists (Prepared.definitions input) ~f:(fun definition ->
          checkpoint ();
          Captured.Id.equal (Prepared.Definition.implementation definition) source)
      in
      let is_project_metadata =
        match Prepared.provenance input with
        | Implicit -> false
        | Project { declaration; lock } ->
          Captured.Id.equal declaration source
          || Option.value_map lock ~default:false ~f:(Captured.Id.equal source)
      in
      if (not is_implementation) || is_project_metadata
      then Prefix.Unavailable Unsupported_source_phase
      else (
        let witness =
          Chatml.Chatml_parse.Top_level_prefix.find original ~cursor_byte ~checkpoint
          |> function
          | Ok witness -> witness
          | Error Chatml.Chatml_parse.Top_level_prefix.Invalid_cursor ->
            Budget.reject Error.Invalid_cursor
        in
        match witness with
        | None -> Prefix.Unavailable No_complete_statement
        | Some witness ->
          let end_byte = Chatml.Chatml_parse.Top_level_prefix.end_byte witness in
          (match
             Provisional_input.create
               input
               ~original
               ~end_byte
               ~max_input_bytes:(Limits.max_input_bytes limits)
               ~checkpoint
           with
           | Error () -> Prefix.Unavailable Invalid_rebased_metadata
           | Ok revised ->
             let identity = Identity.create revised ~selected_contract in
             (match
                Driver.Provisional.check
                  revised
                  ~recording:(Limits.recording limits)
                  ~contract
                  ~max_dependency_depth
                  ~checkpoint
              with
              | Error failure ->
                let diagnostic = Driver.Failure.diagnostic failure in
                (match Driver.Failure.kind failure with
                 | Sites error -> Budget.reject (Sites error)
                 | Recording error -> Budget.reject (Recording error)
                 | Admission -> Budget.reject (Invalid_input diagnostic)
                 | Compiler _ ->
                   let invalidity =
                     Driver.Failure.invalidity failure |> Option.value ~default:Dependency
                   in
                   Prefix.Unavailable (Prefix_rejected invalidity))
              | Ok checked ->
                let selected_was_checked =
                  List.exists (Driver.Provisional.parsed checked) ~f:(fun parsed ->
                    checkpoint ();
                    Captured.Id.equal (Captured.id (Driver.Parsed.source parsed)) source
                    && Chatml_configured_lookup.Phase.equal
                         (Driver.Parsed.phase parsed)
                         Implementation)
                in
                if not selected_was_checked
                then Prefix.Unavailable Unchecked_source
                else (
                  let snapshot =
                    Publication.publish_provisional
                      revised
                      checked
                      ~identity
                      ~limits
                      ~budget
                      ~checkpoint
                  in
                  checkpoint ();
                  Prefix.Provisional
                    { full_input_fingerprint = Prepared.fingerprint input
                    ; full_source_digest = Captured.digest original
                    ; prefix_source_digest =
                        Prepared.find_source revised source
                        |> Option.value_exn
                        |> Captured.digest
                    ; selected_source = source
                    ; end_byte
                    ; snapshot
                    })))))
;;
