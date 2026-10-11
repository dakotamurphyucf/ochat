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

type t =
  | Complete of Snapshot.t
  | Failed of Partial.t

let analyze input ~contract ~max_dependency_depth ~limits ~checkpoint =
  let budget = Budget.create limits in
  Budget.run budget ~checkpoint ~f:(fun ~checkpoint ->
    checkpoint ();
    if
      Prepared.Accounting.input_bytes (Prepared.accounting input)
      > Limits.max_input_bytes limits
    then Budget.reject Input_limit;
    if max_dependency_depth <= 0 then Budget.reject Invalid_dependency_depth;
    let selected_contract = Contract.fingerprint contract ~checkpoint in
    if
      not
        (Chatml_source.Digest.equal selected_contract (Prepared.compiler_contract input))
    then Budget.reject Incompatible_contract;
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
