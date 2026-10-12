open Core

(** Durable current-role selection over already admitted custody. Neither case is
    executable until [Restored_programs.restore] succeeds with the same plan. *)
type t =
  | Legacy of Chatml_prompt_programs.t
  | Selected of Chatml_prompt_programs.t

(** Require the same actual current Root, then match only the plan's actual
    executable declarations through the custody owner's streaming matcher.
    Prefer a unique exact original binding/contract; otherwise require a unique
    preserved declaration-slot/contract. Ambiguous aliases, missing roles and
    partially recorded plans reject. Inactive Provided graphs are records too;
    they never become an implicit-script fallback.

    [Legacy] requires no record for any current executable slot, no configured
    declaration and no active selection. [Selected] retains a nonempty complete
    active mapping. Both preserve the entire inactive inventory and unknown fields
    through [Chatml_prompt_programs.reroot]. The caller must adopt this selection
    before child fingerprint/reservation, restore it authentically, and require
    [Some] for [Selected] or [None] for [Legacy]. No source loading, preparation,
    compilation, contract retargeting, tool admission or initializer occurs here.

    [max_work_steps] is one positive whole-operation cooperative allowance:
    checkpoint calls during validation, matching, bounded qualified-ID equality,
    mapping, reroot and diagnostics count before work. It is separate from semantic
    graph restoration work and does not restart an outer startup deadline. Plan
    rows are already admitted; new state/mapping cells are charged before retention.
    Original checkpoint exceptions, cancellation and backtraces propagate. *)
val select
  :  Script_contract_plan.t
  -> custody:Chatml_prompt_programs.t
  -> authored:Chatmd_source_bundle.t
  -> limits:Chatml_prompt_programs.Limits.t
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t, Chatmd_shell_spec.Diagnostic.t list) Result.t
