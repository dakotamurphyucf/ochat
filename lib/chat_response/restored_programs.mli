open Core

(** Restore only actual active programs under this host's authentic role plan.
    No filesystem/provider/cache/native grant or compilation. Exact Root and
    active binding/contract membership are checked by existing custody owner.
    [limits] is the host's semantic restoration allowance; structural artifact
    admission's project-work placeholder is not suitable for selected graphs.
    [None] preserves the lightweight implicit-script path only after validating the
    exact current parser Root and confirming no active selection, configured
    project declaration or retained program for an executable row of this plan.
    Unused parser declarations do not constitute requested execution. Retained
    records for unrelated Roots do not imply a selection; matching records with
    an omitted selection reject instead of silently selecting legacy code. [Some] requires the complete authentic plan; missing
    sibling graphs never silently fall back. Inactive custody is not selection.
    Original checkpoint exceptions/cancellation propagate unchanged. *)
val restore
  :  Script_contract_plan.t
  -> custody:Chatml_prompt_programs.t
  -> limits:Chatml_prompt_programs.Limits.t
  -> checkpoint:(unit -> unit)
  -> ( Chatml_prompt_programs.Restored.t option
       , Chatmd_shell_spec.Diagnostic.t list )
       Result.t
