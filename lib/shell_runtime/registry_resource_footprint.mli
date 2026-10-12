open Core

(** Pure native-footprint checks over effective declarations, including actual
    secret material. No manifest SHA comparison or native path lookup occurs.
    Diagnostic Source_ref positions/digests/directories are excluded; retained
    original source bases continue to determine native resources. Actual code
    bodies/graph selections are not resource identity. *)
module Error : sig
  type t =
    { code : string
    ; runtime_id : string option
    ; field : string
    }
  [@@deriving sexp, equal]
end

(** Compare every effective runtime field except diagnostic [source]. In
    particular native policy structure, hook references/entrypoint wiring,
    lifecycle/failure behavior and fixed controls remain exact. Only compiled
    code behind those existing references can change. Walks/checks poll [budget]
    before input-sized work and allocate no parallel declaration projection. *)
val check_runtime
  :  Chatmd_shell_spec.Shell_spec.t
  -> candidate:Chatmd_shell_spec.Shell_spec.t
  -> budget:Registry_code_budget.t
  -> (unit, Error.t) Result.t

(** Exact tool specification except diagnostic [source]. Existing prepared
    script-file identity/bytes stay in the original registry without rereading. *)
val check_tool
  :  Chatmd_shell_spec.Shell_tool_spec.t
  -> candidate:Chatmd_shell_spec.Shell_tool_spec.t
  -> budget:Registry_code_budget.t
  -> (unit, Error.t) Result.t

(** Preserve ID/kind/surface/entrypoint/limits, excluding replaceable body digest
    and complete compiled execution fingerprint. *)
val check_extension
  :  Chatmd_shell_spec.Manifest.extension_script
  -> candidate:Chatmd_shell_spec.Manifest.extension_script
  -> budget:Registry_code_budget.t
  -> (unit, Error.t) Result.t

(** The original administrative policy is retained; candidate preparation cannot
    replace it with different authority. Compare its actual typed fields. *)
val check_admin_policy
  :  Admin_policy.t
  -> candidate:Admin_policy.t
  -> budget:Registry_code_budget.t
  -> (unit, Error.t) Result.t
