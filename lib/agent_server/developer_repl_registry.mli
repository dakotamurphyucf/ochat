open! Core
module P = Agent_protocol
module Wire = P.Developer_repl_wire
module Repl = Developer_repl_epoch

(** One Host-owned, process-local comparison registry. Strings never
    reconstruct physical REPL authority. The registry lives across client
    reconnects and expires with the installed Host. It retains issued old
    Epoch/Revision IDs through Epoch's bounded reset/key tombstones and closed
    instance IDs through exact close retries; a new process has no old IDs. *)
type t

(** The installed Host reports its actual I=max_instances and
    K=max_keys_per_instance. At most I*(2*K+1) token IDs can be issued in its
    lifetime: one Instance, up to K Epochs and up to K Revisions per state.
    Checked arithmetic and [max_registry_entries] reject a mismatch before
    any REPL effect. Thus projection after an effect cannot fail merely because
    the logical ID capacity was exhausted. *)
val create : host:Developer_host.t -> limits:Wire.Limits.t -> (t, P.Error.t) result

val resolve_instance : t -> Wire.Wire_id.Instance.t -> (Repl.Instance.t, P.Error.t) result

val resolve_epoch
  :  t
  -> instance:Repl.Instance.t
  -> Wire.Wire_id.Epoch.t
  -> (Repl.Epoch.t, P.Error.t) result

val resolve_revision
  :  t
  -> instance:Repl.Instance.t
  -> epoch:Repl.Epoch.t
  -> Wire.Wire_id.Revision.t
  -> (Repl.Revision.t, P.Error.t) result

(** Stable reverse mappings for a returned authentic Host value. The same
    physical token always receives the same wire ID; projecting a cached
    historical result creates no new token. No caller-selected physical
    token can be registered through the wire. *)
val project_view : t -> Repl.View.t -> (Wire.View.t, P.Error.t) result

val project_submission
  :  t
  -> instance:Repl.Instance.t
  -> Repl.Submission.t
  -> (Wire.Submission.t, P.Error.t) result

(** Trusted same-Host authenticated projections. These helpers register no
    caller-selected physical token; the revision must already be retained
    under the exact issuing instance and epoch. *)
val project_type_inspection
  :  t
  -> instance:Repl.Instance.t
  -> epoch:Repl.Epoch.t
  -> Repl.Type_inspection.t
  -> (Wire.Type_inspection.t, P.Error.t) result

val project_reference_page
  :  t
  -> Repl.Task_reference_page.t
  -> (Wire.Reference_page.t, P.Error.t) result
