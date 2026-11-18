open! Core
module Parent = Agent_session.Recurrence_target.Parent

let matches parent (spec : Agent_session.Session_state.Spec.t) =
  Document_schema.Json.equal
    (Agent_session.Workspace_instance.to_jsonaf spec.workspace_instance)
    (Agent_session.Workspace_instance.to_jsonaf (Parent.workspace parent))
  && String.equal spec.permission_profile (Parent.permission_profile parent)
  && String.equal spec.permission_profile_digest (Parent.permission_profile_digest parent)
  && Option.equal String.equal spec.runtime_policy (Parent.runtime_policy parent)
  && Int64.equal spec.configuration_revision (Parent.configuration_revision parent)
  && Document_schema.Json.equal
       (Inference.Selection.to_json spec.inference_target)
       (Inference.Selection.to_json (Parent.inference_selection parent))
;;
