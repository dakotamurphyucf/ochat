(version 1)
(server
 ((data_dir ./data) (unix_socket ./agent.sock) (http ((enabled false)))
  (authoring_packages (./one-off.json ./standalone.json ./moderator.json ./children.json ./background.json))))
(workspaces
 (((id examples) (source (physical ./public)) (access shared_write)
   (prompt_limits (((prompt lab) (max_root_agents 2) (overflow queue)))))))
(prompts
 (((id lab) (path ./one-off.chatmd) (description "E10 authoring examples")
   (allowed_workspaces (examples)) (permission_profile interactive) (enabled true))))
(permission_profiles
 (((id interactive) (tool_default ask) (approval_timeout none)
   (approval_fallback deny) (manifest_authorization require_grant))))
(manifest_grants ())
