(version 1)
(server ((data_dir ./data) (unix_socket ./agent.sock) (http ((enabled false)))))
(workspaces
 (((id examples)
   (source (physical ./public))
   (access shared_write)
   (prompt_limits
    (((prompt generated) (max_root_agents 2) (overflow queue))
     ((prompt authored) (max_root_agents 2) (overflow queue)))))))
(prompts
 (((id generated)
   (path ./agent.chatmd)
   (description "Generated persisted specialists")
   (allowed_workspaces (examples)) (permission_profile interactive) (enabled true))
  ((id authored)
   (path ./authored-agent.chatmd)
   (description "Authored optional and persistent specialists")
   (allowed_workspaces (examples)) (permission_profile interactive) (enabled true))))
(permission_profiles
 (((id interactive) (tool_default ask) (approval_timeout none)
   (approval_fallback deny) (manifest_authorization require_grant))))
(manifest_grants ())
