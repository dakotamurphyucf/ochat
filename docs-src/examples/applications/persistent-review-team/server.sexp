(version 1)
(server
 ((data_dir ./private-data)
  (unix_socket ./agent.sock)
  (http ((enabled false)))))
(workspaces
 (((id lantern)
   (source (physical ./sample-project))
   (access read_only)
   (prompt_limits (((prompt team) (max_root_agents 1) (overflow queue)))))))
(prompts
 (((id team)
   (path ./team.chatmd)
   (description "Coordinate a persistent Lantern review team")
   (allowed_workspaces (lantern))
   (permission_profile reader)
   (enabled true))))
(permission_profiles
 (((id reader)
   (tool_default allow)
   (approval_timeout none)
   (approval_fallback deny)
   (manifest_authorization require_grant))))
(manifest_grants ())
