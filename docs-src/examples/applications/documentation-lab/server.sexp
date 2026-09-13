(version 1)
(server
 ((data_dir ./private-data)
  (unix_socket ./agent.sock)
  (http ((enabled false)))))
(workspaces
 (((id lab)
   (source (physical ./workspace))
   (access shared_write)
   (prompt_limits (((prompt lab) (max_root_agents 1) (overflow queue)))))))
(prompts
 (((id lab)
   (path ./lab.chatmd)
   (description "Living documentation lab")
   (allowed_workspaces (lab))
   (permission_profile lab)
   (enabled true))))
(permission_profiles
 (((id lab)
   (tool_default allow)
   (approval_timeout none)
   (approval_fallback deny)
   (manifest_authorization assume_authorized))))
(manifest_grants ())
