(version 1)
(server
 ((data_dir ./private-data)
  (unix_socket ./agent.sock)
  (http ((enabled false)))
  (durability
   ((journal_flush each)
    (journal_flush_ms 50)
    (snapshot_every_events 100)
    (snapshot_every_ms 5000)))
  (project_sources
   ((external_roots
     (../../libraries/recovery-rollover
      ../../libraries/lifecycle
      ../lifecycle-composition/components/review))))
  (recovery_capacity
   ((mode manual)
    (host_total 8192)
    (targets
     (((prompt local-recovery)
       (profile first-party-openai-responses)
       (model gpt-4.1)
       (encoder openai_responses_v1)
       (total 16384)
       (input 6000)
       (output 500)
       (session_total 7000)
       (omitted_output_default 60)
       (checkpoint_reserve 512)
       (warning_lead 256)
       (estimate_margin 100))))))))
(workspaces
 (((id recovery-review)
   (source (physical .))
   (access read_only)
   (prompt_limits
    (((prompt local-recovery) (max_root_agents 1) (overflow reject)))))))
(prompts
 (((id local-recovery)
   (path ./agent.chatmd)
   (description "Recover one decision across a checkpointed local rollover")
   (allowed_workspaces (recovery-review))
   (permission_profile recovery-tutorial)
   (enabled true))))
(permission_profiles
 (((id recovery-tutorial)
   (tool_default allow)
   (approval_timeout none)
   (approval_fallback deny)
   (manifest_authorization deny))))
(manifest_grants ())
