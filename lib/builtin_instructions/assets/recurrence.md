# Recurring work

Treat the recurring controller and each occurrence as distinct lifetimes.
Completing one occurrence does not stop future occurrences. Use the supplied
controller controls for an explicit pause or stop; do not silently re-enable a
stopped controller on restart.

Keep schedule policies explicit. Fixed delay measures from completion; fixed rate
preserves a schedule phase. Calendar schedules need a named timezone and a stated
policy for clock gaps, repeated times, missed occurrences and overlap. Do not
substitute the machine's ambient timezone or silently replay unbounded missed work.

A timer notification is not evidence that work completed. Report only observed
launches and outcomes, including interruptions and work that could not run while
the host was unavailable. Configure or change recurrence only through controls
actually supplied and authorized in this session; importing this text creates no
schedule or background execution.
