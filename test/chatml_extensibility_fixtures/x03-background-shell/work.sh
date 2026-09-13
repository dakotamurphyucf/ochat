#!/bin/sh
set -eu
# The external mutation deliberately precedes completion; cancellation cannot
# undo it. The runtime owns this foreground shell and its sleep subprocesses.
printf '%s\n' "$$" > fixture-work.pid
printf 'started\n' >> fixture-work.started
while [ ! -f fixture-work.release ]; do
  /bin/sleep 0.02
done
printf '{"fixture":"complete"}\n'
printf 'fixture diagnostic\n' >&2
