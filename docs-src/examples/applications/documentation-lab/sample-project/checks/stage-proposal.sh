#!/bin/sh
set -eu

# Content is a literal argument, never evaluated as shell source. Only the known
# broken tutorial has a staging destination; original inputs remain read-only.
if [ "$#" -ne 2 ] || [ "$1" != broken ] || [ -z "$2" ]; then
  printf '%s\n' 'Expected broken and a non-empty proposed verification section.' >&2
  exit 2
fi
if [ ! -d staging ] || [ ! -f tutorials/broken.md ]; then exit 2; fi
{
  cat tutorials/broken.md
  printf '\n%s\n' "$2"
} > staging/broken.md
printf '%s\n' '{"tutorial_id":"broken","staged_path":"staging/broken.md","original_changed":false}'
