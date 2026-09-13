#!/bin/sh
set -eu

if [ "$#" -ne 2 ]; then
  printf '%s\n' 'Usage: check-tutorial.sh original|staged passing|broken' >&2
  exit 2
fi
phase=$1
tutorial=$2
case "$tutorial" in passing|broken) ;; *) exit 2 ;; esac
case "$phase" in
  original) source_file="tutorials/$tutorial.md" ;;
  staged)
    if [ "$tutorial" = broken ]; then
      source_file=staging/broken.md
    else
      source_file=tutorials/passing.md
    fi
    ;;
  *) exit 2 ;;
esac
if [ ! -f "$source_file" ]; then
  printf '%s\n' 'The selected tutorial file is missing.' >&2
  exit 2
fi

# These are deliberate mechanical conventions, not a semantic prose review.
if awk '
  /^## Verification$/ { heading = 1 }
  /^Expected result:/ { expected = 1 }
  END { exit !(heading && expected) }
' "$source_file"; then
  status=passed
  code=0
  summary='Verification heading and expected-result line are present.'
else
  status=failed
  code=1
  summary='Add a Verification heading and an Expected result line.'
fi
printf '{"tutorial_id":"%s","status":"%s","exit_code":%s,"evidence_path":"%s","summary":"%s"}\n' \
  "$tutorial" "$status" "$code" "$source_file" "$summary"
exit "$code"
