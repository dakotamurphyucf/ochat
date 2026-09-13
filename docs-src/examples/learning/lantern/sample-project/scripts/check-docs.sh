#!/bin/sh
# Run from sample-project. Inputs are fixed; only the selected check and report
# writing are configurable. Project code still needs an appropriate shell runtime.
set -eu

check=all
write_report=false
while [ "$#" -gt 0 ]; do
  case "$1" in
    --check)
      [ "$#" -ge 2 ] || { printf '%s\n' 'Missing check name' >&2; exit 2; }
      check=$2
      shift 2
      ;;
    --write-report) write_report=true; shift ;;
    *) printf '%s\n' "Unknown argument: $1" >&2; exit 2 ;;
  esac
done
case "$check" in
  all|setup|links|verification) ;;
  *) printf '%s\n' 'Choose all, setup, links or verification' >&2; exit 2 ;;
esac
[ -r docs/setup.md ] || { printf '%s\n' 'Missing docs/setup.md' >&2; exit 2; }
reference_exists=false
[ ! -f docs/reference.md ] || reference_exists=true

# Capture output separately from its exit status, so a failed check still emits
# its evidence. User-controlled text never becomes a JSON key or string here.
status=0
report=$(awk -v selected="$check" -v reference_exists="$reference_exists" '
  /^## Setup$/ { setup_heading = 1 }
  /sh scripts\/check-docs.sh/ { setup_command = 1 }
  /\]\(reference.md\)/ { reference_link = 1 }
  /^## Verification$/ { verification_heading = 1 }
  /^Expected result:/ { expected_result = 1 }

  function emit(key, title, passed) {
    if (selected != "all" && selected != key) return
    printf "%s{\"check\":\"%s\",\"status\":\"%s\"}", separator, title, passed ? "passed" : "failed"
    separator = ",\n"
    if (!passed) failed = 1
  }

  END {
    print "["
    emit("setup", "setup instructions", setup_heading && setup_command)
    emit("links", "source links", reference_link && reference_exists == "true")
    emit("verification", "verification steps", verification_heading && expected_result)
    print "\n]"
    exit failed ? 1 : 0
  }
' docs/setup.md) || status=$?

if [ "$write_report" = true ]; then
  printf '%s\n' "$report" > ../reports/latest.json
fi
printf '%s\n' "$report"
exit "$status"
