#!/usr/bin/env bash

set -euo pipefail

mode="${1:-pretty}"

case "$mode" in
  pretty|follow|color|color-follow)
    shift
    ;;
  *)
    ;;
esac

file="${1:-.chatmd/chatml-runtime.log}"

norm_filter='
  def norm:
    if type == "string" then
      gsub("\\\\n"; "\n") | gsub("\\\\t"; "\t")
    else .
    end;
  {
    timestamp: .timestamp,
    component: .component,
    level: (if .level then .level else "" end),
    message: (.message | norm)
  }
'

color_component() {
  case "$1" in
    chatml_runtime) printf '\033[1;36m' ;;
    moderator_manager) printf '\033[1;35m' ;;
    script_log) printf '\033[1;33m' ;;
    print) printf '\033[1;32m' ;;
    chat_tui) printf '\033[1;34m' ;;
    *) printf '\033[0;37m' ;;
  esac
}

color_level() {
  case "$1" in
    debug) printf '\033[0;36m' ;;
    info) printf '\033[0;32m' ;;
    warn) printf '\033[0;33m' ;;
    error) printf '\033[0;31m' ;;
    *) printf '\033[0m' ;;
  esac
}

print_record() {
  local timestamp="$1"
  local component="$2"
  local level="$3"
  local message="$4"

  printf '[%s] %s' "$timestamp" "$component"
  if [[ -n "$level" ]]; then
    printf ' [%s]' "$level"
  fi
  printf '\n%s\n----------------------------------------\n' "$message"
}

print_record_color() {
  local timestamp="$1"
  local component="$2"
  local level="$3"
  local message="$4"
  local reset=$'\033[0m'
  local ccomp
  local clvl

  ccomp="$(color_component "$component")"
  clvl="$(color_level "$level")"

  printf '[%s] %b%s%b' "$timestamp" "$ccomp" "$component" "$reset"
  if [[ -n "$level" ]]; then
    printf ' %b[%s]%b' "$clvl" "$level" "$reset"
  fi
  printf '\n%s\n----------------------------------------\n' "$message"
}

render_stream() {
  local colorize="${1:-false}"
  while IFS= read -r encoded; do
    [[ -z "$encoded" ]] && continue
    local decoded
    local timestamp
    local component
    local level
    local message
    decoded="$(printf '%s' "$encoded" | base64 --decode)"
    timestamp="$(printf '%s' "$decoded" | jq -r '.timestamp')"
    component="$(printf '%s' "$decoded" | jq -r '.component')"
    level="$(printf '%s' "$decoded" | jq -r 'if .level then .level else "" end')"
    message="$(printf '%s' "$decoded" | jq -r '.message')"
    if [[ "$colorize" == "true" ]]; then
      print_record_color "$timestamp" "$component" "$level" "$message"
    else
      print_record "$timestamp" "$component" "$level" "$message"
    fi
  done
}

case "$mode" in
  pretty)
    jq -r "$norm_filter | @base64" "$file" | render_stream false
    ;;
  follow)
    tail -f "$file" | jq -r "$norm_filter | @base64" | render_stream false
    ;;
  color)
    jq -r "$norm_filter | @base64" "$file" | render_stream true
    ;;
  color-follow)
    tail -f "$file" | jq -r "$norm_filter | @base64" | render_stream true
    ;;
  *)
    cat >&2 <<EOF
Usage:
  $0 [pretty|follow|color|color-follow] [path-to-chatml-runtime.log]

Examples:
  $0
  $0 pretty .chatmd/chatml-runtime.log
  $0 follow
  $0 color
  $0 color-follow
EOF
    exit 2
    ;;
esac
