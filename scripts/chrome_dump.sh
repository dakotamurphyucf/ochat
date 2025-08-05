#!/usr/bin/env zsh
# -----------------------------------------------------------------------------
# chrome_dump – dump the DOM of a web page using a headless Google Chrome.
#
# Usage:
#   chrome_dump <url> [outfile]
#
#   <url>     – The page to fetch.
#   [outfile] – Optional path to write the resulting HTML. If omitted, the HTML
#               is printed to standard output.
# -----------------------------------------------------------------------------

# Abort on any error and treat unset variables as an error.
set -euo pipefail


# ----------------------------- argument parsing ------------------------------
if [[ $# -lt 1 ]]; then
  print -u2 "Usage: $(basename "$0") <url> [outfile]"
  exit 1
fi

url=$1        # required
out=${2:-}    # optional


# ----------------------------- chrome command --------------------------------
# ----------------------------- locate chrome ---------------------------------

# Helper: find a suitable Chrome/Chromium executable depending on the platform.
autoload -U is-at-least

find_chrome() {
  local candidate

  case "$(uname -s)" in
    Darwin)
      # Standard macOS install path.
      candidate='/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
      [[ -x $candidate ]] && { echo "$candidate"; return 0; }
      ;;
    Linux)
      # Common package names across distributions.
      for candidate in google-chrome google-chrome-stable chromium-browser chromium; do
        candidate=$(command -v "$candidate" 2>/dev/null || true)
        [[ -n $candidate && -x $candidate ]] && { echo "$candidate"; return 0; }
      done
      ;;
  esac

  return 1 # not found
}

chrome_app_path=$(find_chrome) || {
  print -u2 "chrome_dump: Could not locate a Chrome/Chromium executable. Please install Google Chrome or Chromium and ensure it is on your PATH.";
  exit 1;
}

res=$("$chrome_app_path" \
        --headless=new \
        --disable-gpu \
        --no-sandbox \
        --user-agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36" \
        --disable-blink-features=AutomationControlled \
        --virtual-time-budget=30000 \
        --dump-dom "$url")

# ----------------------------- output handling -------------------------------
if [[ -n $out ]]; then
  # Use print -r to avoid interpreting backslashes.
  print -r -- "$res" > "$out"
else
  print -r -- "$res"
fi
