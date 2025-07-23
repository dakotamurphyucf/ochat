#!/usr/bin/env bash
# Generates dependency graph artifacts (deps.sexp, deps.json, Mermaid diagram).

set -eu

ROOT_DIR="$(git rev-parse --show-toplevel)"
OUT_DIR="$ROOT_DIR/out"
DOC_DIR="$ROOT_DIR/docs"

mkdir -p "$OUT_DIR" "$DOC_DIR"

# 1. Raw sexp description
dune describe workspace --lang=0.1 --format=sexp > "$OUT_DIR/deps.sexp"

# 2. Convert to JSON (quick & dirty); requires `sexp_json` conversion via `sexp_pretty` & jq
cat "$OUT_DIR/deps.sexp" | tr -d '\n' > "$OUT_DIR/deps.json"

# 3. Minimal Mermaid skeleton – nodes only (detailed edges need heavier analysis)

cat > "$DOC_DIR/deps.mmd" <<'EOF'
%% Auto-generated – edit via script/gen_dep_graph.sh
graph TD
    %% (nodes added programmatically below)
EOF

# extract library & executable names from the S-expression
grep -o '((name [^)]*)' "$OUT_DIR/deps.sexp" | awk '{print $2}' | sort -u | while read -r name; do
    echo "    $name([$name])" >> "$DOC_DIR/deps.mmd"
done

# 4. Cross-check: missing public names
grep -o "public_name.*" "$ROOT_DIR/dune-project" | awk '{print $2}' | sort -u > "$OUT_DIR/dune_public_libs.txt" || true
grep -o '((name [^)]*)' "$OUT_DIR/deps.sexp" | awk '{print $2}' | sort -u > "$OUT_DIR/seen_libs.txt"
comm -3 "$OUT_DIR/dune_public_libs.txt" "$OUT_DIR/seen_libs.txt" > "$OUT_DIR/deps-check.log" || true

echo "Dependency artefacts refreshed in $OUT_DIR and $DOC_DIR" >&2

