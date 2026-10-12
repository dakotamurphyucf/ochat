const fingerprintOwners = new Set(
  [
    "authoring_corpus",
    "declaration_coverage_data",
    "grammar_coverage_data",
    "native_coverage_data",
    "runtime_coverage_data",
    "semantic_coverage_data",
  ].map((name) => `lib/authoring_sources/${name}.ml`),
);
const surfaces = new Set([
  "one_off_v1",
  "tool_v1",
  "moderator_v1",
  "delegated_moderator_v1",
]);
const mappingNames = new Set(
  [
    "task",
    "string",
    "array",
    "option",
    "outcome",
    "json",
    "hashtbl",
    "global",
    "json_alias",
    "moderator_data",
    "host_effect",
    "runtime_control",
    "run",
    "background",
    "invocation_context",
  ].map((name) => `${name}_mappings`),
);

/** Pure path eligibility only. The caller must verify existing regular-file modes,
 * finite blob sizes and exact text decoding at both revisions before proving a diff. */
export function isDocumentationCandidate(path) {
  return (
    typeof path === "string" &&
    !path
      .split("/")
      .some((part) => part === ".." || part === "." || part === "") &&
    (fingerprintOwners.has(path) ||
      (path.startsWith("docs-src/") &&
        path.endsWith(".md") &&
        !path.startsWith("docs-src/examples/")))
  );
}

// Only top-level ordinary prose is disposable. Everything affecting block parsing
// or executable extraction stays exact. Unknown HTML/containers force full checks.
function markdownProof(text) {
  const result = [];
  let fence,
    inline,
    comment = false,
    frontmatter = false,
    prose = false;
  const lines = text.split("\n");
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line === "+++" || (inline && /^ {0,3}(`{3,}|~{3,})/.test(line)))
      return null;
    if (i === 0 && line === "---") frontmatter = true;
    if (frontmatter) {
      result.push(line);
      prose = false;
      if (i > 0 && (line === "---" || line === "...")) frontmatter = false;
      continue;
    }
    if (fence) {
      result.push(line);
      prose = false;
      const end = line.match(/^ {0,3}(`{3,}|~{3,})[ \t]*$/);
      if (end && end[1][0] === fence[0] && end[1].length >= fence.length)
        fence = undefined;
      continue;
    }
    if (comment) {
      result.push(line);
      prose = false;
      if (line.includes("-->")) {
        if (!line.trimEnd().endsWith("-->")) return null;
        comment = false;
      }
      continue;
    }
    const start = line.match(/^ {0,3}(`{3,}|~{3,})(.*)$/);
    if (start) {
      if (start[1][0] === "`" && start[2].includes("`")) return null;
      fence = start[1];
      result.push(line);
      prose = false;
      continue;
    }
    if (line.trimStart().startsWith("<!--")) {
      if (line.includes("-->") && !line.trimEnd().endsWith("-->")) return null;
      comment = !line.includes("-->");
      result.push(line);
      prose = false;
      continue;
    }
    if (/^\s*:::/.test(line)) return null;
    // Setext headings are deliberately unsupported.
    if (/^ {0,3}(?:=+|-+)\s*$/.test(line) && i > 0 && lines[i - 1].trim())
      return null;
    // Unchanged multiline inline-code spans stay protected too.
    let visible = line,
      protectedInline = Boolean(inline);
    if (inline) {
      const closing = visible.indexOf(inline);
      if (closing < 0) {
        result.push(line);
        prose = false;
        continue;
      }
      visible = visible.slice(closing + inline.length);
      inline = undefined;
    }
    while (visible.includes("`")) {
      const opening = visible.indexOf("`");
      const run = visible.slice(opening).match(/^`+/)[0];
      const closing = visible.indexOf(run, opening + run.length);
      if (closing < 0) {
        inline = run;
        protectedInline = true;
        break;
      }
      visible =
        visible.slice(0, opening) +
        "inline-code" +
        visible.slice(closing + run.length);
    }
    if (protectedInline) {
      result.push(line);
      prose = false;
      continue;
    }
    if (visible.includes("<") || visible.includes(">") || /[`~]{3}/.test(line))
      return null;
    const ordinary =
      (/^[^\s#>*+\-\d|:\[\\`~]/.test(visible) ||
        /^\[[^\]]+\]\([^\n]*\)/.test(visible)) &&
      !/!\[/.test(line);
    if (ordinary) {
      if (!prose) result.push("\0prose");
      prose = true;
    } else {
      result.push(line);
      prose = false;
    }
  }
  return fence || inline || comment || frontmatter
    ? null
    : JSON.stringify(result);
}

// A small lexical scanner, not an OCaml parser: it skips nested comments and
// keeps strings/characters opaque, so lookalike tuples inside them cannot qualify.
function tokens(text) {
  const out = [];
  let i = 0;
  while (i < text.length) {
    if (/\s/.test(text[i])) {
      i++;
      continue;
    }
    const start = i;
    if (text.startsWith("(*", i)) {
      let depth = 1;
      i += 2;
      while (i < text.length && depth) {
        if (text.startsWith("(*", i)) {
          depth++;
          i += 2;
        } else if (text.startsWith("*)", i)) {
          depth--;
          i += 2;
        } else i++;
      }
      if (depth) return null;
      continue;
    }
    if (text[i] === '"') {
      i++;
      while (i < text.length && text[i] !== '"') {
        if (text[i] === "\\") i++;
        i++;
      }
      if (i >= text.length) return null;
      i++;
      out.push({
        start,
        end: i,
        kind: "string",
        value: text.slice(start + 1, i - 1),
      });
    } else if (
      text[i] === "'" &&
      /^'(?:[^'\\]|\\(?:[0-9]{3}|.))'/.test(text.slice(i))
    ) {
      i += text.slice(i).match(/^'(?:[^'\\]|\\(?:[0-9]{3}|.))'/)[0].length;
      out.push({
        start,
        end: i,
        kind: "character",
        value: text.slice(start, i),
      });
    } else if (/[A-Za-z_]/.test(text[i])) {
      while (i < text.length && /[A-Za-z_0-9']/.test(text[i])) i++;
      out.push({ start, end: i, kind: "word", value: text.slice(start, i) });
    } else {
      // Quoted OCaml strings have separate delimiters; do not interpret their body.
      if (text[i] === "{" && /^\{[a-z_]*\|/.test(text.slice(i))) return null;
      i++;
      out.push({ start, end: i, kind: "symbol", value: text[start] });
    }
  }
  return out;
}
const hash = (token) =>
  token?.kind === "string" && /^[a-f0-9]{64}$/.test(token.value);
const string = (token) =>
  token?.kind === "string" && !token.value.includes("\\");
const surface = (token) => string(token) && surfaces.has(token.value);
const topic = (token) =>
  string(token) && /^[a-z][a-z0-9_.-]+$/.test(token.value);

function fingerprintProof(path, text) {
  const ts = tokens(text);
  if (!ts) return null;
  const allowed = new Set();
  const corpus = path.endsWith("/authoring_corpus.ml");
  for (let i = 0; i < ts.length; i++) {
    if (ts[i].value !== "let" || ts[i + 2]?.value !== "=") continue;
    const name = ts[i + 1]?.value;
    if (!corpus && name === "topic_contracts") {
      let j = i + 3;
      if (ts[j++]?.value !== "[") return null;
      while (ts[j]?.value !== "]") {
        if (
          ts[j++]?.value !== "(" ||
          !surface(ts[j++]) ||
          ts[j++]?.value !== "," ||
          !topic(ts[j++]) ||
          ts[j++]?.value !== "," ||
          !hash(ts[j])
        )
          return null;
        allowed.add(j++);
        if (ts[j++]?.value !== ")") return null;
        if (ts[j]?.value === ";") j++;
        else if (ts[j]?.value !== "]") return null;
      }
      i = j;
    }
    if (!corpus || (!mappingNames.has(name) && name !== "entrypoint_mappings"))
      continue;
    let end = i + 3;
    while (
      end < ts.length &&
      !(ts[end].value === ";" && ts[end + 1]?.value === ";")
    )
      end++;
    if (end === ts.length) return null;
    for (let j = i + 3; j < end; j++) {
      if (name === "entrypoint_mappings") {
        if (
          ts[j].value === "make" &&
          string(ts[j + 1]) &&
          /^[a-z_0-9]+\/entrypoint\/[a-z_]+$/.test(ts[j + 1].value) &&
          hash(ts[j + 2]) &&
          topic(ts[j + 3]) &&
          hash(ts[j + 4])
        )
          allowed.add(j + 4);
      } else if (
        ts[j].value === "(" &&
        surface(ts[j + 1]) &&
        ts[j + 2]?.value === ","
      ) {
        let k = j + 3;
        if (topic(ts[k]) && !hash(ts[k]) && ts[k + 1]?.value === ",") k += 2;
        if (hash(ts[k]) && ts[k + 1]?.value === "," && ts[k + 2]?.value === "[")
          allowed.add(k);
      }
    }
    i = end;
  }
  if (corpus) {
    for (let start = 0; start < ts.length; start++) {
      if (
        ts[start].value !== "let" ||
        !["language_foundation", "runtime_foundation"].includes(
          ts[start + 1]?.value,
        ) ||
        ts[start + 2]?.value !== "~"
      )
        continue;
      let end = start + 3;
      while (
        end < ts.length &&
        !(ts[end].value === ";" && ts[end + 1]?.value === ";")
      )
        end++;
      if (end === ts.length) return null;
      for (let i = start; i < end; i++) {
        if (
          ts[i].value === "(" &&
          string(ts[i + 1]) &&
          /^#{1,6} /.test(ts[i + 1].value) &&
          ts[i + 2]?.value === "," &&
          hash(ts[i + 3]) &&
          ts[i + 4]?.value === ")"
        )
          allowed.add(i + 3);
        if (
          ts[i].value === "excerpt_sha256" &&
          ts[i + 1]?.value === "=" &&
          ts[i + 2]?.value === "["
        ) {
          let j = i + 3;
          while (hash(ts[j])) {
            allowed.add(j++);
            if (ts[j]?.value !== ";") break;
            j++;
          }
          if (ts[j]?.value !== "]") return null;
        }
      }
      start = end;
    }
  }

  const segments = [];
  let offset = 0;
  for (const i of [...allowed].sort((a, b) => a - b)) {
    const token = ts[i];
    segments.push(text.slice(offset, token.start));
    offset = token.end;
  }
  segments.push(text.slice(offset));
  return JSON.stringify(segments);
}

/** Conservative docs-only proof. False means full qualification, including any
 * unsupported Markdown/OCaml construct. This grants no filesystem or review authority. */
export function isDocumentationOnlyChange(path, before, after) {
  if (
    !isDocumentationCandidate(path) ||
    typeof before !== "string" ||
    typeof after !== "string" ||
    /[\x00-\x08\x0B-\x1F\x7F\uFFFD]/.test(before + after)
  )
    return false;
  const project = fingerprintOwners.has(path)
    ? fingerprintProof
    : (_, text) => markdownProof(text);
  const left = project(path, before),
    right = project(path, after);
  return left !== null && right !== null && left === right;
}
