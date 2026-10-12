import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import {
  isDocumentationCandidate as candidate,
  isDocumentationOnlyChange as proof,
} from "../scripts/docs-diff.mjs";

const hash = "a".repeat(64),
  nextHash = "b".repeat(64);
const doc = "docs-src/guide/test.md";
const corpus = "lib/authoring_sources/authoring_corpus.ml";
const coverageNames = [
  "declaration",
  "grammar",
  "native",
  "runtime",
  "semantic",
];
const coverage = (name) => `lib/authoring_sources/${name}_coverage_data.ml`;
const root = new URL("../../", import.meta.url);
const read = (path) => fs.readFileSync(new URL(path, root), "utf8");

test("candidate paths are exact and do not grant mode or text authority", () => {
  for (const path of [doc, corpus, ...coverageNames.map(coverage)])
    assert.equal(candidate(path), true);
  for (const path of [
    "docs-src/examples/a.md",
    "docs-src/a.mdx",
    "README.md",
    "lib/authoring_sources/other.ml",
    "docs-src/../a.md",
    "docs-src//a.md",
    "/docs-src/a.md",
  ])
    assert.equal(candidate(path), false, path);
  for (const value of [undefined, null, 1])
    assert.equal(candidate(value), false);
  for (const text of [
    null,
    "bad\0text",
    "bad\uFFFDtext",
    "bad\rtext",
    "bad\u000Btext",
    "bad\u001Btext",
  ])
    assert.equal(proof(doc, "text", text), false);
});

test("plain prose edits preserve fenced, indented and marked authoring examples", () => {
  const before =
    '# Heading\n\nOld `inline` prose.\n\n<!-- ochat-authoring-example: {"id":"example"} -->\n```ocaml\nlet main () = 1\n```\n\n    indented_code ()\n';
  assert.equal(
    proof(
      doc,
      before,
      before.replace(
        "Old `inline` prose.",
        "New `inline` prose,\nwith a [link](guide.md).",
      ),
    ),
    true,
  );
  for (const after of [
    before.replace("main () = 1", "main () = 2"),
    before.replace('"example"', '"other"'),
    before.replace("```ocaml", "```text"),
    before.replace("    indented_code", "    changed_code"),
    before.replace("# Heading", "# New heading"),
    before + "```\nnew code\n```\n",
  ])
    assert.equal(proof(doc, before, after), false);
});

test("frontmatter, multiline comments and every fence body remain exact", () => {
  const before =
    '---\ntitle: Test\n---\n\nOld prose.\n\n<!-- marker\nmetadata -->\n\n~~~python\nprint("a")\n~~~\n';
  assert.equal(
    proof(doc, before, before.replace("Old prose.", "New prose.")),
    true,
  );
  for (const after of [
    before.replace("title: Test", "title: Changed"),
    before.replace("metadata", "changed"),
    before.replace('print("a")', 'print("b")'),
    before.replace("~~~python", "~~~sh"),
  ])
    assert.equal(proof(doc, before, after), false);
  for (const uncertain of [
    "```\nopen",
    "<!-- open",
    "---\nopen",
    "Title\n=====\n",
    ":::custom\nbody\n:::\n",
    "<script>\nbody\n</script>\n",
    "Unclosed `span",
    "+++\ntitle = 1\n+++\n",
    "Before `span\n```\nend`",
  ])
    assert.equal(proof(doc, uncertain, uncertain), false, uncertain);
  assert.equal(proof(doc, "Before\n\n    code\n", "After\n    code\n"), false);
});

test("coverage files admit only the third literal of exact topic_contracts tuples", () => {
  for (const name of coverageNames) {
    const path = coverage(name);
    const before = `let implementation_sources = [ ( "runtime.ml", "${hash}" ) ]\n;;\nlet topic_contracts =\n [ ( "tool_v1", "chatml.modules", "${hash}" ) ]\n;;\n`;
    assert.equal(
      proof(
        path,
        before,
        before.replace(
          `"chatml.modules", "${hash}"`,
          `"chatml.modules", "${nextHash}"`,
        ),
      ),
      true,
    );
    assert.equal(
      proof(
        path,
        before,
        before.replace(
          `"runtime.ml", "${hash}"`,
          `"runtime.ml", "${nextHash}"`,
        ),
      ),
      false,
    );
    assert.equal(
      proof(path, before, before.replace("chatml.modules", "chatml.programs")),
      false,
    );
    assert.equal(
      proof(path, before, before.replace("tool_v1", "moderator_v1")),
      false,
    );
    assert.equal(
      proof(
        path,
        before,
        before.replace(`"${hash}" ) ]`, `"${nextHash}", "extra" ) ]`),
      ),
      false,
    );
  }
});

test("all five actual coverage owners protect implementation pins", () => {
  for (const name of coverageNames) {
    const path = coverage(name),
      before = read(path);
    const start = before.indexOf("let topic_contracts =");
    assert.ok(start > 0);
    const topicHash = before.slice(start).match(/"([a-f0-9]{64})"/)[1];
    const after =
      before.slice(0, start) + before.slice(start).replace(topicHash, nextHash);
    assert.equal(proof(path, before, after), true, path);
    const implementation = before.indexOf("let implementation_sources =");
    if (implementation >= 0) {
      const bodyHash = before
        .slice(implementation, start)
        .match(/"([a-f0-9]{64})"/)[1];
      assert.equal(
        proof(path, before, before.replace(bodyHash, nextHash)),
        false,
        path,
      );
    }
  }
});

test("corpus closure, entrypoint and excerpt positions cannot mask compiler hashes", () => {
  const before = `let entrypoint_mappings = [ make "tool_v1/entrypoint/run" "${hash}" "runtime.invocations.standalone" "${nextHash}" ]\n;;\nlet task_mappings = [ ( "tool_v1", "${hash}", [ ( "module/Task", "${nextHash}" ) ] ) ]\n;;\nlet background_mappings = [ ( "tool_v1", "runtime.jobs.owned", "${hash}", [ "test.ml" ], [ ( "module/Job", "${nextHash}" ) ] ) ]\n;;\nlet language_foundation ~sources = make [ ( "## Heading", "${hash}" ) ]\n;;\nlet runtime_foundation ~sources = Audited { excerpt_sha256 = [ "${hash}"; "${nextHash}" ] }\n;;\n`;
  for (const [old, replacement] of [
    [
      `"runtime.invocations.standalone" "${nextHash}"`,
      `"runtime.invocations.standalone" "${hash}"`,
    ],
    [`"tool_v1", "${hash}", [`, `"tool_v1", "${nextHash}", [`],
    [`"runtime.jobs.owned", "${hash}"`, `"runtime.jobs.owned", "${nextHash}"`],
    [`"## Heading", "${hash}"`, `"## Heading", "${nextHash}"`],
    [`excerpt_sha256 = [ "${hash}"`, `excerpt_sha256 = [ "${nextHash}"`],
  ])
    assert.equal(
      proof(corpus, before, before.replace(old, replacement)),
      true,
      old,
    );
  for (const after of [
    before.replace(
      `"tool_v1/entrypoint/run" "${hash}"`,
      `"tool_v1/entrypoint/run" "${nextHash}"`,
    ),
    before.replace(`"module/Task", "${nextHash}"`, `"module/Task", "${hash}"`),
    before.replace("## Heading", "## Other"),
  ]) {
    assert.notEqual(after, before);
    assert.equal(proof(corpus, before, after), false);
  }
  assert.equal(
    proof(
      corpus,
      before,
      before.replace(
        `"## Heading", "${hash}"`,
        '"## Heading", "DOCUMENTATION_SHA256"',
      ),
    ),
    false,
  );
  const outside =
    before + `let unrelated = [ ( "## Heading", "${hash}" ) ]\n;;\n`;
  assert.equal(
    proof(
      corpus,
      outside,
      outside.replace(/(let unrelated.*)a{64}/, `$1${nextHash}`),
    ),
    false,
  );
});

test("the actual corpus admits an excerpt refresh while keeping code and descriptor pins exact", () => {
  const before = read(corpus);
  const match = before.match(
    /\( "## Modules export their own declarations"\s*, "([a-f0-9]{64})"/,
  );
  assert.ok(match);
  assert.equal(proof(corpus, before, before.replace(match[1], nextHash)), true);
  assert.equal(
    proof(
      corpus,
      before,
      before.replace("let digest =", "let changed_digest ="),
    ),
    false,
  );
  const contract = before.match(
    /"one_off_v1\/entrypoint\/main"\s*"([a-f0-9]{64})"/,
  )[1];
  assert.equal(
    proof(corpus, before, before.replace(contract, nextHash)),
    false,
  );
  const fake = `(* let task_mappings = [ ( "tool_v1", "${hash}", [] ) ] ;; *)\n`;
  assert.equal(proof(corpus, fake, fake.replace(hash, nextHash)), false);
  assert.equal(
    proof(
      corpus,
      `let literal = {raw|${fake}|raw}`,
      `let literal = {raw|${fake.replace(hash, nextHash)}|raw}`,
    ),
    false,
  );
});
