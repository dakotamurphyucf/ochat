import test from "node:test";
import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import {
  discoverInventory,
  inventoryFromFiles,
  parseDune,
} from "../scripts/framework-inventory.mjs";
import { createPlan } from "../scripts/framework-shards.mjs";

const root = { path: "dune", text: "(dirs :standard \\ website scratch)" };
function files(text, owner = "test/example") {
  return [root, { path: `${owner}/dune`, text }];
}

test("inventory includes normal alias owners in every layer, including an explicit root alias", () => {
  const sources = [
    { ...root, text: root.text + " (alias (name runtest))" },
    ...[
      "bin",
      "lib",
      "lib/a",
      "test",
      "test/a",
      "test/fixture",
      "bench",
      "website/x",
      "scratch/x",
    ].map((owner) => ({
      path: `${owner}/dune`,
      text: "(library (name support) (inline_tests))",
    })),
  ];
  const inventory = inventoryFromFiles(sources);
  assert.deepEqual(
    inventory.map(({ id }) => id),
    [".", "bench", "bin", "lib", "lib/a", "test", "test/a", "test/fixture"],
  );
  assert.equal(inventory[0].alias, "@@./runtest");
  assert.equal(
    createPlan({
      revision: "a".repeat(40),
      sourceHash: "b".repeat(64),
      runId: "1",
      inventory,
    }).shards.length,
    8,
  );
  assert.deepEqual(inventoryFromFiles([...sources].reverse()), inventory);
});

test("support libraries, executable fixtures and opt-in qualification aliases do not invent runtest owners", () => {
  assert.deepEqual(
    inventoryFromFiles(
      files(
        "(library (name support)) (executable (name fixture)) (alias (name qualification))",
      ),
    ),
    [],
  );
  for (const text of [
    "(library (name suite) (inline_tests))",
    "(test (name suite))",
    "(tests (names a b))",
    "(cram)",
    "(rule (alias runtest) (action (echo ok)))",
    "(alias (name runtest))",
  ]) {
    assert.deepEqual(inventoryFromFiles(files(text)), [
      { id: "test/example", alias: "@@test/example/runtest" },
    ]);
  }
});

test("plural rule aliases own normal tests and participate in reachable dependency closure", () => {
  assert.deepEqual(
    inventoryFromFiles(
      files("(rule (aliases other runtest) (action (echo ok)))"),
    ),
    [{ id: "test/example", alias: "@@test/example/runtest" }],
  );
  inventoryFromFiles(
    files(
      "(alias (name runtest) (deps (alias helper))) (rule (aliases helper alternative) (action (echo ok))) (rule (aliases opt-in other) (deps (alias elsewhere/runtest)) (action (echo ok)))",
    ),
  );
  for (const dependency of [
    "(alias elsewhere/runtest)",
    "(alias_rec elsewhere)",
    "(include aliases.sexp)",
  ]) {
    assert.throws(() =>
      inventoryFromFiles(
        files(
          `(alias (name runtest) (deps (alias helper))) (rule (aliases helper alternative) (deps ${dependency}) (action (echo ok)))`,
        ),
      ),
    );
    assert.throws(() =>
      inventoryFromFiles(
        files(
          `(rule (aliases helper runtest) (deps ${dependency}) (action (echo ok)))`,
        ),
      ),
    );
  }
  assert.throws(
    () =>
      inventoryFromFiles(
        files("(rule (alias runtest) (aliases other) (action (echo ok)))"),
      ),
    /mutually exclusive/,
  );
});

test("parser preserves syntax boundaries across comments and quoted parentheses", () => {
  assert.deepEqual(
    parseDune(
      '; ignored (\n(rule (alias runtest) (action (echo "text ) ; \\\" quoted")))',
    ),
    [["rule", ["alias", "runtest"], ["action", ["echo", 'text ) ; " quoted']]]],
  );
  for (const text of ["(", ")", '"unterminated', '(echo "truncated\\'])
    assert.throws(() => parseDune(text));
});

test("S-expression comments and encoded strings cannot hide reachable cross-directory aliases", () => {
  assert.deepEqual(
    parseDune("(alias #;(name ignored) (name runtest)) #;(include hidden.inc)"),
    [["alias", ["name", "runtest"]]],
  );
  assert.deepEqual(parseDune('(echo "a\\n\\t\\x2f\\047b")'), [
    ["echo", "a\n\t//b"],
  ]);
  inventoryFromFiles(
    files(
      "#;(alias (name runtest) (deps (alias other/runtest))) (alias (name runtest))",
    ),
  );
  for (const encoded of ["other\\047runtest", "other\\x2fruntest"])
    assert.throws(() =>
      inventoryFromFiles(
        files(`(alias (name runtest) (deps (alias "${encoded}")))`),
      ),
    );
  for (const text of ["#;", '(echo "\\q")', '(echo "\\999")', '(echo "\\xZZ")'])
    assert.throws(() => parseDune(text));
});

test("normal closure permits local dependencies and unreachable opt-in cross-directory aliases", () => {
  inventoryFromFiles(
    files(`
    (library (name foo) (inline_tests (deps (alias helper))))
    (alias (name helper) (deps (alias local)))
    (rule (alias local) (action (echo "ok")))
    (alias (name opt-in) (deps (alias elsewhere/runtest)))
    (rule (alias e2e) (deps (alias_rec elsewhere)))
    (alias (name opt-in-includes) (deps (include aliases.sexp)))
    (include_subdirs qualified)
  `),
  );
  // Compatibility forwarding becomes reachable through the implicit inline-test alias.
  assert.throws(() =>
    inventoryFromFiles(
      files(
        "(library (name foo) (inline_tests)) (alias (name runtest-foo) (deps (alias other/runtest)))",
      ),
    ),
  );
});

test("normal closure fails closed for recursive, dynamic, cyclic or cross-directory dependencies", () => {
  for (const dependency of [
    "(alias_rec local)",
    "(alias ../other/runtest)",
    "(alias other/runtest)",
    "(alias %{env:ALIAS})",
    "(alias local extra)",
  ]) {
    assert.throws(() =>
      inventoryFromFiles(files(`(alias (name runtest) (deps ${dependency}))`)),
    );
    assert.throws(() =>
      inventoryFromFiles(files(`(test (name foo) (deps ${dependency}))`)),
    );
  }
  assert.throws(() =>
    inventoryFromFiles(
      files(
        "(alias (name runtest) (deps (alias a))) (alias (name a) (deps (alias runtest)))",
      ),
    ),
  );
  assert.throws(() =>
    inventoryFromFiles(
      files(
        "(rule (alias runtest) (action (progn (echo ok) (alias other/runtest))))",
      ),
    ),
  );
  assert.throws(() =>
    inventoryFromFiles(files("(cram (deps (alias elsewhere/runtest)))")),
  );
});

test("reachable included dependency lists fail closed, including local forwarding and inline tests", () => {
  for (const include of ["include", "dynamic_include"]) {
    for (const text of [
      `(alias (name runtest) (deps (${include} deps.sexp)))`,
      `(alias (name runtest) (deps (alias helper))) (alias (name helper) (deps (${include} deps.sexp)))`,
      `(library (name suite) (inline_tests (deps (${include} deps.sexp))))`,
    ])
      assert.throws(
        () => inventoryFromFiles(files(text)),
        /included normal dependency/,
      );
  }
});

test("inventory rejects traversal changes and malformed or duplicate sources", () => {
  for (const stanza of [
    "(include shared.inc)",
    "(dynamic_include generated)",
    "(subdir nested)",
    "(dirs :standard)",
    "(data_only_dirs fixture)",
    "(ignored_subdirs fixture)",
  ])
    assert.throws(() => inventoryFromFiles(files(stanza)));
  assert.throws(() =>
    inventoryFromFiles([
      { ...root, text: "(dirs :standard \\ website scratch test)" },
    ]),
  );
  assert.throws(() => inventoryFromFiles([{ ...root, text: "" }]));
  assert.throws(() => inventoryFromFiles([root, root]));
  assert.throws(() =>
    inventoryFromFiles([root, { path: "../dune", text: "" }]),
  );
  assert.throws(() =>
    inventoryFromFiles(files("(new_test_generator (name unknown))")),
  );
});

test("tracked implicit Cram directories and symlinked Dune sources fail closed", () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "ochat-inventory-"));
  const git = (...args) =>
    execFileSync("git", args, { cwd: directory, stdio: "ignore" });
  try {
    git("init", "--quiet");
    fs.writeFileSync(path.join(directory, "dune"), root.text);
    fs.mkdirSync(path.join(directory, "implicit"));
    fs.writeFileSync(
      path.join(directory, "implicit", "example.t"),
      "  $ true\n",
    );
    git("add", ".");
    assert.throws(() => discoverInventory(directory), /Cram directory/);
    fs.writeFileSync(path.join(directory, "implicit", "dune"), "(cram)");
    git("add", ".");
    assert.equal(discoverInventory(directory).length, 1);
    fs.rmSync(path.join(directory, "implicit", "dune"));
    fs.symlinkSync("../dune", path.join(directory, "implicit", "dune"));
    assert.throws(() => discoverInventory(directory), /Nonregular Dune/);
  } finally {
    fs.rmSync(directory, { recursive: true, force: true });
  }
});

test("committed inventory covers binary/library owners and omits support-only directories", () => {
  const inventory = discoverInventory(
    fileURLToPath(new URL("../../", import.meta.url)),
  );
  for (const owner of [
    "bin",
    "lib",
    "lib/context_compaction",
    "lib/chat_tui",
    "lib/chatml_project_prepare",
    "test",
  ])
    assert.ok(
      inventory.some(({ id }) => id === owner),
      `Missing ${owner}`,
    );
  for (const owner of [
    ".",
    "lib/agent_protocol",
    "test/chatml_extensibility_fixtures",
  ])
    assert.ok(
      !inventory.some(({ id }) => id === owner),
      `Invented ${owner} alias`,
    );
  assert.ok(inventory.length > 100);
});
