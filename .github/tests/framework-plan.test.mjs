import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync, spawnSync } from "node:child_process";
import {
  repositoryIdentity,
  currentPlan,
  loadCurrentPlan,
  planPath,
} from "../scripts/framework-plan.mjs";

function withRepository(f) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "ochat-framework-plan-"));
  const git = (...args) =>
    execFileSync("git", args, { cwd: root, stdio: "ignore" });
  const write = (name, content) => {
    fs.mkdirSync(path.dirname(path.join(root, name)), { recursive: true });
    fs.writeFileSync(path.join(root, name), content);
  };
  try {
    git("init", "--quiet");
    git("config", "user.name", "Framework test fixture");
    git("config", "user.email", "fixture@example.invalid");
    git("config", "commit.gpgsign", "false");
    git("config", "core.hooksPath", path.join(root, "no-hooks"));
    write("dune", "(dirs :standard \\ website scratch)\n");
    const owners = [
      "chatml_git_host",
      ...Array.from({ length: 7 }, (_, i) => `suite-${i}`),
    ];
    for (const owner of owners)
      write(
        `test/${owner}/dune`,
        `(library (name ${owner.replaceAll("-", "_")}) (inline_tests))\n`,
      );
    write(
      ".github/framework-test-weights.json",
      JSON.stringify({ version: 1, weights: { "test/chatml_git_host": 10 } }),
    );
    git("add", ".");
    git("commit", "--quiet", "-m", "Isolated framework planning fixture");
    const plan = currentPlan(root);
    write(planPath, JSON.stringify(plan));
    return f({ root, git, write, plan });
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
}

test("repository identity and persisted plan bind deterministic tracked source and revision", () =>
  withRepository(({ root, plan, write }) => {
    const identity = repositoryIdentity(root);
    assert.match(identity.revision, /^[a-f0-9]{40}$/);
    assert.match(identity.sourceHash, /^[a-f0-9]{64}$/);
    assert.deepEqual(loadCurrentPlan(root), plan);
    assert.deepEqual(currentPlan(root), plan);
    write("untracked-local-note.txt", "not committed CI input");
    assert.deepEqual(repositoryIdentity(root), identity);
  }));

test("edited source, newly tracked tests, timing changes and another run invalidate retained plan", () => {
  for (const mutation of [
    ({ write }) => write("test/suite-0/dune", "(test (name replacement))\n"),
    ({ write, git }) => {
      write("test/new-owner/dune", "(test (name new_test))\n");
      git("add", "test/new-owner/dune");
    },
    ({ write }) =>
      write(
        ".github/framework-test-weights.json",
        JSON.stringify({ version: 1, weights: { "test/suite-0": 100 } }),
      ),
    ({ write, plan }) =>
      write(planPath, JSON.stringify({ ...plan, runId: "another-run" })),
  ])
    withRepository((fixture) => {
      mutation(fixture);
      assert.throws(() => loadCurrentPlan(fixture.root));
    });
});

test("a valid but reassigned artifact plan cannot override authoritative scheduling", () =>
  withRepository(({ root, write, plan }) => {
    const tampered = structuredClone(plan);
    [tampered.shards[0].aliases, tampered.shards[1].aliases] = [
      tampered.shards[1].aliases,
      tampered.shards[0].aliases,
    ];
    write(planPath, JSON.stringify(tampered));
    assert.throws(() => loadCurrentPlan(root), /differs from this run/);
  }));

test("qualification script consumes nested reports and rejects missing, duplicate or stale artifacts", () => {
  const script = fileURLToPath(
    new URL("../scripts/framework-qualification.mjs", import.meta.url),
  );
  for (const variant of [
    "valid",
    "missing",
    "duplicate",
    "stale",
    "unexpected-file",
  ])
    withRepository(({ root, plan, write }) => {
      const reports = plan.shards.map(({ index, aliases }) => ({
        version: 1,
        revision: plan.revision,
        sourceHash: plan.sourceHash,
        runId: plan.runId,
        index,
        aliases,
        result: "pass",
        durationSeconds: 1,
      }));
      if (variant === "missing") reports.pop();
      if (variant === "duplicate") reports[7] = reports[0];
      if (variant === "stale") reports[0].sourceHash = "c".repeat(64);
      reports.forEach((report, index) =>
        write(
          `.ci-evidence/normal-shards/framework-${index + 1}/normal-shard.json`,
          JSON.stringify(report),
        ),
      );
      if (variant === "unexpected-file")
        write(".ci-evidence/normal-shards/stray.json", "{}");
      const result = spawnSync(process.execPath, [script], {
        cwd: root,
        encoding: "utf8",
        env: { ...process.env, GITHUB_STEP_SUMMARY: "" },
      });
      assert.equal(result.status, variant === "valid" ? 0 : 1, result.stderr);
      if (variant === "valid")
        assert.deepEqual(JSON.parse(result.stdout), {
          shardCount: 8,
          aliasCount: 8,
          durationSeconds: 8,
        });
    });
});

// Shared test-support libraries can consume Git indirectly. Every group must
// receive the same fixture environment as the unsharded suite.
test("every normal group rejects missing native Git before invoking Dune", () =>
  withRepository(({ root, plan }) => {
    const script = fileURLToPath(
      new URL("../scripts/framework-tests.mjs", import.meta.url),
    );
    const env = { ...process.env };
    delete env.OCHAT_TEST_GIT_2_56_DIRECTORY;
    for (const { index } of plan.shards) {
      const result = spawnSync(
        process.execPath,
        [script, "normal", String(index)],
        {
          cwd: root,
          encoding: "utf8",
          env,
        },
      );
      assert.notEqual(result.status, 0);
      assert.match(
        result.stderr,
        /Pinned Git fixture provisioning is required/,
      );
      assert.ok(!fs.existsSync(path.join(root, ".ci-evidence/normal.log")));
    }
  }));
