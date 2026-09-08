import { fileURLToPath } from "node:url";
import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFileSync, spawnSync } from "node:child_process";
import {
  classifyPaths,
  chooseChecks,
  requireSelectedJobs,
} from "../scripts/ci-policy.mjs";
import {
  changedPaths,
  lastSuccessfulDeployment,
} from "../scripts/select-checks.mjs";

test("only explicit maintainer documents avoid qualification; runtime, examples and unknown files run full checks", () => {
  for (const file of [
    "website/planning/maintenance.md",
    "website/README.md",
    "website/CONTRIBUTING.md",
  ]) {
    const result = classifyPaths([file]);
    assert.equal(result.framework || result.semantics || result.website, false);
  }
  for (const file of [
    "lib/chatmd/parser.mly",
    "bin/ochat.ml",
    "test/agent_test.ml",
    "docs-src/examples/x.chatmd",
    "ochat.opam",
    "ochat.opam.locked",
    "dune-project",
    "Readme.md",
    "assets/example.png",
    "tikitoken/data.txt",
    "future/unknown.txt",
    ".github/workflows/website.yml",
  ]) {
    const { framework, semantics, website } = classifyPaths([file]);
    assert.deepEqual([framework, semantics, website], [true, true, true], file);
  }
  assert.deepEqual(
    Object.values(classifyPaths(["website/src/page.astro"])).slice(0, 3),
    [false, true, true],
  );
  assert.equal(classifyPaths(["website/planning/new-code.mjs"]).website, true);
  assert.equal(classifyPaths(["../escape"]).framework, true);
});

test("an unrelated main push catches unshipped website changes and lookup failures fail conservatively", () => {
  const input = {
    event: "push",
    ref: "refs/heads/main",
    paths: ["website/planning/maintenance.md"],
  };
  assert.equal(chooseChecks(input).deploy, false);
  const pending = chooseChecks({
    ...input,
    pendingPaths: ["website/src/page.astro"],
  });
  assert.equal(pending.deploy && pending.website && pending.semantics, true);
  const unknown = chooseChecks({
    ...input,
    fallback: "Unable to read history",
  });
  assert.equal(
    unknown.framework && unknown.website && unknown.semantics && unknown.deploy,
    true,
  );
  assert.equal(
    chooseChecks({
      ...input,
      event: "pull_request",
      pendingPaths: ["lib/runtime.ml"],
    }).deploy,
    false,
  );
});

test("scheduled and manual cold validation run everything without publishing; deliberate recovery requires main", () => {
  for (const event of ["schedule", "workflow_dispatch"]) {
    const result = chooseChecks({
      event,
      ref: "refs/heads/main",
      mode: "cold",
    });
    assert.equal(
      result.framework && result.website && result.semantics && result.cold,
      true,
    );
    assert.equal(result.deploy, false);
  }
  assert.equal(
    chooseChecks({
      event: "workflow_dispatch",
      ref: "refs/heads/main",
      mode: "redeploy",
    }).deploy,
    true,
  );
  assert.equal(
    chooseChecks({
      event: "workflow_dispatch",
      ref: "refs/heads/feature",
      mode: "redeploy",
    }).deploy,
    false,
  );
  assert.throws(() =>
    chooseChecks({ event: "workflow_dispatch", mode: "invalid" }),
  );
});

test("gate accepts only successful selected jobs and explicitly unnecessary skips in all combinations", () => {
  const names = ["semantics", "framework", "website"];
  for (let bits = 0; bits < 8; bits++) {
    const needs = { changes: { result: "success", outputs: {} } };
    for (let index = 0; index < names.length; index++) {
      const selected = Boolean(bits & (1 << index));
      needs.changes.outputs[names[index]] = String(selected);
      needs[names[index]] = { result: selected ? "success" : "skipped" };
    }
    assert.equal(requireSelectedJobs(needs).length, 3);
    for (const name of names)
      for (const status of [
        "failure",
        "cancelled",
        undefined,
        "skipped",
        "success",
      ]) {
        const changed = structuredClone(needs);
        changed[name].result = status;
        if (status !== needs[name].result)
          assert.throws(() => requireSelectedJobs(changed));
      }
    for (const status of ["failure", "cancelled", "skipped", undefined]) {
      const changed = structuredClone(needs);
      changed.changes.result = status;
      assert.throws(() => requireSelectedJobs(changed));
    }
    for (const output of [undefined, "True", "", true]) {
      const changed = structuredClone(needs);
      changed.changes.outputs.framework = output;
      assert.throws(() => requireSelectedJobs(changed));
    }
  }
});

test("deployment baseline ignores failed newer attempts and retains an inactive earlier success", async () => {
  const first = "a".repeat(40),
    second = "b".repeat(40);
  const result = await lastSuccessfulDeployment("owner/repo", async (route) => {
    if (route.includes("/2/statuses")) return [{ state: "failure" }];
    if (route.includes("/1/statuses"))
      return [{ state: "inactive" }, { state: "success" }];
    return [
      { id: 2, sha: second },
      { id: 1, sha: first },
    ];
  });
  assert.deepEqual(result, { revision: first, id: 1 });
  await assert.rejects(lastSuccessfulDeployment("owner/repo", async () => []));
  await assert.rejects(
    lastSuccessfulDeployment("owner/repo", async () => {
      throw new Error("403");
    }),
    /403/,
  );
});

test("real git diffs preserve rename source, deleted paths, filenames with spaces, and full PR merge-base changes", () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "ochat-ci-diff-"));
  const git = (args) =>
    execFileSync("git", args, {
      cwd: directory,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "pipe"],
    });
  const commit = (message) => {
    git(["add", "."]);
    git(["commit", "-qm", message]);
    return git(["rev-parse", "HEAD"]).trim();
  };
  try {
    git(["init", "-q"]);
    git(["config", "user.email", "fixture@example.invalid"]);
    git(["config", "user.name", "CI fixture"]);
    fs.mkdirSync(path.join(directory, "website/planning"), { recursive: true });
    fs.writeFileSync(
      path.join(directory, "website/planning/old name.md"),
      "original",
    );
    fs.writeFileSync(path.join(directory, "runtime.ml"), "runtime");
    const base = commit("base");
    fs.renameSync(
      path.join(directory, "website/planning/old name.md"),
      path.join(directory, "published.md"),
    );
    fs.unlinkSync(path.join(directory, "runtime.ml"));
    const head = commit("move and delete");
    assert.deepEqual(changedPaths(base, head, git).sort(), [
      "published.md",
      "runtime.ml",
      "website/planning/old name.md",
    ]);
    assert.equal(classifyPaths(changedPaths(base, head, git)).framework, true);
    git(["checkout", "-qb", "other", base]);
    fs.writeFileSync(path.join(directory, "unrelated"), "base branch advanced");
    const other = commit("other");
    const mergeBase = git(["merge-base", other, head]).trim();
    assert.equal(mergeBase, base);
    assert.equal(
      changedPaths(mergeBase, head, git).includes("unrelated"),
      false,
    );
    assert.throws(() => changedPaths("0".repeat(40), head, git), /Unavailable/);
    assert.throws(() => changedPaths("c".repeat(40), head, git));
  } finally {
    fs.rmSync(directory, { recursive: true, force: true });
  }
});

test("actual gate entry point returns nonzero when framework fails", () => {
  const needs = {
    changes: {
      result: "success",
      outputs: { framework: "true", semantics: "true", website: "true" },
    },
    framework: { result: "failure" },
    semantics: { result: "success" },
    website: { result: "success" },
  };
  const result = spawnSync(
    process.execPath,
    [fileURLToPath(new URL("../scripts/release-gate.mjs", import.meta.url))],
    {
      env: {
        ...process.env,
        NEEDS_JSON: JSON.stringify(needs),
        GITHUB_STEP_SUMMARY: "",
      },
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    result.stderr.toString(),
    /framework: expected success, got failure/,
  );
});
