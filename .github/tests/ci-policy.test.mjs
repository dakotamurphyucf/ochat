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
  documentationOnlyPaths,
  lastSuccessfulDeployment,
  reuseReferenceQualification,
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
    for (const name of ["browser", "website-qualification"])
      needs[name] = { result: needs.website.result };
    assert.equal(requireSelectedJobs(needs).length, 5);
    for (const name of [...names, "browser", "website-qualification"])
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

test("documentation proofs apply only to their own baseline and never mask mixed runtime changes", () => {
  const doc = "docs-src/guide/example.md";
  const input = {
    event: "push",
    ref: "refs/heads/main",
    paths: [doc],
    documentationPaths: [doc],
  };
  const fast = chooseChecks(input);
  assert.deepEqual(
    [fast.framework, fast.semantics, fast.website, fast.deploy],
    [false, true, true, true],
  );
  assert.equal(
    chooseChecks({ ...input, paths: [doc, "lib/runtime.ml"] }).framework,
    true,
  );
  assert.equal(chooseChecks({ ...input, pendingPaths: [doc] }).framework, true);
  assert.equal(
    chooseChecks({
      ...input,
      pendingPaths: [doc],
      pendingDocumentationPaths: [doc],
    }).framework,
    false,
  );
  assert.equal(
    chooseChecks({ ...input, fallback: "Uncertain history" }).framework,
    true,
  );
  assert.equal(classifyPaths([doc]).framework, true);
  for (const file of [
    ".github/scripts/ci-policy.mjs",
    ".github/scripts/select-checks.mjs",
    ".github/scripts/docs-diff.mjs",
    ".github/tests/ci-policy.test.mjs",
    ".github/tests/docs-diff.test.mjs",
  ]) {
    const selected = classifyPaths([file]);
    assert.deepEqual(
      [selected.framework, selected.semantics, selected.website],
      [false, true, true],
    );
  }
  for (const file of [
    ".github/actions/setup-ochat/action.yml",
    ".github/scripts/framework-tests.mjs",
    ".github/ci-toolchain.json",
    ".github/scripts/future.mjs",
  ])
    assert.equal(classifyPaths([file]).framework, true);
});

test("documentation proof uses exact regular Git blobs and fails closed for moves, modes and read errors", () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "ochat-doc-proof-"));
  const git = (args) =>
    execFileSync("git", args, {
      cwd: directory,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "pipe"],
    });
  const doc = "docs-src/guide/with spaces.md";
  const file = path.join(directory, doc);
  const commit = (message) => {
    git(["add", "."]);
    git(["commit", "-qm", message]);
    return git(["rev-parse", "HEAD"]).trim();
  };
  try {
    git(["init", "-q"]);
    git(["config", "user.email", "fixture@example.invalid"]);
    git(["config", "user.name", "CI fixture"]);
    git(["config", "core.fileMode", "true"]);
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(
      file,
      "# Example\n\nOld prose.\n\n```chatml\nlet x = 1\n```\n",
    );
    const base = commit("base");
    fs.writeFileSync(
      file,
      fs.readFileSync(file, "utf8").replace("Old prose.", "New prose."),
    );
    const prose = commit("prose");
    assert.deepEqual(documentationOnlyPaths(base, prose, [doc], git), [doc]);
    const eventFile = path.join(directory, ".git", "event.json");
    fs.writeFileSync(
      eventFile,
      JSON.stringify({ pull_request: { base: { sha: base } } }),
    );
    const selection = spawnSync(
      process.execPath,
      [fileURLToPath(new URL("../scripts/select-checks.mjs", import.meta.url))],
      {
        cwd: directory,
        encoding: "utf8",
        env: {
          ...process.env,
          GITHUB_EVENT_PATH: eventFile,
          GITHUB_EVENT_NAME: "pull_request",
          GITHUB_REF: "refs/pull/1/merge",
          CI_HEAD_SHA: prose,
          GITHUB_OUTPUT: path.join(directory, ".git", "output"),
          GITHUB_STEP_SUMMARY: path.join(directory, ".git", "summary"),
        },
      },
    );
    assert.equal(selection.status, 0, selection.stderr);
    const report = JSON.parse(
      fs.readFileSync(
        path.join(directory, ".ci-evidence/selection.json"),
        "utf8",
      ),
    );
    assert.deepEqual(
      [report.framework, report.semantics, report.website],
      [false, true, true],
    );
    assert.deepEqual(report.documentationPaths, [doc]);
    fs.rmSync(path.join(directory, ".ci-evidence"), { recursive: true });
    assert.deepEqual(
      documentationOnlyPaths(base, prose, [doc], () => {
        throw new Error("read failure");
      }),
      [],
    );
    fs.chmodSync(file, 0o755);
    const executable = commit("mode");
    assert.deepEqual(documentationOnlyPaths(prose, executable, [doc], git), []);
    fs.chmodSync(file, 0o644);
    fs.writeFileSync(
      file,
      fs.readFileSync(file, "utf8").replace("let x = 1", "let x = 2"),
    );
    const code = commit("code");
    assert.deepEqual(documentationOnlyPaths(base, code, [doc], git), []);
    fs.renameSync(file, file.replace("with spaces", "moved"));
    const moved = commit("move");
    assert.deepEqual(
      documentationOnlyPaths(code, moved, changedPaths(code, moved, git), git),
      [],
    );
    fs.symlinkSync("moved.md", file);
    const symlink = commit("symlink");
    assert.deepEqual(documentationOnlyPaths(base, symlink, [doc], git), []);
    assert.throws(() =>
      documentationOnlyPaths("0".repeat(40), prose, [doc], git),
    );
  } finally {
    fs.rmSync(directory, { recursive: true, force: true });
  }
});

function referenceReuseFixture() {
  const repository = "dakotamurphyucf/ochat";
  const source = "cd1e516922171cd159fb36e9de4c0eff5921992a";
  const base = "5988e9bffadf5a59e39777ac46a20458b8a206a3";
  const head = "e".repeat(40);
  const branch = "docs/chatml-reference-audit-20261012";
  const runId = 38332579027;
  const names = [
    "framework (normal, x64, ubuntu-24.04)",
    "framework (e2e, x64, ubuntu-24.04)",
    "framework (git-native, arm64, ubuntu-24.04-arm)",
    "release-gate",
  ];
  const input = {
    repository,
    kind: "pull_request",
    ref: "refs/pull/112/merge",
    head,
    mode: "validate",
    event: {
      repository: { full_name: repository },
      pull_request: {
        number: 112,
        base: { ref: "main", repo: { full_name: repository } },
        head: { ref: branch, sha: head, repo: { full_name: repository } },
      },
    },
    selection: chooseChecks({
      event: "pull_request",
      paths: ["docs-src/guide/chatml-language-spec.md"],
    }),
  };
  const repo = { url: `https://api.github.com/repos/${repository}` };
  const run = {
    id: runId,
    run_attempt: 1,
    head_sha: source,
    head_branch: branch,
    status: "completed",
    conclusion: "success",
    event: "pull_request",
    path: ".github/workflows/website.yml",
    repository: { full_name: repository },
    head_repository: { full_name: repository },
    pull_requests: [
      {
        number: 112,
        base: { ref: "main", repo },
        head: { ref: branch, sha: head, repo },
      },
    ],
  };
  const jobs = {
    total_count: names.length,
    jobs: names.map((name) => ({
      name,
      run_id: runId,
      run_attempt: 1,
      head_sha: source,
      status: "completed",
      conclusion: "success",
    })),
  };
  const paths = [
    ".github/scripts/select-checks.mjs",
    ".github/tests/ci-policy.test.mjs",
  ];
  const calls = [];
  const request = async (route) => {
    calls.push(route);
    if (route === `/repos/${repository}/actions/runs/${runId}`) return run;
    assert.equal(
      route,
      `/repos/${repository}/actions/runs/${runId}/attempts/1/jobs?per_page=100`,
    );
    return jobs;
  };
  const git = (args) => {
    if (args[0] === "diff") {
      assert.deepEqual(args, [
        "diff",
        "--name-only",
        "--no-renames",
        "-z",
        source,
        head,
        "--",
      ]);
      return paths.map((name) => `${name}\0`).join("");
    }
    assert.equal(args[0], "ls-tree");
    assert.ok([source, head].includes(args[2]));
    return `100644 blob ${"a".repeat(40)}\t${args[4]}\0`;
  };
  return { input, run, jobs, paths, calls, request, git, base };
}

test("PR112 reuses only the pinned successful attempt over the same complete tree", async () => {
  for (const main of [false, true]) {
    const f = referenceReuseFixture();
    if (main) {
      // GitHub drops a run's PR association after merge. Its run/job SHAs remain.
      f.run.pull_requests = [];
      f.input.kind = "push";
      f.input.ref = "refs/heads/main";
      f.input.event = {
        repository: f.input.event.repository,
        before: f.base,
        after: f.input.head,
      };
      f.input.selection = chooseChecks({
        event: "push",
        ref: f.input.ref,
        paths: ["docs-src/guide/chatml-language-spec.md"],
        pendingPaths: ["lib/previously-unshipped.ml"],
      });
    }
    const original = structuredClone(f.input.selection);
    const result = await reuseReferenceQualification(f.input, f.request, f.git);
    assert.equal(result.framework, false);
    for (const key of ["semantics", "website", "deploy", "cold"])
      assert.equal(result[key], original[key]);
    assert.deepEqual(
      f.input.selection,
      original,
      "original selection is immutable",
    );
    assert.equal(result.qualificationReuse.reused, true);
    assert.equal(result.qualificationReuse.sourceRun, 38332579027);
    assert.equal(result.qualificationReuse.sourceAttempt, 1);
    assert.deepEqual(result.qualificationReuse.paths, f.paths);
    assert.equal(result.qualificationReuse.jobs.length, 4);
    assert.match(result.reasons.at(-1), /PR112 run 38332579027 attempt 1/);
    assert.equal(f.calls.length, 2);
  }
});

test("one-off reuse never applies to other events, repositories, cold or uncertain selection", async () => {
  const cases = [
    (f) => {
      f.input.repository = "other/ochat";
    },
    (f) => {
      f.input.event.repository.full_name = "other/ochat";
    },
    (f) => {
      f.input.event.pull_request.number = 113;
    },
    (f) => {
      f.input.event.pull_request.head.ref = "another-branch";
    },
    (f) => {
      f.input.event.pull_request.head.repo.full_name = "fork/ochat";
    },
    (f) => {
      f.input.event.pull_request.head.sha = "f".repeat(40);
    },
    (f) => {
      f.input.event.pull_request.base.ref = "release";
    },
    (f) => {
      f.input.event.pull_request.base.repo.full_name = "fork/ochat";
    },
    (f) => {
      f.input.ref = "refs/pull/113/merge";
    },
    (f) => {
      f.input.kind = "workflow_dispatch";
    },
    (f) => {
      f.input.kind = "schedule";
    },
    (f) => {
      f.input.selection.cold = true;
    },
    (f) => {
      f.input.mode = "cold";
    },
    (f) => {
      f.input.fallback = "failed baseline proof";
    },
    (f) => {
      f.input.kind = "push";
      f.input.ref = "refs/heads/main";
      f.input.event.before = "f".repeat(40);
      f.input.event.after = f.input.head;
    },
    (f) => {
      f.input.kind = "push";
      f.input.ref = "refs/heads/main";
      f.input.event.before = f.base;
      f.input.event.after = "f".repeat(40);
    },
    (f) => {
      f.input.kind = "push";
      f.input.ref = "refs/heads/other";
      f.input.event.before = f.base;
      f.input.event.after = f.input.head;
    },
  ];
  for (const change of cases) {
    const f = referenceReuseFixture();
    change(f);
    const result = await reuseReferenceQualification(f.input, f.request, f.git);
    assert.strictEqual(result, f.input.selection);
    assert.equal(result.framework, true);
    assert.equal(f.calls.length, 0);
  }
});

test("qualification run and exact job inventory mismatches keep full existing checks", async () => {
  const cases = [
    (f) => {
      f.run.id++;
    },
    (f) => {
      f.run.head_sha = "f".repeat(40);
    },
    (f) => {
      f.run.repository.full_name = "other/ochat";
    },
    (f) => {
      f.run.head_repository.full_name = "fork/ochat";
    },
    (f) => {
      f.run.event = "push";
    },
    (f) => {
      f.run.path = ".github/workflows/other.yml";
    },
    (f) => {
      f.run.run_attempt = 2;
    },
    (f) => {
      f.run.status = "in_progress";
    },
    (f) => {
      f.run.conclusion = "failure";
    },
    (f) => {
      f.run.head_branch = "other";
    },
    (f) => {
      f.jobs.jobs[0].conclusion = "skipped";
    },
    (f) => {
      f.jobs.jobs[0].status = "in_progress";
    },
    (f) => {
      f.jobs.jobs[0].run_attempt = 2;
    },
    (f) => {
      f.jobs.jobs[0].run_id++;
    },
    (f) => {
      f.jobs.jobs[0].head_sha = "f".repeat(40);
    },
    (f) => {
      f.jobs.jobs.pop();
      f.jobs.total_count--;
    },
    (f) => {
      f.jobs.jobs.push({ ...f.jobs.jobs[0] });
      f.jobs.total_count++;
    },
    (f) => {
      f.jobs.total_count++;
    },
    (f) => {
      f.jobs.total_count = 101;
    },
  ];
  for (const change of cases) {
    const f = referenceReuseFixture();
    change(f);
    const result = await reuseReferenceQualification(f.input, f.request, f.git);
    assert.deepEqual(
      { ...result, qualificationReuse: undefined },
      { ...f.input.selection, qualificationReuse: undefined },
    );
    assert.equal(result.qualificationReuse.reused, false);
    assert.ok(result.qualificationReuse.reason);
  }
});

test("full tree mismatch, missing regular blobs and request or Git errors cannot grant reuse", async () => {
  for (const path of [
    "docs-src/guide/chatml-language-spec.md",
    "lib/runtime.ml",
    ".github/workflows/website.yml",
    "ochat.opam",
    "unknown.txt",
  ]) {
    const f = referenceReuseFixture();
    f.paths.push(path);
    const result = await reuseReferenceQualification(f.input, f.request, f.git);
    assert.equal(result.framework, true);
    assert.equal(result.qualificationReuse.reused, false);
    assert.equal(f.calls.length, 0);
  }
  for (const git of [
    () => {
      throw new Error("missing source tree");
    },
    (args) =>
      args[0] === "diff"
        ? ".github/scripts/select-checks.mjs\0"
        : "120000 blob " +
          "a".repeat(40) +
          "\t.github/scripts/select-checks.mjs\0",
  ]) {
    const f = referenceReuseFixture();
    const result = await reuseReferenceQualification(f.input, f.request, git);
    assert.equal(result.framework, true);
    assert.equal(result.qualificationReuse.reused, false);
  }
  for (const failedCall of [1, 2]) {
    const f = referenceReuseFixture();
    let calls = 0;
    const result = await reuseReferenceQualification(
      f.input,
      async (route) => {
        if (++calls === failedCall) throw new Error("public API unavailable");
        return f.request(route);
      },
      f.git,
    );
    assert.equal(result.framework, true);
    assert.equal(result.qualificationReuse.reason, "public API unavailable");
  }
});
