import fs from "node:fs";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { chooseChecks } from "./ci-policy.mjs";
import {
  isDocumentationCandidate,
  isDocumentationOnlyChange,
} from "./docs-diff.mjs";

const readGit = (args) =>
  execFileSync("git", args, {
    encoding: "utf8",
    maxBuffer: 4 * 1024 * 1024,
    stdio: ["ignore", "pipe", "pipe"],
  });

// Proof is specific to this pair of revisions. Never reuse the PR/push proof
// for a different deployment baseline. Missing or unusual blobs remain full CI.
export function documentationOnlyPaths(base, head, paths, git = readGit) {
  for (const sha of [base, head])
    if (!/^[a-f0-9]{40}$/.test(sha) || /^0+$/.test(sha))
      throw new Error("Unavailable documentation diff revision");
  const read = (revision, path) => {
    const entry = git(["ls-tree", "-z", revision, "--", path]);
    const match = /^100644 blob ([a-f0-9]{40})\t([^\0]+)\0$/.exec(entry);
    if (!match || match[2] !== path) return undefined;
    const content = git(["cat-file", "blob", match[1]]);
    if (content.includes("\0") || content.includes("\ufffd")) return undefined;
    return content;
  };
  return paths.filter((path) => {
    if (!isDocumentationCandidate(path)) return false;
    try {
      const before = read(base, path),
        after = read(head, path);
      return (
        before !== undefined &&
        after !== undefined &&
        isDocumentationOnlyChange(path, before, after)
      );
    } catch {
      // A failed/oversized read cannot grant permission to skip tests.
      return false;
    }
  });
}

export function changedPaths(
  base,
  head,
  git = (args) =>
    execFileSync("git", args, {
      encoding: "utf8",
      maxBuffer: 32 * 1024 * 1024,
    }),
) {
  for (const sha of [base, head])
    if (!/^[a-f0-9]{40}$/.test(sha) || /^0+$/.test(sha))
      throw new Error("Unavailable diff revision");
  // Disabling rename detection gives both the old and new path, including moves
  // between ignored maintainer docs and published/runtime inputs. Preserve NULs.
  return git(["diff", "--name-only", "--no-renames", "-z", base, head, "--"])
    .split("\0")
    .filter(Boolean);
}

export async function lastSuccessfulDeployment(repository, request) {
  for (let page = 1; page <= 10; page++) {
    const deployments = await request(
      `/repos/${repository}/deployments?environment=production&per_page=100&page=${page}`,
    );
    if (!Array.isArray(deployments))
      throw new Error("Invalid deployment history");
    for (const deployment of deployments) {
      const statuses = await request(
        `/repos/${repository}/deployments/${deployment.id}/statuses?per_page=100`,
      );
      if (!Array.isArray(statuses))
        throw new Error("Invalid deployment status history");
      // A previous success may now be inactive. Failed newer attempts are not a
      // shipping baseline; only a deployment with a success observation counts.
      if (statuses.some((status) => status.state === "success")) {
        if (!/^[a-f0-9]{40}$/.test(deployment.sha))
          throw new Error("Invalid deployed revision");
        return { revision: deployment.sha, id: deployment.id };
      }
    }
    if (deployments.length < 100) break;
  }
  throw new Error(
    "No successful production deployment found within history limit",
  );
}

// One-off reuse for the already qualified PR112 tree, including its squash
// onto the exact current main baseline. This is not an ancestry exemption.
const referenceQualification = Object.freeze({
  repository: "dakotamurphyucf/ochat",
  run: 38332579027,
  attempt: 1,
  head: "cd1e516922171cd159fb36e9de4c0eff5921992a",
  base: "5988e9bffadf5a59e39777ac46a20458b8a206a3",
  branch: "docs/chatml-reference-audit-20261012",
});
const reusedJobs = [
  "framework (normal, x64, ubuntu-24.04)",
  "framework (e2e, x64, ubuntu-24.04)",
  "framework (git-native, arm64, ubuntu-24.04-arm)",
  "release-gate",
];
const reusePolicyFiles = new Set([
  ".github/scripts/select-checks.mjs",
  ".github/tests/ci-policy.test.mjs",
]);

const requestPublicQualification = async (route) => {
  const response = await fetch(`https://api.github.com${route}`, {
    headers: {
      Accept: "application/vnd.github+json",
      "X-GitHub-Api-Version": "2022-11-28",
    },
    redirect: "error",
    signal: AbortSignal.timeout(10000),
  });
  if (!response.ok)
    throw new Error(`Qualification lookup returned HTTP ${response.status}`);
  return response.json();
};

// Borrowed request/git adapters admit only this fixed proof. Any uncertainty
// retains the original selection; semantic, website and deploy decisions stay
// independent, including their separate last-deployment baseline proof.
export async function reuseReferenceQualification(
  { repository, kind, ref, event, head, selection, fallback, mode },
  request = requestPublicQualification,
  git = readGit,
) {
  const proof = referenceQualification;
  const pr = event.pull_request;
  const currentPullRequest =
    kind === "pull_request" &&
    ref === "refs/pull/112/merge" &&
    pr?.number === 112 &&
    pr.base?.ref === "main" &&
    pr.base?.repo?.full_name === proof.repository &&
    pr.head?.ref === proof.branch &&
    pr.head?.repo?.full_name === proof.repository &&
    pr.head?.sha === head;
  const currentMainPush =
    kind === "push" &&
    ref === "refs/heads/main" &&
    event.before === proof.base &&
    event.after === head;
  if (
    repository !== proof.repository ||
    event.repository?.full_name !== proof.repository ||
    !(currentPullRequest || currentMainPush) ||
    fallback !== undefined ||
    selection.cold ||
    mode !== "validate" ||
    !selection.framework
  )
    return selection;
  const evidence = {
    sourceRun: proof.run,
    sourceAttempt: proof.attempt,
    sourceRevision: proof.head,
    sourceUrl: `https://github.com/${proof.repository}/actions/runs/${proof.run}/attempts/${proof.attempt}`,
    reused: false,
  };
  try {
    const paths = changedPaths(proof.head, head, git);
    if (paths.some((path) => !reusePolicyFiles.has(path)))
      throw new Error(
        "Qualified tree differs outside the two tested policy files",
      );
    for (const path of paths)
      for (const revision of [proof.head, head]) {
        const entry = git(["ls-tree", "-z", revision, "--", path]);
        const match = /^100644 blob ([a-f0-9]{40})\t([^\0]+)\0$/.exec(entry);
        if (!match || match[2] !== path)
          throw new Error(
            "Qualification policy files must remain regular blobs",
          );
      }
    const route = `/repos/${proof.repository}/actions/runs/${proof.run}`;
    const run = await request(route);
    // pull_requests is a mutable association: its SHA changes when the PR
    // advances and the array can disappear after merge. The fixed run identity,
    // immutable head_sha and pinned attempt's job.head_sha certify source code.
    if (
      run.id !== proof.run ||
      run.run_attempt !== proof.attempt ||
      run.head_sha !== proof.head ||
      run.status !== "completed" ||
      run.conclusion !== "success" ||
      run.event !== "pull_request" ||
      run.path !== ".github/workflows/website.yml" ||
      run.repository?.full_name !== proof.repository ||
      run.head_repository?.full_name !== proof.repository ||
      run.head_branch !== proof.branch
    )
      throw new Error(
        "Source qualification run metadata does not match the pinned proof",
      );
    const result = await request(
      `${route}/attempts/${proof.attempt}/jobs?per_page=100`,
    );
    if (
      !Array.isArray(result.jobs) ||
      !Number.isInteger(result.total_count) ||
      result.total_count !== result.jobs.length ||
      result.total_count > 100
    )
      throw new Error("Source qualification job inventory is incomplete");
    for (const name of reusedJobs) {
      const matching = result.jobs.filter((job) => job.name === name);
      if (
        matching.length !== 1 ||
        matching[0].run_id !== proof.run ||
        matching[0].run_attempt !== proof.attempt ||
        matching[0].head_sha !== proof.head ||
        matching[0].status !== "completed" ||
        matching[0].conclusion !== "success"
      )
        throw new Error(`Source qualification job is not successful: ${name}`);
    }
    return {
      ...selection,
      framework: false,
      qualificationReuse: {
        ...evidence,
        reused: true,
        paths,
        jobs: [...reusedJobs],
      },
      reasons: [
        ...selection.reasons,
        `Framework qualification reused from PR112 run ${proof.run} attempt ${proof.attempt}; exact tree differs only in mandatory-tested selection policy`,
      ],
    };
  } catch (error) {
    return {
      ...selection,
      qualificationReuse: {
        ...evidence,
        reason:
          error instanceof Error
            ? error.message.split("\n")[0]
            : "Qualification proof failed",
      },
    };
  }
}

async function main() {
  const event = JSON.parse(
    fs.readFileSync(process.env.GITHUB_EVENT_PATH, "utf8"),
  );
  const kind = process.env.GITHUB_EVENT_NAME;
  const ref = process.env.GITHUB_REF;
  const head = process.env.CI_HEAD_SHA;
  const git = (args) =>
    execFileSync("git", args, {
      encoding: "utf8",
      maxBuffer: 32 * 1024 * 1024,
      stdio: ["ignore", "pipe", "pipe"],
    }).trimEnd();
  let paths = [],
    pendingPaths = [],
    documentationPaths = [],
    pendingDocumentationPaths = [],
    fallback,
    base,
    deployment;
  try {
    if (git(["rev-parse", "HEAD"]) !== head)
      throw new Error("Checkout does not match requested revision");
    if (kind === "pull_request") {
      base = git(["merge-base", event.pull_request.base.sha, head]);
      paths = changedPaths(base, head);
      documentationPaths = documentationOnlyPaths(base, head, paths);
    } else if (kind === "push") {
      base = event.before;
      paths = changedPaths(base, head);
      documentationPaths = documentationOnlyPaths(base, head, paths);
      if (ref === "refs/heads/main") {
        const request = async (route) => {
          const response = await fetch(`https://api.github.com${route}`, {
            headers: {
              Authorization: `Bearer ${process.env.GITHUB_TOKEN}`,
              Accept: "application/vnd.github+json",
              "X-GitHub-Api-Version": "2022-11-28",
            },
            signal: AbortSignal.timeout(30000),
          });
          if (!response.ok)
            throw new Error(
              `Deployment lookup returned HTTP ${response.status}`,
            );
          return response.json();
        };
        deployment = await lastSuccessfulDeployment(
          process.env.GITHUB_REPOSITORY,
          request,
        );
        git(["merge-base", "--is-ancestor", deployment.revision, head]);
        pendingPaths = changedPaths(deployment.revision, head);
        pendingDocumentationPaths = documentationOnlyPaths(
          deployment.revision,
          head,
          pendingPaths,
        );
      }
    } else if (!["workflow_dispatch", "schedule"].includes(kind))
      throw new Error("Unrecognized workflow event");
  } catch (error) {
    // Never convert lookup, shallow-history, or diff errors into permission to skip.
    fallback = error.message.split("\n")[0];
  }
  const originalSelection = chooseChecks({
    event: kind,
    ref,
    paths,
    pendingPaths,
    documentationPaths,
    pendingDocumentationPaths,
    fallback,
    mode: event.inputs?.mode || "validate",
  });
  const selection = await reuseReferenceQualification({
    repository: process.env.GITHUB_REPOSITORY,
    kind,
    ref,
    event,
    head,
    selection: originalSelection,
    fallback,
    mode: event.inputs?.mode || "validate",
  });
  const report = {
    revision: head,
    event: kind,
    base,
    deployment,
    paths,
    pendingPaths,
    documentationPaths,
    pendingDocumentationPaths,
    fallback,
    ...selection,
  };
  fs.mkdirSync(".ci-evidence", { recursive: true });
  fs.writeFileSync(
    ".ci-evidence/selection.json",
    JSON.stringify(report, null, 2) + "\n",
  );
  for (const key of ["framework", "semantics", "website", "deploy", "cold"])
    fs.appendFileSync(process.env.GITHUB_OUTPUT, `${key}=${selection[key]}\n`);
  const rows = ["framework", "semantics", "website", "deploy", "cold"]
    .map((key) => `| ${key} | ${selection[key]} |`)
    .join("\n");
  fs.appendFileSync(
    process.env.GITHUB_STEP_SUMMARY,
    `## Check selection\n\n| Decision | Selected |\n| --- | --- |\n${rows}\n\nChanged paths: ${paths.length}. Unshipped paths: ${pendingPaths.length}. See the retained selection report for paths, reasons and baseline.\n`,
  );
  console.log(JSON.stringify(report));
}
if (process.argv[1] === fileURLToPath(import.meta.url)) await main();
