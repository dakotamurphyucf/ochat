import fs from "node:fs";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { chooseChecks } from "./ci-policy.mjs";

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
    fallback,
    base,
    deployment;
  try {
    if (git(["rev-parse", "HEAD"]) !== head)
      throw new Error("Checkout does not match requested revision");
    if (kind === "pull_request") {
      base = git(["merge-base", event.pull_request.base.sha, head]);
      paths = changedPaths(base, head);
    } else if (kind === "push") {
      base = event.before;
      paths = changedPaths(base, head);
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
      }
    } else if (!["workflow_dispatch", "schedule"].includes(kind))
      throw new Error("Unrecognized workflow event");
  } catch (error) {
    // Never convert lookup, shallow-history, or diff errors into permission to skip.
    fallback = error.message.split("\n")[0];
  }
  const selection = chooseChecks({
    event: kind,
    ref,
    paths,
    pendingPaths,
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
