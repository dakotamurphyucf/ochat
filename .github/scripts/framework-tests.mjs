import fs from "node:fs";
import path from "node:path";
import { execFileSync, spawnSync } from "node:child_process";
import { startProgressMonitor } from "./framework-progress.mjs";
import { loadCurrentPlan } from "./framework-plan.mjs";
const tier = process.argv[2];
const shardIndex =
  process.argv[3] === undefined ? undefined : Number(process.argv[3]);
const plan =
  tier === "normal" && shardIndex !== undefined ? loadCurrentPlan() : undefined;
const shard = plan?.shards.find(({ index }) => index === shardIndex);
if (shardIndex !== undefined && !shard)
  throw new Error("Invalid normal shard index");
// Match the concurrency used for local qualification. Several persistence
// scenarios do CPU work inside bounded foreground operations.
const commands = {
  normal: shard
    ? ["build", "--force", "-j", "2", ...shard.aliases]
    : ["runtest", "--force", "-j", "2"],
  e2e: ["build", "--force", "-j", "2", "@agent-e2e-pr"],
  "git-native": [
    "build",
    "--force",
    "-j",
    "2",
    "@test/chatml_git_host/runtest",
    "@test/bounded_process/runtest",
    "@test/private_storage/runtest",
  ],
};
if (!Object.hasOwn(commands, tier))
  throw new Error("Expected normal, e2e or git-native");
if (
  (tier === "git-native" || tier === "normal") &&
  !path.isAbsolute(process.env.OCHAT_TEST_GIT_2_56_DIRECTORY ?? "")
)
  throw new Error(
    "Pinned Git fixture provisioning is required before native tests",
  );
// Cold Linux run 38278439617 exhausted 60 minutes with all 12,254 completed
// processes successful. Only the final repair partition was interrupted: it
// had 35.5 seconds to run versus 48.6 seconds in the previous CI run. Allow five
// minutes of aggregate headroom for compilation and the complete normal suite;
// individual test/runtime deadlines and two-worker concurrency remain unchanged.
const timeoutMinutes = { normal: 65, e2e: 25, "git-native": 45 };
const timeoutMs = timeoutMinutes[tier] * 60 * 1000;
fs.mkdirSync(".ci-evidence", { recursive: true });
// Verbose output identifies started actions even while their output is buffered.
// Write the trace inside the uploaded evidence directory, including on timeout.
const command = [
  ...commands[tier],
  "--root=.",
  "--display=verbose",
  `--trace-file=${path.resolve(`.ci-evidence/${tier}-dune-trace.json`)}`,
];
const start = Date.now();
const report = {
  tier,
  architecture: process.arch,
  revision: execFileSync("git", ["rev-parse", "HEAD"], {
    encoding: "utf8",
  }).trim(),
  command: ["dune", ...command],
  timeoutSeconds: timeoutMs / 1000,
  startedAt: new Date(start).toISOString(),
  result: "fail",
};
const log = fs.openSync(`.ci-evidence/${tier}.log`, "w");
const monitor =
  tier === "e2e" ? startProgressMonitor(`.ci-evidence/${tier}.log`) : undefined;
let result;
try {
  result = spawnSync("dune", command, {
    stdio: ["ignore", log, log],
    timeout: timeoutMs,
  });
} finally {
  fs.closeSync(log);
  await monitor?.close();
}
// Retain the build log under the same artifact root rather than relying on a
// separate upload glob. Missing diagnostics must not hide the primary result.
const duneLog = path.join(process.env.DUNE_BUILD_DIR ?? "_build", "log");
try {
  fs.copyFileSync(duneLog, `.ci-evidence/${tier}-dune.log`);
  report.duneLog = { source: duneLog, status: "copied" };
} catch (error) {
  report.duneLog = {
    source: duneLog,
    status: error.code === "ENOENT" ? "missing" : "error",
    error: error.message,
  };
}
Object.assign(report, {
  result: result.status === 0 && !result.error ? "pass" : "fail",
  exitCode: result.status,
  signal: result.signal,
  error: result.error?.message,
  durationSeconds: (Date.now() - start) / 1000,
});
fs.writeFileSync(
  `.ci-evidence/${tier}.json`,
  JSON.stringify(report, null, 2) + "\n",
);
if (shard)
  fs.writeFileSync(
    ".ci-evidence/normal-shard.json",
    JSON.stringify(
      {
        version: 1,
        revision: plan.revision,
        sourceHash: plan.sourceHash,
        runId: plan.runId,
        runAttempt: process.env.GITHUB_RUN_ATTEMPT ?? "local",
        index: shard.index,
        aliases: shard.aliases,
        result: report.result,
        durationSeconds: report.durationSeconds,
      },
      null,
      2,
    ) + "\n",
  );
console.log(fs.readFileSync(`.ci-evidence/${tier}.log`, "utf8"));
console.log(JSON.stringify(report));
if (process.env.GITHUB_STEP_SUMMARY)
  fs.appendFileSync(
    process.env.GITHUB_STEP_SUMMARY,
    `Framework ${tier}: **${report.result}** in ${report.durationSeconds}s. Full command output and E2E artifacts retained.\n`,
  );
process.exitCode = report.result === "pass" ? 0 : 1;
