import fs from "node:fs";
import path from "node:path";
import { execFileSync, spawnSync } from "node:child_process";
import { startProgressMonitor } from "./framework-progress.mjs";
const tier = process.argv[2];
// Match the concurrency used for local qualification. Several persistence
// scenarios do CPU work inside bounded foreground operations.
const commands = {
  normal: ["runtest", "--force", "-j", "2"],
  e2e: ["build", "--force", "-j", "2", "@agent-e2e-pr"],
};
if (!Object.hasOwn(commands, tier)) throw new Error("Expected normal or e2e");
// An uncached Linux run reached the 45-minute aggregate limit with late tests
// still running and no completed process failures. Its CPU-heavy tests took
// 1.4–1.8 times the passing PR run; trace-based completion estimates were 52–56
// minutes. Allow a bounded hour for compilation and the complete normal suite;
// individual test/runtime deadlines and two-worker concurrency remain unchanged.
const timeoutMs = (tier === "normal" ? 60 : 25) * 60 * 1000;
fs.mkdirSync(".ci-evidence", { recursive: true });
// Verbose output identifies started actions even while their output is buffered.
// Write the trace inside the uploaded evidence directory, including on timeout.
const command = [
  ...commands[tier],
  "--display=verbose",
  `--trace-file=${path.resolve(`.ci-evidence/${tier}-dune-trace.json`)}`,
];
const start = Date.now();
const report = {
  tier,
  revision: execFileSync("git", ["rev-parse", "HEAD"], {
    encoding: "utf8",
  }).trim(),
  command: ["dune", ...command],
  timeoutSeconds: timeoutMs / 1000,
  startedAt: new Date(start).toISOString(),
  result: "fail",
};
const log = fs.openSync(`.ci-evidence/${tier}.log`, "w");
const monitor = tier === "e2e" ? startProgressMonitor(`.ci-evidence/${tier}.log`) : undefined;
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
console.log(fs.readFileSync(`.ci-evidence/${tier}.log`, "utf8"));
console.log(JSON.stringify(report));
if (process.env.GITHUB_STEP_SUMMARY)
  fs.appendFileSync(
    process.env.GITHUB_STEP_SUMMARY,
    `Framework ${tier}: **${report.result}** in ${report.durationSeconds}s. Full command output and E2E artifacts retained.\n`,
  );
process.exitCode = report.result === "pass" ? 0 : 1;
