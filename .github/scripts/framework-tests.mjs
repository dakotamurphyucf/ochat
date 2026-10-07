import fs from "node:fs";
import { execFileSync, spawnSync } from "node:child_process";
const tier = process.argv[2];
// Match the concurrency used for local qualification. Several persistence
// scenarios do CPU work inside bounded foreground operations.
const commands = {
  normal: ["runtest", "--force", "-j", "2"],
  e2e: ["build", "--force", "-j", "2", "@agent-e2e-pr"],
};
if (!Object.hasOwn(commands, tier)) throw new Error("Expected normal or e2e");
// The complete normal suite takes about 31 minutes with two workers. Bound the
// aggregate run separately from individual test and runtime execution deadlines.
const timeoutMs = (tier === "normal" ? 35 : 25) * 60 * 1000;
fs.mkdirSync(".ci-evidence", { recursive: true });
const start = Date.now();
const report = {
  tier,
  revision: execFileSync("git", ["rev-parse", "HEAD"], {
    encoding: "utf8",
  }).trim(),
  command: ["dune", ...commands[tier]],
  timeoutSeconds: timeoutMs / 1000,
  startedAt: new Date(start).toISOString(),
  result: "fail",
};
const log = fs.openSync(`.ci-evidence/${tier}.log`, "w");
const result = spawnSync("dune", commands[tier], {
  stdio: ["ignore", log, log],
  timeout: timeoutMs,
});
fs.closeSync(log);
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
