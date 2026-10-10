import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFileSync, spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import { summarizeActions, readProgress, memoryEvents, e2eIdentity } from "../scripts/framework-progress.mjs";

const script = fileURLToPath(new URL("../scripts/framework-progress.mjs", import.meta.url));

test("progress reports action identity without compiler args, credentials or runtime text", () => {
  const secret = "bearer-secret-must-not-leave-log";
  const input = `Running[42]: (cd _build && ocamlopt.opt --token ${secret})\nRunning[43]: agent_server_e2e.exe --scenario conformance.pending-inputs\nAuthorization: Bearer ${secret}\n`;
  const result = summarizeActions(input);
  assert.deepEqual(result, [{ action: "42", executable: "ocamlopt.opt" }, { action: "43", executable: "agent_server_e2e.exe", scenario: "conformance.pending-inputs" }]);
  assert.equal(JSON.stringify(result).includes(secret), false);
  assert.equal(summarizeActions(Array.from({ length: 100 }, (_, i) => `Running[${i}]: unknown`).join("\n")).length, 8);
});

test("bounded tail tracks appended bytes, quiet progress and truncation", () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "framework-progress-"));
  const filename = path.join(directory, "e2e.log");
  try {
    fs.writeFileSync(filename, "Running[1]: ocamlc.opt\n" + "x".repeat(20_000) + "\nRunning[2]: ocamlopt.opt\n");
    const first = readProgress(filename, 0);
    assert.deepEqual(first.actions, [{ action: "2", executable: "ocamlopt.opt" }]);
    assert.equal(first.newBytes, fs.statSync(filename).size);
    assert.equal(readProgress(filename, first.offset).newBytes, 0);
    fs.appendFileSync(filename, "Running[3]: agent_server_e2e.exe --scenario process-harness\n");
    assert.deepEqual(readProgress(filename, first.offset).actions, [{ action: "3", executable: "agent_server_e2e.exe", scenario: "process-harness" }]);
    fs.writeFileSync(filename, "Running[4]: ocamlc.opt\n");
    assert.equal(readProgress(filename, first.offset).actions[0].action, "4");
    assert.deepEqual(memoryEvents("oom 2\noom_kill 1\nsecret bearer-token\nunrecognized 999\n"), { oom: "2", oom_kill: "1" });
  } finally { fs.rmSync(directory, { recursive: true, force: true }); }
});

test("monitor exits on parent lifetime EOF without waiting for a sample interval", { timeout: 10_000 }, async () => {
  const child = spawn(process.execPath, [script, "--monitor", "missing-progress-log"], { stdio: ["pipe", "pipe", "pipe"] });
  let output = "";
  child.stdout.on("data", (data) => { output += data; });
  child.stderr.resume();
  const finished = new Promise((resolve, reject) => { child.once("error", reject); child.once("close", (code, signal) => resolve({ code, signal })); });
  try {
    child.stdin.end();
    assert.deepEqual(await finished, { code: 0, signal: null });
    const records = output.trim().split("\n").map((line) => JSON.parse(line));
    assert.equal(records.every((record) => record.unavailable === "ENOENT"), true);
    assert.equal(records.some((record) => "processes" in record || "processesUnavailable" in record), true);
  } finally { if (child.exitCode === null) child.kill("SIGKILL"); await finished; }
});

const frameworkScript = fileURLToPath(new URL("../scripts/framework-tests.mjs", import.meta.url));

async function runFrameworkFixture(tier, duneExit) {
  const directory = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), "framework-wrapper-")));
  let child;
  let finished;
  let timer;
  try {
    // Local config arguments create a disposable repo; user/global Git config
    // and actual Dune executables are never changed or invoked.
    execFileSync("git", ["init", "--quiet", directory], { timeout: 5000 });
    fs.writeFileSync(path.join(directory, "fixture.txt"), "framework fixture\n");
    execFileSync("git", ["-C", directory, "add", "fixture.txt"], { timeout: 5000 });
    execFileSync("git", ["-C", directory, "-c", "user.name=Framework fixture", "-c", "user.email=fixture@example.invalid", "-c", "commit.gpgsign=false", "commit", "--quiet", "-m", "fixture"], { timeout: 5000 });
    const revision = execFileSync("git", ["-C", directory, "rev-parse", "HEAD"], { encoding: "utf8", timeout: 5000 }).trim();
    const bin = path.join(directory, "bin");
    fs.mkdirSync(bin);
    const fake = path.join(bin, "fake-dune.mjs");
    fs.writeFileSync(fake, `import fs from "node:fs";
fs.writeFileSync("invocation.json", JSON.stringify(process.argv.slice(2)));
fs.mkdirSync("_build");
fs.writeFileSync("_build/log", "fake dune build log\\n");
console.log("Running[77]: agent_server_e2e.exe --scenario fake-fixture");
process.exitCode = Number(process.env.FAKE_DUNE_EXIT);
`);
    const quote = (value) => "'" + value.replaceAll("'", "'\\''") + "'";
    fs.writeFileSync(path.join(bin, "dune"), `#!/bin/sh\nexec ${quote(process.execPath)} ${quote(fake)} "$@"\n`, { mode: 0o700 });
    child = spawn(process.execPath, [frameworkScript, tier], {
      cwd: directory,
      env: { ...process.env, PATH: bin + path.delimiter + process.env.PATH, FAKE_DUNE_EXIT: String(duneExit), DUNE_BUILD_DIR: "_build", GITHUB_STEP_SUMMARY: path.join(directory, "summary.md") },
      stdio: ["ignore", "pipe", "pipe"],
    });
    let output = "";
    let errors = "";
    child.stdout.on("data", (data) => { output += data; });
    child.stderr.on("data", (data) => { errors += data; });
    finished = new Promise((resolve, reject) => {
      child.once("error", reject);
      child.once("close", (code, signal) => resolve({ code, signal }));
    });
    // Also bound the owned process if a wrapper/monitor cleanup regression
    // prevents close. Parent death closes the monitor's lifetime pipe.
    timer = setTimeout(() => child.kill("SIGKILL"), 5000);
    const exit = await finished;
    assert.equal(exit.signal, null, `wrapper or monitor cleanup did not finish: ${errors}`);
    assert.equal(exit.code, duneExit === 0 ? 0 : 1);
    const report = JSON.parse(fs.readFileSync(path.join(directory, ".ci-evidence", `${tier}.json`), "utf8"));
    const invocation = JSON.parse(fs.readFileSync(path.join(directory, "invocation.json"), "utf8"));
    assert.equal(report.revision, revision);
    assert.equal(report.result, duneExit === 0 ? "pass" : "fail");
    assert.equal(report.exitCode, duneExit);
    assert.equal(report.signal, null);
    assert.equal(report.timeoutSeconds, tier === "e2e" ? 1500 : 3600);
    assert.deepEqual(report.command, ["dune", ...invocation]);
    assert.deepEqual(invocation, [tier === "e2e" ? "build" : "runtest", "--force", "-j", "2", ...(tier === "e2e" ? ["@agent-e2e-pr"] : []), "--display=verbose", `--trace-file=${path.join(directory, ".ci-evidence", `${tier}-dune-trace.json`)}`]);
    assert.equal(fs.readFileSync(path.join(directory, ".ci-evidence", `${tier}-dune.log`), "utf8"), "fake dune build log\n");
    const progress = output.split("\n").filter((line) => line.startsWith('{"frameworkProgress":')).map((line) => JSON.parse(line));
    if (tier === "e2e") {
      assert.ok(progress.length > 0, "actual wrapper must start and join its monitor");
      assert.ok(progress.some((record) => record.actions?.some((action) => action.action === "77")));
    } else {
      assert.deepEqual(progress, [], "normal tier must not spawn a monitor");
    }
  } finally {
    clearTimeout(timer);
    if (child && child.exitCode === null && child.signalCode === null) child.kill("SIGKILL");
    if (finished) await finished;
    fs.rmSync(directory, { recursive: true, force: true });
  }
}

for (const [tier, duneExit] of [["e2e", 0], ["e2e", 7], ["normal", 0]]) {
  test(`actual framework wrapper preserves ${tier} exit ${duneExit} and closes owned monitor`, { timeout: 10_000 }, async () => {
    await runFrameworkFixture(tier, duneExit);
  });
}

test("actual coloured Dune verbose output yields sanitized action and executable", () => {
  const line = "\u001b[1;34mRunning\u001b[0m[\u001b[1;33m123\u001b[0m]: (cd _build/default && /host/bin/\u001b[34;102mocamlopt\u001b[0m.opt --private secret)";
  assert.deepEqual(summarizeActions(line), [{ action: "123", executable: "ocamlopt.opt" }]);
  const e2e = "  \u001b[1;34mRunning\u001b[0m[\u001b[1;33m124\u001b[0m]: agent_server_e2e.exe --scenario crash-matrix --case generated.creator-outcome-recovery";
  assert.equal(summarizeActions(e2e)[0].scenario, "crash-matrix");
});

test("proc identity admits only the E2E executable and bounded scenario/case fields", () => {
  const args = ["/host/private/path/agent_server_e2e.exe", "--scenario", "crash-matrix", "--case", "generated.creator-outcome-recovery", "--token", "never-disclose"].join("\0") + "\0";
  assert.deepEqual(e2eIdentity("13383", args), { pid: "13383", executable: "agent_server_e2e.exe", scenario: "crash-matrix", case: "generated.creator-outcome-recovery" });
  assert.equal(e2eIdentity("1", "/host/ochat_agent_server.exe\0--token\0never-disclose\0"), undefined);
  assert.equal(e2eIdentity("../1", args), undefined);
  assert.deepEqual(e2eIdentity("2", "agent_server_e2e.exe\0--scenario\0private/path\0--case\0" + "x".repeat(81) + "\0"), { pid: "2", executable: "agent_server_e2e.exe" });
});
