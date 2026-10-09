import fs from "node:fs";
import path from "node:path";
import { execFileSync, spawn } from "node:child_process";
import { fileURLToPath } from "node:url";

const sampleBytes = 16 * 1024;
const memoryKeys = new Set(["low", "high", "max", "oom", "oom_kill", "oom_group_kill"]);

// Only fixed action categories and validated scenario names leave the private log.
// Compiler arguments, runtime output, environment and credentials are excluded.
export function summarizeActions(text) {
  const actions = [];
  for (const line of text.split("\n")) {
    const running = line.match(/^Running\[(\d+)\]:/);
    if (!running) continue;
    const executable = line.match(/\b(ocamlopt(?:\.opt)?|ocamlc(?:\.opt)?|ocamldep(?:\.opt)?|agent_server_e2e\.exe|ochat_agent_server\.exe)\b/);
    const scenario = line.match(/--scenario ([a-z0-9_.-]{1,80})(?:[ )]|$)/);
    actions.push({ action: running[1], executable: executable?.[1] ?? "other", ...(scenario ? { scenario: scenario[1] } : {}) });
  }
  return actions.slice(-8);
}

export function readProgress(filename, offset) {
  let fd;
  try {
    fd = fs.openSync(filename, "r");
    const size = fs.fstatSync(fd).size;
    const previous = size < offset ? 0 : offset;
    const position = Math.max(previous, size - sampleBytes);
    const buffer = Buffer.alloc(Math.min(sampleBytes, size - position));
    const count = fs.readSync(fd, buffer, 0, buffer.length, position);
    return { offset: size, newBytes: size - previous, actions: summarizeActions(buffer.subarray(0, count).toString("utf8")) };
  } catch (error) {
    return { offset, unavailable: error.code ?? "read_failed" };
  } finally {
    if (fd !== undefined) fs.closeSync(fd);
  }
}

export function memoryEvents(text) {
  return Object.fromEntries(text.split("\n").flatMap((line) => {
    const match = line.match(/^([a-z_]+) (\d+)$/);
    return match && memoryKeys.has(match[1]) ? [[match[1], match[2]]] : [];
  }));
}

function resourceSample() {
  const result = {};
  try {
    // No args or environment; highest RSS processes first, bounded output.
    result.processes = execFileSync("ps", ["-eo", "pid,ppid,rss,comm", "--sort=-rss"], { encoding: "utf8", timeout: 2000, maxBuffer: 1024 * 1024 }).split("\n").slice(0, 17).map((line) => line.slice(0, 160));
  } catch (error) { result.processesUnavailable = error.code ?? "ps_failed"; }
  try {
    const entry = fs.readFileSync("/proc/self/cgroup", "utf8").split("\n").find((line) => line.startsWith("0::"));
    if (entry) {
      const directory = path.resolve("/sys/fs/cgroup", "." + entry.slice(3));
      if (directory === "/sys/fs/cgroup" || directory.startsWith("/sys/fs/cgroup/")) {
        result.memoryEvents = memoryEvents(fs.readFileSync(path.join(directory, "memory.events"), "utf8"));
      }
    }
  } catch (error) { result.memoryUnavailable = error.code ?? "cgroup_failed"; }
  return result;
}

// A dedicated monitor keeps sampling while the parent retains its exact
// synchronous Dune invocation. EOF is its sole parent-lifetime release signal.
export function startProgressMonitor(filename) {
  const child = spawn(process.execPath, [fileURLToPath(import.meta.url), "--monitor", filename], { stdio: ["pipe", "inherit", "inherit"] });
  const finished = new Promise((resolve) => {
    child.once("error", () => resolve());
    child.once("close", () => resolve());
  });
  // Early monitor exit is diagnostic failure, never the test's primary outcome.
  child.stdin.on("error", () => {});
  return { async close() { child.stdin.end(); await finished; } };
}

if (process.argv[1] === fileURLToPath(import.meta.url) && process.argv[2] === "--monitor") {
  let offset = 0;
  const sample = () => {
    const progress = readProgress(process.argv[3], offset);
    offset = progress.offset;
    console.log(JSON.stringify({ frameworkProgress: new Date().toISOString(), ...progress, ...resourceSample() }));
  };
  sample();
  const timer = setInterval(sample, 30_000);
  process.stdin.resume();
  process.stdin.once("end", () => { clearInterval(timer); sample(); });
}
