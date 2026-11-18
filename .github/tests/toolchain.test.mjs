import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { bootstrapEnvironment, bootstrapCommands } from "../../scripts/bootstrap-opam.mjs";
import { createHash } from "node:crypto";
import {
  overlayInputs, overlayInventory, checkOverlayInventory, checkOverlay, checkOverlayMetadata, checkOverlayRepository,
  cacheKey,
  describeCacheIdentity,
  keyInputs,
  checkInstalled,
  checkRunner,
} from "../scripts/toolchain.mjs";

const checkout = fileURLToPath(new URL("../../", import.meta.url));
const readCheckout = (name) => fs.readFileSync(path.join(checkout, name));
const overlayConfig = () => JSON.parse(readCheckout(".github/ci-toolchain.json"));

test("reviewed overlay rejects content, family and installed-definition drift", () => {
  const config = overlayConfig();
  checkOverlay(config, readCheckout);
  for (const changed of overlayInputs)
    assert.throws(() => checkOverlay(config, (name) => name === changed
      ? Buffer.concat([readCheckout(name), Buffer.from("changed")]) : readCheckout(name)),
    /overlay changed/);
  for (const name of config.eioOverlay.packages) {
    const changed = structuredClone(config);
    changed.packages[name] = "1.3";
    assert.throws(() => checkOverlay(changed, readCheckout), /family version mismatch/);
  }
  const canonical = 'depends: ["ocaml" {>= "5.2.0"}]\nbuild: [["dune" "build"]]\n';
  checkOverlayMetadata("eio", canonical, canonical);
  for (const changed of [canonical.replace("5.2.0", "5.3.0"),
    canonical.replace('"dune"', '"other"'), canonical + 'patches: ["extra.patch"]\n'])
    assert.throws(() => checkOverlayMetadata("eio", canonical, changed), /metadata differs/);
});

test("overlay inventory rejects unreviewed packages and links without following them", () => {
  checkOverlayInventory(overlayInventory(checkout));
  const temporary = fs.mkdtempSync(path.join(os.tmpdir(), "ochat-overlay-"));
  try {
    fs.mkdirSync(path.join(temporary, "tooling"));
    const root = path.join(temporary, "tooling/opam-overlay");
    fs.cpSync(path.join(checkout, "tooling/opam-overlay"), root, { recursive: true });
    checkOverlayInventory(overlayInventory(temporary));
    const extra = path.join(root, "packages/unreviewed");
    fs.mkdirSync(extra);
    assert.throws(() => checkOverlayInventory(overlayInventory(temporary)), /Unreviewed/);
    fs.rmdirSync(extra);
    const metadata = path.join(root, "repo");
    fs.unlinkSync(metadata);
    assert.throws(() => checkOverlayInventory(overlayInventory(temporary)), /Missing/);
    fs.symlinkSync("../../missing-target", metadata);
    assert.throws(() => checkOverlayInventory(overlayInventory(temporary)), /symlink/);
  } finally { fs.rmSync(temporary, { recursive: true, force: true }); }
});

test("bootstrap commands target one isolated environment and discard inherited switch selection", () => {
  const config = overlayConfig();
  const destination = path.join(os.tmpdir(), "new ochat environment");
  const commands = bootstrapCommands(config, destination, checkout);
  for (const args of commands)
    assert.ok(args.includes(`--root=${path.join(destination, "root")}`));
  assert.ok(commands[1].includes(path.join(destination, "switch")));
  assert.ok(commands[2].includes(`--switch=${path.join(destination, "switch")}`));
  assert.ok(commands[0].includes("--no-setup"));
  assert.ok(commands[2].includes("--this-switch"));
  const clean = bootstrapEnvironment({
    PATH: "/custom/old/bin:/usr/bin:/home/user/.opam/default/bin:/tmp/project/_opam/bin",
    OPAMROOT: "/custom", OPAMSWITCH: "old", OPAM_SWITCH_PREFIX: "/custom/old",
    OPAMCLI: "2.0", OPAMVAR_os: "other", OCAMLPATH: "/old/lib", CAML_LD_LIBRARY_PATH: "/old/stubs",
    HOME: "/home/user", KEEP: "unchanged",
  });
  assert.deepEqual(clean, { PATH: "/usr/bin", HOME: "/home/user", KEEP: "unchanged" });
});

test("bootstrap preflight refuses existing destinations and wrong opam before creating an environment", () => {
  const temporary = fs.mkdtempSync(path.join(os.tmpdir(), "ochat-bootstrap-"));
  try {
    const executable = path.join(temporary, "fake-opam");
    const calls = path.join(temporary, "calls");
    fs.writeFileSync(executable, `#!${process.execPath}\n` +
      'require("node:fs").appendFileSync(process.env.OCHAT_TEST_CALLS, JSON.stringify(process.argv.slice(2))+"\\n");\n' +
      'console.log("0.0.0");\n', { mode: 0o700 });
    const invoke = (destination) => spawnSync(process.execPath,
      [path.join(checkout, "scripts/bootstrap-opam.mjs"), destination, executable],
      { encoding: "utf8", env: { PATH: process.env.PATH, HOME: temporary, OCHAT_TEST_CALLS: calls } });
    const existing = path.join(temporary, "existing");
    fs.mkdirSync(existing);
    fs.writeFileSync(path.join(existing, "keep"), "unchanged");
    let result = invoke(existing);
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /destination must not exist/);
    assert.equal(fs.existsSync(calls), false);
    assert.equal(fs.readFileSync(path.join(existing, "keep"), "utf8"), "unchanged");
    const dangling = path.join(temporary, "dangling");
    fs.symlinkSync("missing", dangling);
    result = invoke(dangling);
    assert.notEqual(result.status, 0);
    assert.equal(fs.existsSync(calls), false);
    const absent = path.join(temporary, "new", "environment");
    result = invoke(absent);
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /Expected opam/);
    assert.deepEqual(fs.readFileSync(calls, "utf8").trim().split("\n").map(JSON.parse), [["--version"]]);
    assert.equal(fs.existsSync(path.dirname(absent)), false);
    fs.writeFileSync(executable, `#!${process.execPath}\n` +
      'require("node:fs").appendFileSync(process.env.OCHAT_TEST_CALLS, JSON.stringify(process.argv.slice(2))+"\\n");\n' +
      `console.log(${JSON.stringify(overlayConfig().opam)});\n`, { mode: 0o700 });
    fs.unlinkSync(calls);
    result = invoke(absent);
    assert.equal(result.status, 0, result.stderr);
    assert.deepEqual(fs.readFileSync(calls, "utf8").trim().split("\n").map(JSON.parse),
      [["--version"], ...bootstrapCommands(overlayConfig(), absent, checkout)]);
    const saved = JSON.parse(fs.readFileSync(path.join(absent, "environment.json")));
    assert.equal(saved.root, path.join(absent, "root"));
    assert.equal(saved.switch, path.join(absent, "switch"));
    assert.match(result.stdout, /Project dependencies are not installed/);
  } finally { fs.rmSync(temporary, { recursive: true, force: true }); }
});

test("committed dependency definition and Linux lock match the CI manifest", () => {
  const read = (name) =>
    fs.readFileSync(new URL(`../../${name}`, import.meta.url), "utf8");
  const config = JSON.parse(read(".github/ci-toolchain.json"));
  assert.equal(
    createHash("sha256").update(read("ochat.opam")).digest("hex"),
    config.opamFileSha256,
    "Review the base dependency changes against the Linux lock before refreshing its fingerprint",
  );
  // The generated lock deliberately uses exact versions for its entire closure.
  const lock = read("ochat.opam.locked");
  const dependencies = lock.match(/^depends: \[\n([\s\S]*?)^\]/m)?.[1];
  assert.ok(dependencies, "Missing locked dependency closure");
  const packages = Object.fromEntries(
    dependencies
      .trim()
      .split("\n")
      .map((line) => {
        const entry = line.match(/^\s*"([^"]+)" \{= "([^"]+)"\}\s*$/);
        assert.ok(entry, `Expected an exact locked version: ${line}`);
        return [entry[1], entry[2]];
      }),
  );
  assert.deepEqual(packages, config.packages);
  for (const pin of Object.values(config.sourcePins))
    assert.ok(lock.includes(`"${pin}"`), `Missing locked source pin: ${pin}`);
});

test("dependency cache invalidates for every definition, pin/lock/action change and runner/toolchain fingerprint", () => {
  const files = Object.fromEntries(
    keyInputs.map((name) => [name, `original ${name}`]),
  );
  const fingerprint = {
    platform: "linux",
    arch: "x64",
    image: "ubuntu24",
    imageVersion: "1",
    gcc: "13.3",
    cpuTarget: "-march=x86-64-v3 -mavx2 [enabled]",
    compiler: "5.3.0",
    opam: "2.5.2",
  };
  const original = cacheKey(fingerprint, (name) => files[name]);
  assert.equal(
    original,
    cacheKey(fingerprint, (name) => files[name]),
  );
  for (const name of keyInputs)
    assert.notEqual(
      original,
      cacheKey(fingerprint, (key) => (key === name ? "changed" : files[key])),
    );
  for (const name of Object.keys(fingerprint))
    assert.notEqual(
      original,
      cacheKey({ ...fingerprint, [name]: "changed" }, (key) => files[key]),
    );
});

test("restored dependencies must match all locked package versions", () => {
  const expected = { dune: "3.21.1", oniguruma: "0.1.2" };
  checkInstalled(expected, "# Packages\ndune 3.21.1\noniguruma 0.1.2\n");
  assert.throws(
    () => checkInstalled(expected, "dune 3.21.1\noniguruma 0.2\n"),
    /oniguruma/,
  );
  assert.throws(() => checkInstalled(expected, "dune 3.21.1\n"), /oniguruma/);
});

test("runner admission accepts exact Ubuntu 24.04 image/architecture pairs without sharing native caches", () => {
  const x64 = {
    platform: "linux",
    arch: "x64",
    image: "ubuntu24",
    imageVersion: "20261004.142.1",
  };
  // The official ubuntu24-arm64/20261004.142 image exports this ImageOS.
  const arm64 = { ...x64, arch: "arm64", image: "ubuntu24-arm64" };
  checkRunner(x64);
  checkRunner(arm64);
  assert.notEqual(
    cacheKey(x64, () => "source"),
    cacheKey(arm64, () => "source"),
  );

  for (const unsupported of [
    { ...x64, image: arm64.image },
    { ...arm64, image: x64.image },
    { ...x64, platform: "darwin" },
    { ...arm64, platform: "win32" },
    { ...x64, arch: "ia32" },
    { ...arm64, arch: "riscv64" },
    { ...x64, image: "ubuntu22" },
    { ...arm64, image: "ubuntu26-arm64" },
    { ...arm64, image: undefined },
    { ...x64, imageVersion: undefined },
    { ...arm64, imageVersion: "" },
    { ...x64, imageVersion: "  " },
  ]) {
    assert.throws(
      () => checkRunner(unsupported),
      (error) => {
        assert.match(error.message, /Ubuntu 24\.04 x64 or ARM64/);
        assert.ok(error.message.includes(JSON.stringify(unsupported)));
        return true;
      },
    );
  }
});

test("cache diagnostics explain native and dependency identity without changing the exact key", () => {
  const fingerprint = {
    platform: "linux",
    arch: "x64",
    image: "ubuntu24",
    imageVersion: "20261004.327.1",
    gcc: "13.3.0",
    cpuTarget: "-march=x86-64-v3 -mavx2 [enabled]",
    compiler: "5.3.0",
    opam: "2.5.2",
  };
  const read = (name) => `original ${name}`;
  const identity = describeCacheIdentity(fingerprint, read);
  assert.equal(identity.key, cacheKey(fingerprint, read));
  assert.equal(identity.compilationCachePrefix, `dune-${identity.key}-`);
  assert.equal(identity.imageVersion, fingerprint.imageVersion);
  assert.equal(identity.gcc, fingerprint.gcc);
  assert.equal(
    identity.cpuTargetSha256,
    createHash("sha256").update(fingerprint.cpuTarget).digest("hex"),
  );
  assert.equal(Object.hasOwn(identity, "cpuTarget"), false);
  const changedCpu = describeCacheIdentity(
    { ...fingerprint, cpuTarget: "other CPU" },
    read,
  );
  assert.notEqual(changedCpu.key, identity.key);
  assert.notEqual(changedCpu.cpuTargetSha256, identity.cpuTargetSha256);
  assert.equal(
    changedCpu.dependencyContractSha256,
    identity.dependencyContractSha256,
  );
  const changedContract = describeCacheIdentity(fingerprint, (name) =>
    name === keyInputs[0] ? "changed dependency" : read(name),
  );
  assert.notEqual(changedContract.key, identity.key);
  assert.notEqual(
    changedContract.dependencyContractSha256,
    identity.dependencyContractSha256,
  );
  assert.equal(changedContract.cpuTargetSha256, identity.cpuTargetSha256);
});

// Public opam 2.5.2 --all table uses padded columns; URLs may contain spaces.
test("overlay provenance requires the configured local repository URL", () => {
  const listing = (rows) => [
    "# Repository".padEnd(20) + "# Url".padEnd(100) + "# Switches(rank)",
    ...rows.map(([name, url]) => name.padEnd(20) + url.padEnd(100) + "local(1/2)"),
  ].join("\n");
  const target = "/tmp/ochat checkout/tooling/opam-overlay";
  const valid = "file:///tmp/ochat%20checkout/tooling/opam-overlay";
  assert.equal(checkOverlayRepository("ochat-eio", target,
    listing([["default", "https://example.invalid/repository"], ["ochat-eio", valid]])), valid);
  for (const wrong of ["file:///tmp/other", "https://example.invalid/repository",
    valid + "#other", valid + "?other", "file://remote/tmp/ochat%20checkout/tooling/opam-overlay"])
    assert.throws(() => checkOverlayRepository("ochat-eio", target,
      listing([["ochat-eio", wrong]])), /repository URL differs/);
  for (const invalid of ["changed format", listing([]),
    listing([["ochat-eio", valid], ["ochat-eio", valid]])])
    assert.throws(() => checkOverlayRepository("ochat-eio", target, invalid));
});
