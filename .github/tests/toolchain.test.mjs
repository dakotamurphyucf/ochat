import test from "node:test";
import assert from "node:assert/strict";
import { cacheKey, keyInputs, checkInstalled } from "../scripts/toolchain.mjs";

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
