import fs from "node:fs";
import path from "node:path";
import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { fileURLToPath, pathToFileURL } from "node:url";

export const overlayInputs = [
  "tooling/opam-overlay/LICENSE.eio",
  "tooling/opam-overlay/README.md",
  "tooling/opam-overlay/packages/eio/eio.1.3+ochat.1/files/eio-release-report.patch",
  "tooling/opam-overlay/packages/eio/eio.1.3+ochat.1/opam",
  "tooling/opam-overlay/packages/eio_linux/eio_linux.1.3+ochat.1/opam",
  "tooling/opam-overlay/packages/eio_main/eio_main.1.3+ochat.1/opam",
  "tooling/opam-overlay/packages/eio_posix/eio_posix.1.3+ochat.1/opam",
  "tooling/opam-overlay/repo"
];

export const overlayRoot = "tooling/opam-overlay";
export const overlayPackages = ["eio", "eio_linux", "eio_posix", "eio_main"];
const overlayVersion = "1.3+ochat.1";
const overlayPatch = `${overlayRoot}/packages/eio/eio.${overlayVersion}/files/eio-release-report.patch`;

export function overlayDigest(read = (name) => fs.readFileSync(name)) {
  const hash = createHash("sha256");
  for (const name of overlayInputs)
    hash.update(name).update("\0").update(read(name)).update("\0");
  return hash.digest("hex");
}

// Include directory entries so a newly introduced package cannot escape review.
// The filesystem walker never follows links; tests can supply the same entries.
export function checkOverlayInventory(entries) {
  const expected = new Map(overlayInputs.map((name) => [name, "file"]));
  for (const name of overlayInputs) {
    for (let parent = path.posix.dirname(name); parent !== overlayRoot;
         parent = path.posix.dirname(parent))
      expected.set(parent, "directory");
  }
  const seen = new Set();
  for (const { path: name, kind } of entries) {
    if (seen.has(name) || expected.get(name) !== kind)
      throw new Error(`Unreviewed Eio overlay entry: ${name} (${kind})`);
    seen.add(name);
  }
  for (const name of expected.keys())
    if (!seen.has(name)) throw new Error(`Missing Eio overlay entry: ${name}`);
}

export function overlayInventory(root = ".") {
  const entries = [];
  const walk = (relative) => {
    const stat = fs.lstatSync(path.join(root, relative));
    const kind = stat.isSymbolicLink() ? "symlink"
      : stat.isDirectory() ? "directory" : stat.isFile() ? "file" : "other";
    if (relative !== overlayRoot) entries.push({ path: relative, kind });
    if (kind !== "directory") {
      if (relative === overlayRoot)
        throw new Error("Eio overlay root must be a real directory");
      return;
    }
    for (const name of fs.readdirSync(path.join(root, relative)).sort())
      walk(`${relative}/${name}`);
  };
  walk(overlayRoot);
  return entries;
}

export function checkOverlay(config, read = (name) => fs.readFileSync(name)) {
  const overlay = config.eioOverlay;
  if (!overlay || overlay.name !== "ochat-eio" || overlay.path !== overlayRoot
      || overlay.version !== overlayVersion
      || JSON.stringify(overlay.packages) !== JSON.stringify(overlayPackages))
    throw new Error("Unexpected Eio overlay package contract");
  const sha256 = overlayDigest(read);
  const patchSha256 = createHash("sha256").update(read(overlayPatch)).digest("hex");
  if (sha256 !== overlay.sha256 || patchSha256 !== overlay.patchSha256)
    throw new Error("Eio overlay changed: review patch/metadata and qualify provenance");
  for (const name of overlayPackages)
    if (config.packages[name] !== overlayVersion)
      throw new Error(`Eio family version mismatch: ${name}`);
  return { sha256, patchSha256, version: overlayVersion, packages: [...overlayPackages] };
}

export function checkOverlayMetadata(packageName, expectedCanonical, installedCanonical) {
  if (expectedCanonical !== installedCanonical)
    throw new Error(`Installed Eio overlay metadata differs: ${packageName}`);
}

// opam 2.5.2 has no repository get-url/columns query. Its public --all
// table supplies the URL; use the header's column boundaries, never split a
// local path on spaces. Fail closed if the public output shape changes.
export function checkOverlayRepository(name, expectedPath, listing) {
  const lines = listing.split(/\r?\n/).filter((line) => line.trim());
  const header = lines[0] ?? "";
  const urlStart = header.indexOf("# Url");
  const switchesStart = header.indexOf("# Switches");
  if (!header.startsWith("# Repository") || urlStart <= 0 || switchesStart <= urlStart)
    throw new Error("Unexpected opam repository URL listing");
  const matches = lines.slice(1).filter((line) => line.slice(0, urlStart).trim() === name);
  if (matches.length !== 1)
    throw new Error(`Missing or duplicate Eio repository URL: ${name}`);
  const actual = matches[0].slice(urlStart, switchesStart).trim();
  const expected = pathToFileURL(path.resolve(expectedPath)).href;
  let configured;
  try { configured = new URL(actual); }
  catch { throw new Error(`Invalid Eio repository URL: ${actual}`); }
  if (configured.protocol !== "file:" || configured.host || configured.search || configured.hash
      || path.resolve(fileURLToPath(configured)) !== path.resolve(expectedPath))
    throw new Error(`Eio repository URL differs: expected ${expected}, found ${actual}`);
  return actual;
}

export const keyInputs = [
  "ochat.opam",
  "ochat.opam.locked",
  "dune-project",
  ".github/ci-toolchain.json",
  ".github/actions/setup-ochat/action.yml",
  ".github/scripts/toolchain.mjs",
  "scripts/bootstrap-opam.mjs",
  ...overlayInputs,
];
export function cacheKey(fingerprint, read = (name) => fs.readFileSync(name)) {
  const hash = createHash("sha256").update(JSON.stringify(fingerprint));
  for (const name of keyInputs)
    hash.update(name).update("\0").update(read(name)).update("\0");
  return `ochat-opam-v1-${hash.digest("hex")}`;
}
// Diagnostics use exactly the same inputs as cacheKey. CPU details remain in
// the existing evidence fingerprint; the summary contains only their digest.
export function describeCacheIdentity(
  fingerprint,
  read = (name) => fs.readFileSync(name),
) {
  const dependencyContract = createHash("sha256");
  for (const name of keyInputs)
    dependencyContract
      .update(name)
      .update("\0")
      .update(read(name))
      .update("\0");
  const key = cacheKey(fingerprint, read);
  return {
    key,
    compilationCachePrefix: `dune-${key}-`,
    platform: fingerprint.platform,
    arch: fingerprint.arch,
    image: fingerprint.image,
    imageVersion: fingerprint.imageVersion,
    gcc: fingerprint.gcc,
    cpuTargetSha256: createHash("sha256")
      .update(fingerprint.cpuTarget)
      .digest("hex"),
    dependencyContractSha256: dependencyContract.digest("hex"),
    compiler: fingerprint.compiler,
    opam: fingerprint.opam,
  };
}

export function checkRunner(fingerprint) {
  const supportedImage =
    (fingerprint.arch === "x64" && fingerprint.image === "ubuntu24") ||
    (fingerprint.arch === "arm64" && fingerprint.image === "ubuntu24-arm64");
  if (
    fingerprint.platform !== "linux" ||
    !supportedImage ||
    typeof fingerprint.imageVersion !== "string" ||
    !fingerprint.imageVersion.trim()
  )
    throw new Error(
      "The dependency cache requires GitHub Ubuntu 24.04 x64 or ARM64 runners; " +
        `found ${JSON.stringify({
          platform: fingerprint.platform,
          arch: fingerprint.arch,
          image: fingerprint.image,
          imageVersion: fingerprint.imageVersion,
        })}`,
    );
}

export function checkInstalled(expected, output) {
  const installed = new Map(
    output
      .split("\n")
      .filter((line) => line.trim() && !line.startsWith("#"))
      .map((line) => line.trim().split(/\s+/)),
  );
  for (const [name, version] of Object.entries(expected)) {
    if (installed.get(name) !== version)
      throw new Error(
        `${name}: expected ${version}, found ${installed.get(name)}`,
      );
  }
}

function main() {
  const config = JSON.parse(
    fs.readFileSync(".github/ci-toolchain.json", "utf8"),
  );
  const run = (command, args) =>
    execFileSync(command, args, { encoding: "utf8" }).trim();
  const mode = process.argv[2];
  checkOverlayInventory(overlayInventory());
  const eioOverlay = checkOverlay(config);
  fs.mkdirSync(".ci-evidence", { recursive: true });
  if (mode === "overlay") {
    if (run("opam", ["--version"]) !== config.opam)
      throw new Error(`Expected opam ${config.opam} before overlay registration`);
    run("opam", ["repository", "add", config.eioOverlay.name,
      path.resolve(config.eioOverlay.path), "--this-switch", "--rank=1", "--yes"]);
  } else if (mode === "key") {
    const base = createHash("sha256")
      .update(fs.readFileSync("ochat.opam"))
      .digest("hex");
    if (base !== config.opamFileSha256)
      throw new Error(
        "Dependency definition changed: regenerate and qualify the Linux lock",
      );
    const fingerprint = {
      platform: process.platform,
      arch: process.arch,
      image: process.env.ImageOS,
      imageVersion: process.env.ImageVersion,
      gcc: run("gcc", ["-dumpfullversion"]),
      // Hosted x64 runners can expose different instruction sets. Native C
      // dependencies must not be restored onto an incompatible CPU.
      cpuTarget: run("gcc", ["-march=native", "-Q", "--help=target"]),
      compiler: config.compiler,
      opam: config.opam,
    };
    checkRunner(fingerprint);
    const identity = describeCacheIdentity(fingerprint);
    const { key } = identity;
    fs.appendFileSync(
      process.env.GITHUB_OUTPUT,
      `key=${key}\ncompilation-cache-prefix=${identity.compilationCachePrefix}\nrepository=${config.repository}\n`,
    );
    fs.writeFileSync(
      ".ci-evidence/toolchain-key.json",
      JSON.stringify({ key, fingerprint, identity }, null, 2) + "\n",
    );
  } else if (mode === "verify") {
    const installed = run("opam", [
      "list",
      "--installed",
      "--columns=name,version",
      "--color=never",
    ]);
    checkInstalled(config.packages, installed);
    if (run("opam", ["--version"]) !== config.opam)
      throw new Error(
        "opam changed: update and qualify the toolchain configuration",
      );
    if (run("opam", ["exec", "--", "ocamlc", "-version"]) !== config.compiler)
      throw new Error("Wrong OCaml compiler");
    fs.writeFileSync(".ci-evidence/opam-packages.txt", installed + "\n");
    const pins = run("opam", ["pin", "list", "--color=never"]);
    for (const url of Object.values(config.sourcePins))
      if (!pins.includes(url))
        throw new Error(
          "Installed source pin differs from qualified configuration",
        );
    fs.writeFileSync(".ci-evidence/opam-pins.txt", pins + "\n");
    const pinNames = run("opam", ["pin", "list", "--short", "--color=never"])
      .split(/\s+/).filter(Boolean);
    for (const name of overlayPackages)
      if (pinNames.some((pin) => pin === name || pin.startsWith(`${name}.`)))
        throw new Error(`Eio overlay must not be overridden by a source pin: ${name}`);
    const repositories = run("opam", ["repository", "list", "--short", "--color=never"])
      .split(/\s+/).filter(Boolean);
    if (repositories[0] !== config.eioOverlay.name)
      throw new Error("Reviewed Eio overlay must be the first selected repository");
    const repositoryListing = run("opam", ["repository", "list", "--all", "--safe", "--color=never"]);
    const repositoryUrl = checkOverlayRepository(config.eioOverlay.name,
      path.resolve(config.eioOverlay.path), repositoryListing);
    const prefix = run("opam", ["var", "prefix"]);
    const metadata = [];
    for (const name of overlayPackages) {
      const packageId = `${name}.${eioOverlay.version}`;
      const expectedPath = path.join(overlayRoot, "packages", name, packageId, "opam");
      const installedPath = path.join(prefix, ".opam-switch", "packages", packageId, "opam");
      // Public opam parser canonicalizes the full definition; never compare
      // formatting, a version label, or only a custom provenance marker.
      const canonical = (file) => run("opam", ["show", "--safe", "--just-file", "--raw",
        "--normalise", "--sort", file]);
      const expected = canonical(path.resolve(expectedPath));
      const installedMetadata = canonical(installedPath);
      checkOverlayMetadata(name, expected, installedMetadata);
      metadata.push({ package: packageId,
        sha256: createHash("sha256").update(installedMetadata).digest("hex"),
        definition: installedMetadata });
    }
    fs.writeFileSync(".ci-evidence/eio-overlay-provenance.json",
      JSON.stringify({ ...eioOverlay, repository: config.eioOverlay.name, repositoryUrl,
        source: "https://github.com/ocaml-multicore/eio/releases/download/v1.3/eio-1.3.tbz",
        sourceSha256: "8ed5c13e6689f31c85dca5f12762d84b8cc0042a7b07d3e464df6eb4b72b3dfc",
        metadata }, null, 2) + "\n");
    fs.writeFileSync(
      ".ci-evidence/toolchain.json",
      JSON.stringify(
        {
          result: "pass",
          compiler: config.compiler,
          opam: config.opam,
          dependencyCacheHit: process.env.DEPENDENCY_CACHE_HIT === "true",
          packages: config.packages,
          repository: config.repository,
          eioOverlay,
        },
        null,
        2,
      ) + "\n",
    );
  } else throw new Error("Expected key, overlay or verify");
}
if (process.argv[1] === fileURLToPath(import.meta.url)) main();
