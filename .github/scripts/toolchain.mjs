import fs from "node:fs";
import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";

export const keyInputs = [
  "ochat.opam",
  "ochat.opam.locked",
  "dune-project",
  ".github/ci-toolchain.json",
  ".github/actions/setup-ochat/action.yml",
  ".github/scripts/toolchain.mjs",
];
export function cacheKey(fingerprint, read = (name) => fs.readFileSync(name)) {
  const hash = createHash("sha256").update(JSON.stringify(fingerprint));
  for (const name of keyInputs)
    hash.update(name).update("\0").update(read(name)).update("\0");
  return `ochat-opam-v1-${hash.digest("hex")}`;
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
  fs.mkdirSync(".ci-evidence", { recursive: true });
  if (mode === "key") {
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
    if (
      fingerprint.platform !== "linux" ||
      fingerprint.image !== "ubuntu24" ||
      !fingerprint.imageVersion
    )
      throw new Error(
        "The dependency cache is qualified for GitHub ubuntu-24.04 runners only",
      );
    const key = cacheKey(fingerprint);
    fs.appendFileSync(
      process.env.GITHUB_OUTPUT,
      `key=${key}\nrepository=${config.repository}\n`,
    );
    fs.writeFileSync(
      ".ci-evidence/toolchain-key.json",
      JSON.stringify({ key, fingerprint }, null, 2) + "\n",
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
        },
        null,
        2,
      ) + "\n",
    );
  } else throw new Error("Expected key or verify");
}
if (process.argv[1] === fileURLToPath(import.meta.url)) main();
