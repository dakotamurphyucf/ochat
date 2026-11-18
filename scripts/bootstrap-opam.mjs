#!/usr/bin/env node
import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { checkOverlay, checkOverlayInventory, overlayInventory } from "../.github/scripts/toolchain.mjs";

export function bootstrapEnvironment(inherited) {
  const environment = {};
  const libraryVariables = new Set(["OCAMLPATH", "OCAMLLIB", "OCAML_TOPLEVEL_PATH",
    "OCAMLTOP_INCLUDE_PATH", "CAML_LD_LIBRARY_PATH", "CAML_LIB"]);
  for (const [name, value] of Object.entries(inherited))
    if (!name.startsWith("OPAM") && !libraryVariables.has(name)) environment[name] = value;
  const oldPrefixes = [inherited.OPAM_SWITCH_PREFIX];
  if (inherited.OPAMROOT && inherited.OPAMSWITCH && !path.isAbsolute(inherited.OPAMSWITCH))
    oldPrefixes.push(path.join(inherited.OPAMROOT, inherited.OPAMSWITCH));
  environment.PATH = (inherited.PATH ?? "").split(path.delimiter).filter((entry) => {
    if (!entry) return false;
    if (oldPrefixes.some((prefix) => prefix &&
        (entry === prefix || entry.startsWith(`${prefix}${path.sep}`))))
      return false;
    return !/(?:^|\/)_(?:opam)(?:\/|$)|(?:^|\/)\.opam\/[^/]+\/bin(?:\/|$)/.test(entry);
  }).join(path.delimiter);
  return environment;
}

export function bootstrapCommands(config, taskDirectory, checkout) {
  const root = path.join(taskDirectory, "root");
  const selectedSwitch = path.join(taskDirectory, "switch");
  return [
    ["init", `--root=${root}`, "--bare", "--no-setup", "--yes", "default", config.repository],
    ["switch", "create", `--root=${root}`, selectedSwitch,
      `ocaml-base-compiler.${config.compiler}`, "--yes"],
    ["repository", "add", `--root=${root}`, `--switch=${selectedSwitch}`,
      config.eioOverlay.name, path.join(checkout, config.eioOverlay.path),
      "--this-switch", "--rank=1", "--yes"],
  ];
}

function main() {
  const checkout = fileURLToPath(new URL("../", import.meta.url));
  const config = JSON.parse(fs.readFileSync(path.join(checkout, ".github/ci-toolchain.json"), "utf8"));
  const requested = process.argv[2];
  if (!requested || process.argv.length > 4)
    throw new Error("Usage: node scripts/bootstrap-opam.mjs NEW_DIRECTORY [OPAM_BINARY]");
  const taskDirectory = path.resolve(requested);
  if (fs.existsSync(taskDirectory)) throw new Error("Bootstrap destination must not exist");
  // lstat catches a dangling symlink too; mkdir below is exclusive after preflight.
  try { fs.lstatSync(taskDirectory); throw new Error("Bootstrap destination must not exist"); }
  catch (error) { if (error.code !== "ENOENT") throw error; }
  checkOverlayInventory(overlayInventory(checkout));
  checkOverlay(config, (name) => fs.readFileSync(path.join(checkout, name)));
  const environment = bootstrapEnvironment(process.env);
  const requestedOpam = process.argv[3] ?? "opam";
  const opam = requestedOpam.includes(path.sep) ? path.resolve(requestedOpam) : requestedOpam;
  const version = execFileSync(opam, ["--version"], { env: environment, encoding: "utf8" }).trim();
  if (version !== config.opam)
    throw new Error(`Expected opam ${config.opam}, found ${version}; use a task-local binary or update PATH`);
  fs.mkdirSync(path.dirname(taskDirectory), { recursive: true });
  fs.mkdirSync(taskDirectory);
  for (const args of bootstrapCommands(config, taskDirectory, checkout))
    execFileSync(opam, args, { env: environment, cwd: checkout, stdio: "inherit" });
  const root = path.join(taskDirectory, "root");
  const selectedSwitch = path.join(taskDirectory, "switch");
  fs.writeFileSync(path.join(taskDirectory, "environment.json"),
    JSON.stringify({ root, switch: selectedSwitch, compiler: config.compiler,
      opam: config.opam, eioOverlay: config.eioOverlay.sha256 }, null, 2) + "\n");
  const quote = (value) => "'" + value.replaceAll("'", "'\\''") + "'";
  console.log(`Compiler environment created. Project dependencies are not installed.\n` +
    `Load only this terminal's environment with:\n` +
    `eval "$( ${quote(opam)} env --root=${quote(root)} --switch=${quote(selectedSwitch)} --set-root --set-switch )"`);
}
if (process.argv[1] === fileURLToPath(import.meta.url)) main();
