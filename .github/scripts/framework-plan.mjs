import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { discoverInventory } from "./framework-inventory.mjs";
import { createPlan, validatePlan } from "./framework-shards.mjs";

export const planPath = ".ci-evidence/framework-plan.json";

/** Bind evidence to tracked source bytes, including workflow and test selection. */
export function repositoryIdentity(root = ".") {
  const git = (...args) =>
    execFileSync("git", args, { cwd: root, encoding: "utf8" });
  const hash = createHash("sha256");
  for (const name of git("ls-files", "-z").split("\0").filter(Boolean).sort()) {
    const target = path.join(root, name);
    const stat = fs.lstatSync(target);
    const bytes = stat.isSymbolicLink()
      ? Buffer.from(fs.readlinkSync(target))
      : fs.readFileSync(target);
    hash.update(
      JSON.stringify([
        name,
        stat.isSymbolicLink() ? "link" : "file",
        bytes.length,
      ]),
    );
    hash.update(bytes);
  }
  return {
    revision: git("rev-parse", "HEAD").trim(),
    sourceHash: hash.digest("hex"),
  };
}

export function currentPlan(
  root = ".",
  runId = process.env.GITHUB_RUN_ID ?? "local",
) {
  const weights = JSON.parse(
    fs.readFileSync(
      path.join(root, ".github/framework-test-weights.json"),
      "utf8",
    ),
  );
  assert.equal(weights.version, 1, "Unsupported test timing format");
  return createPlan({
    ...repositoryIdentity(root),
    runId,
    inventory: discoverInventory(root),
    weights: weights.weights,
  });
}

/** Regenerate the authoritative plan; artifacts cannot change the selected tests. */
export function loadCurrentPlan(root = ".") {
  const plan = validatePlan(
    JSON.parse(fs.readFileSync(path.join(root, planPath), "utf8")),
  );
  assert.deepEqual(
    plan,
    currentPlan(root),
    "Framework plan differs from this run's tracked source",
  );
  return plan;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  if (process.argv[2] === "inspect") {
    const shard = loadCurrentPlan().shards.find(
      ({ index }) => index === Number(process.argv[3]),
    );
    assert.ok(shard, "Invalid framework shard");
    console.log(`Shard ${shard.index}: ${shard.aliases.length} aliases`);
  } else {
    assert.equal(process.argv.length, 2, "Unexpected framework plan arguments");
    const plan = currentPlan();
    fs.mkdirSync(path.dirname(planPath), { recursive: true });
    fs.writeFileSync(planPath, JSON.stringify(plan, null, 2) + "\n");
    console.log(
      `Planned ${plan.inventory.length} nonrecursive aliases across ${plan.shardCount} shards.`,
    );
  }
}
