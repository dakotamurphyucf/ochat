import assert from "node:assert/strict";

export const frameworkShardCount = 8;

/** @typedef {{id: string, alias: string}} InventoryEntry */
/** @typedef {{version: 1, revision: string, sourceHash: string, runId: string,
 * shardCount: 8, inventory: InventoryEntry[],
 * shards: {index: number, aliases: string[]}[]}} FrameworkPlan */
/** @typedef {{version: 1, revision: string, sourceHash: string, runId: string,
 * index: number, aliases: string[], result: 'pass'|'fail', durationSeconds: number,
 * runAttempt?: string}} FrameworkReport */

function keys(value, required, optional = []) {
  assert.ok(
    value && typeof value === "object" && !Array.isArray(value),
    "Expected an object",
  );
  assert.deepEqual(
    Object.keys(value)
      .filter((key) => !optional.includes(key))
      .sort(),
    [...required].sort(),
    "Unexpected or missing evidence fields",
  );
}

function identity(value) {
  assert.equal(value.version, 1, "Unsupported framework evidence version");
  assert.match(value.revision, /^[a-f0-9]{40}$/, "Invalid revision");
  assert.match(value.sourceHash, /^[a-f0-9]{64}$/, "Invalid source hash");
  assert.ok(
    typeof value.runId === "string" && value.runId.trim().length > 0,
    "Invalid workflow run ID",
  );
}

function safeDirectory(value) {
  return (
    value === "." ||
    (typeof value === "string" &&
      /^(?:[A-Za-z0-9_.-]+\/)*[A-Za-z0-9_.-]+$/.test(value) &&
      value.split("/").every((part) => part !== "." && part !== ".."))
  );
}

function inventoryEntries(inventory) {
  assert.ok(
    Array.isArray(inventory) && inventory.length >= frameworkShardCount,
    "Eight nonempty shards require at least eight aliases",
  );
  const ids = new Set(),
    aliases = new Set();
  for (const entry of inventory) {
    keys(entry, ["id", "alias"]);
    assert.ok(safeDirectory(entry.id), "Unsafe inventory ID");
    assert.ok(
      typeof entry.alias === "string" &&
        entry.alias.startsWith("@@") &&
        entry.alias.endsWith("/runtest") &&
        safeDirectory(entry.alias.slice(2, -8)),
      "Unsafe or recursive test alias",
    );
    assert.ok(!ids.has(entry.id), "Duplicate inventory ID");
    assert.ok(!aliases.has(entry.alias), "Duplicate inventory alias");
    ids.add(entry.id);
    aliases.add(entry.alias);
  }
  return aliases;
}

/** Validate the complete authoritative partition; never infer coverage from counts. */
export function validatePlan(plan) {
  keys(plan, [
    "version",
    "revision",
    "sourceHash",
    "runId",
    "shardCount",
    "inventory",
    "shards",
  ]);
  identity(plan);
  assert.equal(
    plan.shardCount,
    frameworkShardCount,
    "Wrong framework shard count",
  );
  const expected = inventoryEntries(plan.inventory);
  assert.ok(
    Array.isArray(plan.shards) && plan.shards.length === frameworkShardCount,
    "Missing or extra planned shards",
  );
  const indexes = new Set(),
    seen = new Set();
  for (const shard of plan.shards) {
    keys(shard, ["index", "aliases"]);
    assert.ok(
      Number.isInteger(shard.index) &&
        shard.index >= 1 &&
        shard.index <= frameworkShardCount,
      "Invalid shard index",
    );
    assert.ok(!indexes.has(shard.index), "Duplicate planned shard");
    indexes.add(shard.index);
    assert.ok(
      Array.isArray(shard.aliases) && shard.aliases.length > 0,
      "Empty planned shard",
    );
    for (const alias of shard.aliases) {
      assert.ok(expected.has(alias), "Unknown planned alias");
      assert.ok(!seen.has(alias), "Duplicate planned alias");
      seen.add(alias);
    }
  }
  assert.equal(seen.size, expected.size, "Missing planned aliases");
  return plan;
}

/** Deterministic longest-processing-time assignment; unknown weight IDs are ignored. */
export function createPlan({
  revision,
  sourceHash,
  runId,
  inventory,
  weights = {},
}) {
  inventoryEntries(inventory);
  assert.ok(
    weights && typeof weights === "object" && !Array.isArray(weights),
    "Invalid weights",
  );
  const entries = inventory
    .map((entry) => {
      const weight = Object.hasOwn(weights, entry.id) ? weights[entry.id] : 1;
      assert.ok(
        Number.isFinite(weight) && weight >= 0,
        "Invalid inventory weight",
      );
      return { ...entry, weight };
    })
    .sort(
      (a, b) => b.weight - a.weight || (a.id < b.id ? -1 : a.id > b.id ? 1 : 0),
    );
  const groups = Array.from({ length: frameworkShardCount }, (_, index) => ({
    index: index + 1,
    aliases: [],
    weight: 0,
  }));
  for (const entry of entries) {
    const group = [...groups].sort(
      (a, b) =>
        a.weight - b.weight ||
        a.aliases.length - b.aliases.length ||
        a.index - b.index,
    )[0];
    group.aliases.push(entry.alias);
    group.weight += entry.weight;
  }
  return validatePlan({
    version: 1,
    revision,
    sourceHash,
    runId,
    shardCount: frameworkShardCount,
    inventory: inventory
      .map((entry) => ({ ...entry }))
      .sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0)),
    shards: groups.map(({ index, aliases }) => ({
      index,
      aliases: aliases.sort(),
    })),
  });
}

/** Qualify fresh execution evidence from this run, including retained successful attempts. */
export function qualify(plan, reports) {
  validatePlan(plan);
  assert.ok(
    Array.isArray(reports) && reports.length === frameworkShardCount,
    "Missing or extra framework reports",
  );
  const seen = new Set();
  let durationSeconds = 0;
  for (const report of reports) {
    keys(
      report,
      [
        "version",
        "revision",
        "sourceHash",
        "runId",
        "index",
        "aliases",
        "result",
        "durationSeconds",
      ],
      ["runAttempt"],
    );
    identity(report);
    for (const field of ["revision", "sourceHash", "runId"])
      assert.equal(report[field], plan[field], `Framework ${field} mismatch`);
    const shard = plan.shards.find(
      (candidate) => candidate.index === report.index,
    );
    assert.ok(shard, "Unknown framework shard");
    assert.ok(!seen.has(report.index), "Duplicate framework report");
    seen.add(report.index);
    assert.ok(Array.isArray(report.aliases), "Invalid executed aliases");
    assert.deepEqual(
      [...report.aliases].sort(),
      [...shard.aliases].sort(),
      "Executed aliases differ from plan",
    );
    assert.equal(report.result, "pass", "Framework shard did not pass");
    assert.ok(
      Number.isFinite(report.durationSeconds) && report.durationSeconds >= 0,
      "Invalid execution duration",
    );
    durationSeconds += report.durationSeconds;
  }
  assert.ok(
    Number.isFinite(durationSeconds),
    "Invalid total execution duration",
  );
  return {
    shardCount: frameworkShardCount,
    aliasCount: plan.inventory.length,
    durationSeconds,
  };
}
