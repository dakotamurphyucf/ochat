import test from "node:test";
import assert from "node:assert/strict";
import {
  createPlan,
  validatePlan,
  qualify,
} from "../scripts/framework-shards.mjs";

function fixture() {
  const inventory = Array.from({ length: 17 }, (_, i) => ({
    id: `test/suite-${i}`,
    alias: `@@test/suite-${i}/runtest`,
  }));
  const input = {
    revision: "a".repeat(40),
    sourceHash: "b".repeat(64),
    runId: "123",
    inventory,
  };
  const plan = createPlan(input);
  const reports = plan.shards.map(({ index, aliases }) => ({
    version: 1,
    revision: input.revision,
    sourceHash: input.sourceHash,
    runId: input.runId,
    index,
    aliases: [...aliases],
    result: "pass",
    durationSeconds: index,
  }));
  return { input, plan, reports };
}

test("LPT partition is deterministic, complete, nonempty and independent of inventory order", () => {
  const { input, plan, reports } = fixture();
  assert.deepEqual(
    createPlan({ ...input, inventory: [...input.inventory].reverse() }),
    plan,
  );
  assert.deepEqual(qualify(plan, reports.reverse()), {
    shardCount: 8,
    aliasCount: 17,
    durationSeconds: 36,
  });
  const weighted = createPlan({
    ...input,
    weights: { [input.inventory[0].id]: 100, ignored: NaN },
  });
  assert.deepEqual(weighted.shards[0].aliases, [input.inventory[0].alias]);
  const zero = createPlan({
    ...input,
    weights: Object.fromEntries(input.inventory.map(({ id }) => [id, 0])),
  });
  assert.ok(zero.shards.every(({ aliases }) => aliases.length > 0));
  assert.deepEqual(
    plan.inventory,
    input.inventory.slice().sort((a, b) => (a.id < b.id ? -1 : 1)),
  );
});

test("plan rejects stale formats, unsafe identities and incomplete or overlapping partitions", () => {
  const mutations = [
    (p) => (p.version = 2),
    (p) => (p.revision = "bad"),
    (p) => (p.sourceHash = "bad"),
    (p) => (p.runId = ""),
    (p) => (p.shardCount = 7),
    (p) => (p.extra = true),
    (p) => p.inventory.pop(),
    (p) => p.inventory.push(p.inventory[0]),
    (p) => (p.inventory[0].id = "../escape"),
    (p) => (p.inventory[0].alias = "@test/runtest"),
    (p) => (p.inventory[0].alias = "@@test/../escape/runtest"),
    (p) => p.shards.pop(),
    (p) => (p.shards[0].index = 0),
    (p) => (p.shards[0].index = p.shards[1].index),
    (p) => (p.shards[0].aliases = []),
    (p) => p.shards[0].aliases.pop(),
    (p) => p.shards[0].aliases.push(p.shards[1].aliases[0]),
    (p) => p.shards[0].aliases.push("@@unknown/runtest"),
  ];
  for (const mutate of mutations) {
    const { plan } = fixture();
    mutate(plan);
    assert.throws(() => validatePlan(plan));
  }
  const { input } = fixture();
  for (const weight of [-1, NaN, Infinity, "1"])
    assert.throws(() =>
      createPlan({ ...input, weights: { [input.inventory[0].id]: weight } }),
    );
  assert.throws(() =>
    createPlan({ ...input, inventory: input.inventory.slice(0, 7) }),
  );
});

test("qualification rejects missing, duplicate, failed, stale and partial execution evidence", () => {
  const mutations = [
    (r) => r.pop(),
    (r) => r.push(r[0]),
    (r) => (r[0] = r[1]),
    (r) => (r[0].version = 2),
    (r) => (r[0].revision = "c".repeat(40)),
    (r) => (r[0].sourceHash = "c".repeat(64)),
    (r) => (r[0].runId = "124"),
    (r) => (r[0].index = 9),
    (r) => (r[0].result = "fail"),
    (r) => r[0].aliases.pop(),
    (r) => r[0].aliases.push(r[0].aliases[0]),
    (r) => (r[0].aliases[0] = r[1].aliases[0]),
    (r) => (r[0].durationSeconds = -1),
    (r) => (r[0].durationSeconds = Infinity),
    (r) => (r[0].durationSeconds = "1"),
    (r) => delete r[0].result,
    (r) => (r[0].unknownField = true),
  ];
  for (const mutate of mutations) {
    const { plan, reports } = fixture();
    mutate(reports);
    assert.throws(() => qualify(plan, reports));
  }
});

test("same-run partial reruns may retain successful earlier attempts", () => {
  const { plan, reports } = fixture();
  reports.forEach(
    (report, index) => (report.runAttempt = index === 0 ? "2" : "1"),
  );
  assert.equal(qualify(plan, reports).aliasCount, 17);
  reports[0].runId = "previous-run";
  assert.throws(() => qualify(plan, reports));
});
