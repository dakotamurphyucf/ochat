import fs from "node:fs";
import path from "node:path";
import { loadCurrentPlan } from "./framework-plan.mjs";
import { qualify } from "./framework-shards.mjs";

const root = ".ci-evidence/normal-shards";
const reports = [];
for (const artifact of fs.readdirSync(root, { withFileTypes: true })) {
  if (!artifact.isDirectory())
    throw new Error("Unexpected normal shard artifact");
  const report = path.join(root, artifact.name, "normal-shard.json");
  reports.push(JSON.parse(fs.readFileSync(report, "utf8")));
}
const result = qualify(loadCurrentPlan(), reports);
console.log(JSON.stringify(result));
if (process.env.GITHUB_STEP_SUMMARY)
  fs.appendFileSync(
    process.env.GITHUB_STEP_SUMMARY,
    `Normal framework coverage: **${result.shardCount} shards passed**, ${result.aliasCount} aliases assigned exactly once.\n`,
  );
