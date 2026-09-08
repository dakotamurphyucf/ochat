import fs from "node:fs";
import { requireSelectedJobs } from "./ci-policy.mjs";
const decisions = requireSelectedJobs(JSON.parse(process.env.NEEDS_JSON));
console.log(decisions.join("\n"));
if (process.env.GITHUB_STEP_SUMMARY)
  fs.appendFileSync(
    process.env.GITHUB_STEP_SUMMARY,
    "## Release gate\n\n" +
      decisions.map((row) => `- ${row}`).join("\n") +
      "\n",
  );
