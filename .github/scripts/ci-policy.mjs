// Conservative dependency policy. Unknown inputs always request full validation.
export function classifyPaths(paths) {
  const result = {
    framework: false,
    semantics: false,
    website: false,
    reasons: [],
  };
  for (const path of paths) {
    if (
      typeof path !== "string" ||
      !path ||
      path.startsWith("/") ||
      path.split("/").includes("..")
    ) {
      return {
        framework: true,
        semantics: true,
        website: true,
        reasons: ["Invalid changed path; full validation"],
      };
    }
    if (
      /^website\/planning\/[^/]+\.md$/.test(path) ||
      ["website/README.md", "website/CONTRIBUTING.md"].includes(path)
    ) {
      result.reasons.push(`${path}: maintainer documentation only`);
    } else if (path.startsWith("website/")) {
      // Production artifacts require same-revision semantic evidence, even for UI-only changes.
      result.website = result.semantics = true;
      result.reasons.push(
        `${path}: website qualification and semantic evidence`,
      );
    } else {
      // Runtime, tests, docs/examples, dependency definitions, CI, assets, and
      // every unrecognized path are deliberately included. Keep this default.
      result.framework = result.semantics = result.website = true;
      result.reasons.push(`${path}: shared or unknown input; full validation`);
    }
  }
  return result;
}

export function chooseChecks({
  event,
  ref,
  paths = [],
  pendingPaths = [],
  fallback,
  mode = "validate",
}) {
  if (!["validate", "redeploy", "cold"].includes(mode))
    throw new Error("Unknown dispatch mode");
  const current = classifyPaths(paths);
  const pending = classifyPaths(pendingPaths);
  const full =
    Boolean(fallback) || ["workflow_dispatch", "schedule"].includes(event);
  const selection = {
    framework: full || current.framework || pending.framework,
    semantics: full || current.semantics || pending.semantics,
    website: full || current.website || pending.website,
    deploy: false,
    cold:
      event === "schedule" ||
      (event === "workflow_dispatch" && mode === "cold"),
    reasons: [
      ...current.reasons,
      ...pending.reasons.map((reason) => `Since last deployment: ${reason}`),
    ],
  };
  if (fallback) selection.reasons.push(`Conservative fallback: ${fallback}`);
  if (full) selection.reasons.push("Full validation requested");
  selection.deploy =
    ref === "refs/heads/main" &&
    selection.website &&
    (event === "push" ||
      (event === "workflow_dispatch" && mode === "redeploy"));
  return selection;
}

export function requireSelectedJobs(needs) {
  if (needs.changes?.result !== "success")
    throw new Error("Change detection did not succeed");
  const decisions = [];
  for (const name of ["semantics", "framework", "website"]) {
    const selected = needs.changes.outputs?.[name];
    if (!["true", "false"].includes(selected))
      throw new Error(`Missing or invalid selection for ${name}`);
    const expected = selected === "true" ? "success" : "skipped";
    if (needs[name]?.result !== expected)
      throw new Error(
        `${name}: expected ${expected}, got ${needs[name]?.result}`,
      );
    decisions.push(`${name}: ${expected}`);
  }
  return decisions;
}
