import { qualifyBrowserShards } from '../../scripts/browser-evidence.mjs';

export function browserFixture(
  build,
  runId = process.env.GITHUB_RUN_ID || 'local',
) {
  const identity = {
    version: 1,
    environment: build.environment,
    revision: build.revision,
    artifactSha256: build.sha256,
    runId,
    status: 'passed',
    errors: [],
  };
  const tests = ['chromium', 'firefox', 'webkit'].flatMap((project) =>
    [1, 2].map((index) => ({
      id: `${project}-${index}`,
      project,
      expectedStatus: 'passed',
      outcome: 'expected',
      results: [{ status: 'passed', retry: 0 }],
    })),
  );
  const plan = { ...identity, shard: null, tests: structuredClone(tests) };
  const shards = [1, 2].map((current) => ({
    ...identity,
    shard: { current, total: 2 },
    tests: structuredClone(
      tests.filter((_, index) => index % 2 === current - 1),
    ),
  }));
  return {
    version: 2,
    status: 'passed',
    failedTests: [],
    runId,
    plan,
    shards,
    counts: qualifyBrowserShards(plan, shards, build, runId),
  };
}
