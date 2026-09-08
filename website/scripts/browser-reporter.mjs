import fs from 'node:fs';

// Used for both discovery (--list) and execution. IDs come from Playwright,
// so qualification can detect omitted or duplicated tests across machines.
export default class BrowserReporter {
  errors = [];
  onBegin(config, suite) {
    this.config = config;
    this.suite = suite;
  }
  onError(error) {
    this.errors.push(error.message || String(error));
  }
  onEnd(result) {
    const artifact = JSON.parse(
      fs.readFileSync(process.env.CI_BROWSER_ARTIFACT, 'utf8'),
    );
    fs.writeFileSync(
      process.env.CI_BROWSER_REPORT,
      JSON.stringify(
        {
          version: 1,
          environment: artifact.build.environment,
          revision: artifact.build.revision,
          artifactSha256: artifact.build.sha256,
          runId: process.env.GITHUB_RUN_ID || 'local',
          runAttempt: process.env.GITHUB_RUN_ATTEMPT || 'local',
          shard: this.config.shard,
          status: result.status,
          errors: this.errors,
          durationMs: result.duration,
          tests: this.suite.allTests().map((test) => ({
            id: test.id,
            project: test.parent.project().name,
            title: test.titlePath().join(' > '),
            expectedStatus: test.expectedStatus,
            outcome: test.outcome(),
            results: test.results.map((result) => ({
              status: result.status,
              retry: result.retry,
            })),
          })),
        },
        null,
        2,
      ) + '\n',
    );
  }
}
