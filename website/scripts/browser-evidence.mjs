import assert from 'node:assert/strict';

export const browserShardCount = 2;
const projects = ['chromium', 'firefox', 'webkit'];

function checkIdentity(report, build, runId) {
  assert.equal(report.version, 1, 'Unsupported browser evidence');
  assert.equal(
    report.environment,
    build.environment,
    'Browser environment mismatch',
  );
  assert.equal(report.revision, build.revision, 'Browser revision mismatch');
  assert.equal(
    report.artifactSha256,
    build.sha256,
    'Browser artifact mismatch',
  );
  assert.equal(report.runId, runId, 'Browser workflow run mismatch');
  assert.equal(report.status, 'passed', 'Browser execution did not pass');
  assert.deepEqual(report.errors, [], 'Browser runner reported errors');
  assert.ok(report.tests.length > 0, 'Empty browser test evidence');
}

export function qualifyBrowserShards(plan, shards, build, runId) {
  checkIdentity(plan, build, runId);
  assert.equal(plan.shard, null, 'Expected an unsharded discovery plan');
  assert.deepEqual(
    [...new Set(plan.tests.map((t) => t.project))].sort(),
    projects,
  );
  const expected = new Map(plan.tests.map((t) => [t.id, t.project]));
  assert.equal(expected.size, plan.tests.length, 'Duplicate planned test');
  assert.equal(
    shards.length,
    browserShardCount,
    'Missing or extra browser shard',
  );
  const seenShards = new Set();
  const seenTests = new Set();
  let passed = 0,
    skipped = 0;
  for (const shard of shards) {
    checkIdentity(shard, build, runId);
    assert.equal(shard.shard?.total, browserShardCount, 'Wrong shard total');
    const index = shard.shard.current;
    assert.ok(
      Number.isInteger(index) && index >= 1 && index <= browserShardCount,
      'Invalid shard index',
    );
    assert.ok(!seenShards.has(index), 'Duplicate browser shard');
    seenShards.add(index);
    for (const test of shard.tests) {
      assert.ok(expected.has(test.id), 'Unexpected browser test');
      assert.equal(
        test.project,
        expected.get(test.id),
        'Browser project mismatch',
      );
      assert.ok(
        !seenTests.has(test.id),
        'Duplicate browser test across shards',
      );
      seenTests.add(test.id);
      assert.equal(test.results.length, 1, 'Test missing execution or retried');
      assert.equal(
        test.results[0].retry,
        0,
        'Browser retries must stay disabled',
      );
      if (test.expectedStatus === 'skipped') {
        assert.equal(test.results[0].status, 'skipped');
        assert.equal(test.outcome, 'skipped');
        skipped++;
      } else {
        assert.equal(test.expectedStatus, 'passed');
        assert.equal(
          test.results[0].status,
          'passed',
          'Browser test did not pass',
        );
        assert.equal(test.outcome, 'expected');
        passed++;
      }
    }
  }
  assert.equal(seenTests.size, expected.size, 'Missing browser tests');
  assert.ok(passed > 0, 'No browser tests passed');
  return { total: expected.size, passed, skipped };
}

export function checkBrowserQualification(
  browser,
  build,
  runId = browser.runId,
) {
  assert.equal(
    browser.version,
    2,
    'Complete sharded browser qualification required',
  );
  assert.equal(browser.status, 'passed');
  assert.deepEqual(browser.failedTests, []);
  assert.equal(browser.runId, runId);
  const counts = qualifyBrowserShards(
    browser.plan,
    browser.shards,
    build,
    runId,
  );
  assert.deepEqual(
    browser.counts,
    counts,
    'Browser summary disagrees with shard evidence',
  );
  return counts;
}
