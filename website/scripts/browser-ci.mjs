import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { inventory, verifyArtifact } from './release-artifact.mjs';
import {
  browserShardCount,
  qualifyBrowserShards,
} from './browser-evidence.mjs';

const site = fileURLToPath(new URL('../', import.meta.url));
const read = async (file) => JSON.parse(await fs.readFile(file, 'utf8'));
const [command, target, shardIndex] = process.argv.slice(2);
assert.ok(
  ['plan', 'run', 'merge'].includes(command) && target,
  'Usage: browser-ci.mjs plan|run|merge ARTIFACT [SHARD]',
);
const directory = path.resolve(target);
const artifact = await verifyArtifact(directory);
const revision = execFileSync('git', ['rev-parse', 'HEAD'], {
  cwd: site,
  encoding: 'utf8',
}).trim();
assert.equal(
  artifact.build.revision,
  revision,
  'Browser checkout differs from build',
);
assert.equal(
  artifact.build.environment,
  process.env.SITE_ENV,
  'Browser environment differs from build',
);
assert.equal(
  artifact.packageLockSha256,
  createHash('sha256')
    .update(await fs.readFile(path.join(site, 'package-lock.json')))
    .digest('hex'),
);
const output = path.join(site, '.release/browser');
await fs.mkdir(output, { recursive: true });
const runId = process.env.GITHUB_RUN_ID || 'local';
const env = {
  ...process.env,
  CI_BROWSER_ARTIFACT: path.join(directory, 'artifact.json'),
  CI_BROWSER_REPORT: path.join(directory, 'browser-plan.json'),
};
const playwright = (args, extraEnv = {}) =>
  spawnSync(
    process.execPath,
    [path.join(site, 'node_modules/@playwright/test/cli.js'), ...args],
    {
      cwd: site,
      env: { ...env, ...extraEnv },
      stdio: 'inherit',
    },
  );
const succeeded = (result) =>
  assert.equal(
    result.status,
    0,
    `Playwright failed (${result.error || result.signal || result.status})`,
  );
if (command === 'plan') {
  succeeded(
    playwright(['test', '--list', '--reporter=./scripts/browser-reporter.mjs']),
  );
} else if (command === 'run') {
  const index = Number(shardIndex);
  assert.ok(
    Number.isInteger(index) && index >= 1 && index <= browserShardCount,
    'Invalid browser shard',
  );
  // Restore the built bytes and fixture reports, without invoking the generator
  // or rebuilding on any shard. Verify the served copy before and after testing.
  await fs.rm(path.join(site, 'dist'), { recursive: true, force: true });
  await fs.rm(path.join(site, '.generated'), { recursive: true, force: true });
  await fs.cp(path.join(directory, 'dist'), path.join(site, 'dist'), {
    recursive: true,
  });
  await fs.cp(path.join(directory, 'evidence'), path.join(site, '.generated'), {
    recursive: true,
  });
  const verifyServed = async () =>
    assert.equal(
      (await inventory(path.join(site, 'dist'))).sha256,
      artifact.build.sha256,
      'Served browser artifact changed',
    );
  await verifyServed();
  const result = playwright(
    [
      'test',
      `--shard=${index}/${browserShardCount}`,
      '--workers=2',
      '--reporter=list,blob,./scripts/browser-reporter.mjs',
      `--output=${path.join(output, 'test-results')}`,
    ],
    {
      CI_BROWSER_REPORT: path.join(output, 'shard.json'),
      PLAYWRIGHT_BLOB_OUTPUT_DIR: path.join(output, 'blobs'),
    },
  );
  await verifyServed();
  succeeded(result);
} else {
  const input = path.join(site, '.release/shards');
  assert.deepEqual(
    (await fs.readdir(input)).sort(),
    [1, 2].map(
      (index) => `website-browser-${artifact.build.environment}-${index}`,
    ),
    'Missing or extra shard artifacts',
  );
  const plan = await read(path.join(directory, 'browser-plan.json'));
  const blobs = path.join(output, 'blobs');
  await fs.mkdir(blobs, { recursive: true });
  const shards = [];
  for (let index = 1; index <= browserShardCount; index++) {
    const source = path.join(
      input,
      `website-browser-${artifact.build.environment}-${index}`,
    );
    shards.push(await read(path.join(source, 'shard.json')));
    const files = (await fs.readdir(path.join(source, 'blobs'))).filter(
      (name) => name.endsWith('.zip'),
    );
    assert.equal(files.length, 1, 'Expected one blob report per shard');
    await fs.copyFile(
      path.join(source, 'blobs', files[0]),
      path.join(blobs, `shard-${index}.zip`),
    );
  }
  // Retain a human-readable merged report even when execution evidence fails.
  succeeded(
    playwright(['merge-reports', '--reporter=html,json', blobs], {
      PLAYWRIGHT_HTML_OUTPUT_DIR: path.join(output, 'html'),
      PLAYWRIGHT_HTML_OPEN: 'never',
      PLAYWRIGHT_JSON_OUTPUT_NAME: path.join(output, 'merged.json'),
    }),
  );
  const counts = qualifyBrowserShards(plan, shards, artifact.build, runId);
  const merged = await read(path.join(output, 'merged.json'));
  assert.deepEqual(merged.errors, []);
  assert.equal(merged.stats.unexpected, 0);
  assert.equal(merged.stats.flaky, 0);
  assert.equal(merged.stats.expected, counts.passed);
  assert.equal(merged.stats.skipped, counts.skipped);
  const result = {
    version: 2,
    status: 'passed',
    failedTests: [],
    runId,
    plan,
    shards,
    counts,
  };
  await fs.writeFile(
    path.join(directory, 'browser-result.json'),
    JSON.stringify(result, null, 2) + '\n',
  );
  console.log(
    JSON.stringify({
      environment: artifact.build.environment,
      ...counts,
      result: 'pass',
    }),
  );
}
