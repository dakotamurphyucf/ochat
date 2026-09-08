import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { browserFixture } from './fixtures/browser-evidence.mjs';
import {
  checkBrowserQualification,
  qualifyBrowserShards,
} from '../scripts/browser-evidence.mjs';

const build = {
  environment: 'production',
  revision: 'a'.repeat(40),
  sha256: 'b'.repeat(64),
};

test('browser qualification rejects missing, duplicate, stale, failed, cancelled, empty and partially executed shards', () => {
  const good = browserFixture(build, '123');
  assert.deepEqual(checkBrowserQualification(good, build, '123'), {
    total: 6,
    passed: 6,
    skipped: 0,
  });
  for (const mutate of [
    (b) => b.shards.pop(),
    (b) => b.shards.push(b.shards[0]),
    (b) => (b.shards[1].shard.current = 1),
    (b) => (b.shards[1].shard.current = 3),
    (b) => (b.shards[1].shard.total = 3),
    (b) => (b.shards[1].environment = 'preview'),
    (b) => (b.shards[1].revision = 'c'.repeat(40)),
    (b) => (b.shards[1].artifactSha256 = 'c'.repeat(64)),
    (b) => (b.shards[1].runId = '122'),
    (b) => (b.shards[1].status = 'failed'),
    (b) => (b.shards[1].status = 'interrupted'),
    (b) => b.shards[1].errors.push('runner error'),
    (b) => (b.shards[1].tests = []),
    (b) => b.shards[1].tests.pop(),
    (b) => b.shards[1].tests.push(b.shards[0].tests[0]),
    (b) => (b.shards[1].tests[0].id = 'unknown'),
    (b) => (b.shards[1].tests[0].project = 'unknown'),
    (b) => (b.shards[1].tests[0].results = []),
    (b) => (b.shards[1].tests[0].results[0].retry = 1),
    (b) => (b.shards[1].tests[0].results[0].status = 'timedOut'),
    (b) => b.plan.tests.push(b.plan.tests[0]),
    (b) => b.plan.tests.forEach((t) => (t.project = 'chromium')),
    (b) => (b.plan.shard = { current: 1, total: 2 }),
    (b) => (b.plan.status = 'failed'),
    (b) => (b.counts.total = 5),
  ]) {
    const changed = structuredClone(good);
    mutate(changed);
    assert.throws(() => checkBrowserQualification(changed, build, '123'));
  }
  assert.throws(() => checkBrowserQualification(good, build, 'other-run'));
});

test('real Playwright discovery, two shard reporters and blob merging preserve complete test identities and explicit skips', async () => {
  const directory = await fs.mkdtemp(
    path.join(os.tmpdir(), 'ochat-browser-shards-'),
  );
  const reporter = fileURLToPath(
    new URL('../scripts/browser-reporter.mjs', import.meta.url),
  );
  const cli = fileURLToPath(
    new URL('../node_modules/@playwright/test/cli.js', import.meta.url),
  );
  const playwrightImport = new URL(
    '../node_modules/@playwright/test/index.mjs',
    import.meta.url,
  ).href;
  const read = async (file) =>
    JSON.parse(await fs.readFile(path.join(directory, file), 'utf8'));
  try {
    await fs.writeFile(
      path.join(directory, 'artifact.json'),
      JSON.stringify({ build }),
    );
    await fs.writeFile(
      path.join(directory, 'fixture.spec.mjs'),
      `import { test } from ${JSON.stringify(playwrightImport)};
      test('first', () => {});
      test('second', () => {});
      test('explicit skip', () => { test.skip(true, 'Existing platform exception'); });`,
    );
    await fs.writeFile(
      path.join(directory, 'config.mjs'),
      `export default {
      testDir: ${JSON.stringify(directory)}, testMatch: 'fixture.spec.mjs', fullyParallel: true, retries: 0,
      projects: [{name:'chromium'}, {name:'firefox'}, {name:'webkit'}]
    };`,
    );
    const run = (args, extra = {}) => {
      const result = spawnSync(process.execPath, [cli, ...args], {
        cwd: directory,
        encoding: 'utf8',
        env: {
          ...process.env,
          GITHUB_RUN_ID: '123',
          CI_BROWSER_ARTIFACT: path.join(directory, 'artifact.json'),
          ...extra,
        },
      });
      assert.equal(result.status, 0, result.stdout + result.stderr);
    };
    run(['test', '-c', 'config.mjs', '--list', `--reporter=${reporter}`], {
      CI_BROWSER_REPORT: path.join(directory, 'plan.json'),
    });
    const shards = [];
    await fs.mkdir(path.join(directory, 'blobs'));
    for (const index of [1, 2]) {
      run(
        [
          'test',
          '-c',
          'config.mjs',
          `--shard=${index}/2`,
          '--workers=2',
          `--reporter=blob,${reporter}`,
        ],
        {
          CI_BROWSER_REPORT: path.join(directory, `shard-${index}.json`),
          PLAYWRIGHT_BLOB_OUTPUT_DIR: path.join(directory, `blob-${index}`),
        },
      );
      shards.push(await read(`shard-${index}.json`));
      for (const file of await fs.readdir(
        path.join(directory, `blob-${index}`),
      ))
        await fs.copyFile(
          path.join(directory, `blob-${index}`, file),
          path.join(directory, 'blobs', file),
        );
    }
    assert.deepEqual(
      qualifyBrowserShards(await read('plan.json'), shards, build, '123'),
      { total: 9, passed: 6, skipped: 3 },
    );
    run(['merge-reports', '--reporter=json', 'blobs'], {
      PLAYWRIGHT_JSON_OUTPUT_NAME: path.join(directory, 'merged.json'),
    });
    const merged = await read('merged.json');
    assert.deepEqual(merged.errors, []);
    assert.equal(merged.stats.expected, 6);
    assert.equal(merged.stats.skipped, 3);
    assert.equal(merged.stats.unexpected, 0);
  } finally {
    await fs.rm(directory, { recursive: true, force: true });
  }
});
