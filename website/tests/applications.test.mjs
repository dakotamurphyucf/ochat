import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { applicationReport } from '../scripts/applications.mjs';
import { sourceFileId } from '../config/source-links.mjs';
const siteRoot = new URL('../', import.meta.url).pathname;
const read = async (name) =>
  JSON.parse(await fs.readFile(path.join(siteRoot, name), 'utf8'));
const entries = await read('config/docs-manifest.json');
const { examples } = await read('.generated/examples-report.json');
const source = 'docs-src/examples/applications/docs-review/recording.json';
const original = JSON.parse(
  await fs.readFile(path.join(siteRoot, '..', source), 'utf8'),
);

test('application publishing rejects stale or misrepresented recording evidence', async () => {
  const root = await fs.mkdtemp(
    path.join(os.tmpdir(), 'ochat-recording-check-'),
  );
  try {
    await fs.mkdir(path.dirname(path.join(root, source)), { recursive: true });
    for (const source of Object.keys(original.runtimeSources || {})) {
      await fs.mkdir(path.dirname(path.join(root, source)), {
        recursive: true,
      });
      await fs.copyFile(
        path.join(siteRoot, '..', source),
        path.join(root, source),
      );
    }
    const check = async (record) => {
      await fs.writeFile(path.join(root, source), JSON.stringify(record));
      return applicationReport({
        root,
        siteRoot,
        entries,
        examples,
        isProduction: false,
      });
    };
    const report = await check(original);
    assert.equal(report.applications.length, 6);
    assert.equal(report.recording.steps.length, 3);
    const stale = structuredClone(original);
    stale.sourceHashes['explorer.chatmd'] = '0'.repeat(64);
    await assert.rejects(check(stale), /Stale recording source/);
    const altered = structuredClone(original);
    altered.result = 'An invented replacement report';
    await assert.rejects(check(altered), /differs from captured response/);
    const mislabeled = structuredClone(original);
    mislabeled.liveProvider = !original.liveProvider;
    await assert.rejects(check(mislabeled), /provider label mismatch/);
    const missing = structuredClone(original);
    missing.requests[1].request.input =
      missing.requests[1].request.input.filter(
        (i) => i.type !== 'function_call_output',
      );
    await assert.rejects(check(missing), /actual file-tool output/);
    await fs.writeFile(path.join(root, source), JSON.stringify(original));
    await assert.rejects(
      applicationReport({
        root,
        siteRoot,
        entries,
        examples,
        isProduction: true,
        facts: { source: () => ({ sourceModified: true }) },
      }),
      /committed recording bytes/,
    );
  } finally {
    await fs.rm(root, { recursive: true, force: true });
  }
});

test('file permalinks are unique for punctuation, directories and each catalog instance', () => {
  const ids = examples.flatMap((e) =>
    e.files.map((f) => sourceFileId(e.id, f.path)),
  );
  assert.equal(ids.length, new Set(ids).size);
  assert.notEqual(
    sourceFileId('example', 'a/b.txt'),
    sourceFileId('example', 'a_2f_b.txt'),
  );
  assert.notEqual(
    sourceFileId('example', 'a/b.txt'),
    sourceFileId('example', 'a-b.txt'),
  );
});

test('every curriculum lesson has an outcome, setup and an explained next step', async () => {
  const lessons = await read('config/lesson-overviews.json');
  const curriculum = await read('config/tutorials.json');
  assert.deepEqual(
    lessons.map((l) => l.page),
    curriculum.map((t) => t.page),
  );
  for (const l of lessons)
    for (const key of ['outcome', 'requires', 'nextBenefit'])
      assert.ok(l[key].length > 25);
});
