import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import { buildSidebar, documentationPaths } from '../config/navigation.mjs';

const entries = JSON.parse(
  await fs.readFile(
    new URL('../config/docs-manifest.json', import.meta.url),
    'utf8',
  ),
);

test('every navigable page appears exactly once; hidden recipes and bridges stay out', () => {
  // Navigation can nest sections; retain the exact route-ownership invariant.
  const flatten = (items) =>
    items.flatMap((item) => (item.items ? flatten(item.items) : [item.link]));
  const routes = flatten(buildSidebar(entries));
  assert.equal(routes.length, new Set(routes).size);
  assert.deepEqual(
    routes.toSorted(),
    entries
      .filter((entry) => entry.navigation)
      .map((entry) => entry.route)
      .toSorted(),
  );
  assert.ok(
    !routes.includes('/docs/guides/search-and-indexing/examples/code/'),
  );
  const homePaths = documentationPaths(entries);
  for (const path of homePaths) assert.ok(routes.includes(path.href));
});

test('unknown sidebar sections and deferred learning paths fail before publication', () => {
  assert.throws(
    () =>
      buildSidebar([
        { ...entries.find((entry) => entry.navigation), section: 'Typo' },
      ]),
    /Unmapped navigation/,
  );
  assert.throws(
    () =>
      documentationPaths(
        entries.map((entry) =>
          entry.id === 'tutorials'
            ? { ...entry, disposition: 'deferred' }
            : entry,
        ),
      ),
    /requires a published page/,
  );
});

test('migration inventory accounts for every source and retains deferral explanations', async () => {
  const report = JSON.parse(
    await fs.readFile(
      new URL('../.generated/migration-report.json', import.meta.url),
      'utf8',
    ),
  );
  const content = JSON.parse(
    await fs.readFile(
      new URL('../.generated/content-report.json', import.meta.url),
      'utf8',
    ),
  );
  assert.equal(report.total, entries.length);
  assert.equal(
    Object.values(report.counts).reduce((sum, count) => sum + count, 0),
    entries.length,
  );
  assert.equal(report.rendered, content.pages.length);
  assert.deepEqual(
    report.sources.map((source) => source.id).toSorted(),
    entries.map((entry) => entry.id).toSorted(),
  );
  for (const entry of entries) {
    const source = report.sources.find((source) => source.id === entry.id);
    assert.equal(source.reviewNote, entry.reviewNote);
    assert.equal(source.disposition, entry.disposition);
    if (entry.disposition === 'deferred') {
      assert.equal(source.route, null);
      assert.equal(source.sha256, null);
      assert.equal(source.effectiveVerification, 'not-checked');
    }
  }
  assert.deepEqual(
    report.sourceCorrections.map((source) => source.source).toSorted(),
    content.pages
      .filter((page) => page.sourceModified)
      .map((page) => page.source)
      .toSorted(),
  );
});
