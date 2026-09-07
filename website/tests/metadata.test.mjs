import test from 'node:test';
import assert from 'node:assert/strict';
import { pageMetadata } from '../scripts/content.mjs';
import {
  transformMarkdown,
  validateManifest,
} from '../scripts/content-lib.mjs';
const e = {
  id: 'a',
  source: 'docs-src/a.md',
  title: 'A',
  description: 'About A',
  route: '/docs/a/',
  disposition: 'publish',
  provenance: 'authored',
  navigation: false,
  search: true,
  sitemap: false,
  noindex: true,
};
const options = {
  isProduction: true,
  sourceUrl: () => 'https://github.com/source',
  provenance: { lastUpdated: null },
  byId: new Map(),
};
test('C26/C30: navigation, search, sitemap, and robots are independent policies', () => {
  const meta = pageMetadata(e, options);
  assert.equal(meta.sidebar.hidden, true);
  assert.equal(meta.pagefind, true);
  assert.equal(meta.head[0].attrs.content, 'noindex, nofollow');
  assert.deepEqual(pageMetadata({ ...e, noindex: false }, options).head, []);
  assert.equal(
    pageMetadata({ ...e, noindex: false }, { ...options, isProduction: false })
      .head[0].attrs.name,
    'robots',
  );
});
test('source frontmatter cannot override route or conflicting metadata', () => {
  const context = { tracked: new Set(), assets: {}, bySource: new Map() };
  assert.match(
    transformMarkdown('---\ntitle: A\n---\n# A\n', e, context).body,
    /<a id="a">/,
  );
  assert.throws(
    () => transformMarkdown('---\nslug: wrong\n---\n# A\n', e, context),
    /conflicts.*slug/,
  );
  assert.throws(
    () => transformMarkdown('---\ntitle: Wrong\n---\n# A\n', e, context),
    /conflicts.*title/,
  );
});
test('manifest rejects typos, impossible dates, and unverifiable checked labels', () => {
  for (const item of [
    { ...e, serach: true },
    { ...e, verifiedAt: '2026-99-99' },
    { ...e, verification: 'live-checked' },
  ])
    assert.throws(() => validateManifest([item], new Set([e.source])));
});
test('related navigation cannot expose deferred entries', () => {
  assert.throws(
    () => pageMetadata({ ...e, related: ['missing'] }, options),
    /Unpublished related/,
  );
});
