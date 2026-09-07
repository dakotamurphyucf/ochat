import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import { safeResultUrl, decodeExcerpt } from '../src/scripts/search-utils.ts';
import { searchStatus, searchWeight } from '../config/search.mjs';

test('search result destinations cannot leave approved documentation routes', () => {
  const origin = 'https://docs.ochat.test';
  for (const value of [
    'javascript:alert(1)',
    '//evil.test/docs/',
    '/docs/../../private/',
    '/downloads/hello.chatmd',
    '/docs/a/?q=secret',
    'https://evil.test/docs/',
    '/docs/a/index.html',
  ])
    assert.equal(safeResultUrl(value, origin), null, value);
  assert.equal(
    safeResultUrl('/docs/reference/chatmd/#path-variables', origin),
    '/docs/reference/chatmd/#path-variables',
  );
  assert.equal(safeResultUrl(origin + '/docs/', origin), '/docs/');
});

test('plain excerpts decode entities once without interpreting source or malformed code points', () => {
  assert.equal(
    decodeExcerpt('&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt;'),
    '<script>alert("x")</script>',
  );
  assert.equal(decodeExcerpt('&amp;lt;tool&amp;gt;'), '&lt;tool&gt;');
  assert.equal(
    decodeExcerpt('&#x1F331; &#39; &#0; &#xD800; &#9999999;'),
    "🌱 ' &#0; &#xD800; &#9999999;",
  );
});

test('benchmark expectations are explicit searchable routes and compatibility stays distinctly labeled', async () => {
  const load = async (path) =>
    JSON.parse(await fs.readFile(new URL(path, import.meta.url), 'utf8'));
  const queries = await load('../config/search-queries.json');
  const manifest = await load('../config/docs-manifest.json');
  assert.equal(queries.length, 21);
  assert.equal(new Set(queries.map((q) => q.query)).size, queries.length);
  assert.ok(queries.some((q) => q.query === '${workspace}'));
  for (const q of queries) {
    assert.ok(q.expected.length > 0);
    for (const route of q.expected)
      assert.ok(
        manifest.some((e) => e.route === route && e.search),
        route,
      );
  }
  for (const entry of manifest.filter(
    (e) => e.disposition === 'compatibility',
  )) {
    assert.equal(searchStatus(entry), 'Compatibility');
    assert.equal(searchWeight(entry), 0.1);
  }
  assert.equal(searchStatus({ status: 'experimental' }), 'Experimental');
  assert.equal(searchWeight({ disposition: 'publish' }), undefined);
});
