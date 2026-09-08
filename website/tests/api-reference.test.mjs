import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import searchIndex from '../scripts/search-index.mjs';
import { assertApiReferenceExcluded } from '../config/api-reference.mjs';
import { buildSidebar } from '../config/navigation.mjs';
import { sitemapIncluded } from '../config/presentation.mjs';
const origin = 'https://docs.ochat.test';

test('indexing refuses an accidental API artifact before creating prose search files', async () => {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'ochat-api-exclusion-'));
  try {
    await fs.mkdir(path.join(dir, 'api'));
    await fs.writeFile(
      path.join(dir, 'api', 'db.js'),
      'unexpected API search index',
    );
    await assert.rejects(
      searchIndex().hooks['astro:build:done']({
        dir: pathToFileURL(dir + '/'),
      }),
      /API reference is deferred/,
    );
    await assert.rejects(fs.access(path.join(dir, 'pagefind')), {
      code: 'ENOENT',
    });
  } finally {
    await fs.rm(dir, { recursive: true, force: true });
  }
});

test('deferred API boundary rejects copies, navigation, and same-origin encoded URLs', () => {
  for (const file of [
    'api',
    'api/index.html',
    'api/ochat/db.js',
    'api\\odoc.support\\odoc.css',
  ])
    assert.throws(
      () => assertApiReferenceExcluded({ files: [file] }),
      /API reference is deferred/,
    );
  for (const url of [
    '/api',
    '/api/',
    '/api/ochat/index.html#type-t',
    '/%61pi/',
    origin + '/api/?q=foo',
    '/docs/../api/',
  ])
    assert.throws(
      () => assertApiReferenceExcluded({ urls: [url], origin }),
      /API reference is deferred/,
    );
  assert.doesNotThrow(() =>
    assertApiReferenceExcluded({
      files: ['docs/integrations/ocaml/index.html', '_astro/api-client.js'],
      urls: [
        '/docs/integrations/ocaml/',
        'https://example.org/api/',
        'mailto:docs@example.org',
      ],
      origin,
    }),
  );
  assert.throws(
    () => buildSidebar([{ disposition: 'publish', route: '/api/' }]),
    /API reference is deferred/,
  );
  for (const production of [true, false])
    assert.equal(sitemapIncluded('/api/', [], production), false);
});
