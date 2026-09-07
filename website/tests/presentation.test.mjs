import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import sharp from 'sharp';
import { socialPath, sitemapIncluded } from '../config/presentation.mjs';
import { digest } from '../scripts/provenance.mjs';
test('preview sitemap stays absent and production includes only eligible canonical routes', () => {
  const entries = [
    { route: '/docs/current/', sitemap: true, noindex: false },
    { route: '/docs/old/', sitemap: false, noindex: true },
    { route: '/docs/private/', sitemap: true, noindex: true },
  ];
  for (const route of ['/', ...entries.map((e) => e.route)])
    assert.equal(sitemapIncluded(route, entries, false), false);
  assert.equal(sitemapIncluded('/', entries, true), true);
  assert.equal(sitemapIncluded('/docs/current/', entries, true), true);
  assert.equal(sitemapIncluded('/docs/old/', entries, true), false);
  assert.equal(sitemapIncluded('/docs/private/', entries, true), false);
});
test('generated social art covers every rendered route without collisions and preserves declared dimensions and bytes', async () => {
  const generated = new URL('../.generated/', import.meta.url);
  const content = JSON.parse(
    await fs.readFile(new URL('content-report.json', generated)),
  );
  const report = JSON.parse(
    await fs.readFile(new URL('publishing-assets.json', generated)),
  );
  const cards = report.assets.filter((a) => a.kind === 'social');
  assert.deepEqual(
    cards.map((a) => a.route).sort(),
    ['/', ...content.pages.map((p) => p.route)].sort(),
  );
  assert.equal(new Set(cards.map((a) => a.url)).size, cards.length);
  for (const asset of report.assets) {
    const bytes = await fs.readFile(new URL('public' + asset.url, generated));
    assert.equal(digest(bytes), asset.sha256);
    if (asset.kind === 'social') {
      assert.equal(asset.url, socialPath(asset.route));
      const image = await sharp(bytes).metadata();
      assert.equal(image.width, 1200);
      assert.equal(image.height, 630);
      assert.equal(image.format, 'png');
    }
  }
});
