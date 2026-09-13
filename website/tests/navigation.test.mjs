import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import { buildSidebar, documentationPaths } from '../config/navigation.mjs';

test('feature destinations remain visible and onboarding stays in order', async () => {
  const entries = JSON.parse(
    await fs.readFile(new URL('../config/docs-manifest.json', import.meta.url)),
  );
  const sidebar = buildSidebar(entries);
  const links = (items) =>
    items.flatMap((item) => (item.link ? [item.link] : links(item.items)));
  const start = links(
    sidebar.find((group) => group.label === 'Start here').items,
  );
  const installation = start.indexOf('/docs/start/installation/');
  assert.ok(installation >= 0);
  assert.deepEqual(start.slice(installation, installation + 3), [
    '/docs/start/installation/',
    '/docs/start/build-troubleshooting/',
    '/docs/start/first-agent/',
  ]);
  for (const label of [
    'Tools and shell access',
    'Subagents and agent teams',
    'ChatML workflows',
  ])
    assert.ok(sidebar.some((group) => group.label === label));
  assert.deepEqual(
    documentationPaths(entries).map((item) => item.href),
    [
      '/docs/concepts/shell-access/',
      '/docs/guides/subagents/',
      '/docs/concepts/chatml/',
    ],
  );
});
