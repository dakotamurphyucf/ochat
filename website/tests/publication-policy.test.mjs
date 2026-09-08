import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import {
  supplementalInventory,
  capabilityCoverage,
} from '../scripts/publication-policy.mjs';
import {
  resolveLink,
  bridgeMappings,
  transformMarkdown,
} from '../scripts/content-lib.mjs';
const read = async (file) =>
  JSON.parse(await fs.readFile(new URL(file, import.meta.url), 'utf8'));
const entries = await read('../config/docs-manifest.json');
const policy = await read('../config/supplemental-sources.json');
const assets = await read('../config/assets.json');
const examples = await read('../../docs-src/examples/catalog.json');
const capabilities = await read('../config/capabilities.json');
const tracked = new Set(
  execFileSync('git', ['ls-files', '-z'], {
    cwd: new URL('../../', import.meta.url),
    encoding: 'utf8',
  })
    .split('\0')
    .filter(Boolean),
);

test('inline HTML heading anchors retain their alias and match renderer text slugs', () => {
  const entry = {
    id: 'test',
    source: 'docs-src/test.md',
    disposition: 'publish',
  };
  const context = {
    tracked: new Set(),
    assets: {},
    bySource: new Map(),
    sourceUrl: () => 'unused',
  };
  const result = transformMarkdown(
    '# Title\n\n## API <a id="legacy-api"></a>\n\n## **Typed** <em>calls</em>\n',
    entry,
    context,
  );
  assert.deepEqual(result.headings, ['title', 'api-', 'typed-calls']);
  assert.ok(result.body.includes('id="legacy-api"'));
  const bridge = { ...entry, disposition: 'bridge' };
  assert.throws(
    () => bridgeMappings('# Missing target\n', bridge, context),
    /no forwarding link/,
  );
});

test('every legacy TUI bridge records all original heading destinations', async () => {
  const report = await read('../.generated/content-report.json');
  const bridges = report.pages.filter((page) => page.disposition === 'bridge');
  assert.equal(bridges.length, 9);
  for (const page of bridges) {
    assert.deepEqual(
      page.bridgeMappings.map((mapping) => mapping.fragment),
      page.headings,
    );
    for (const mapping of page.bridgeMappings)
      assert.ok(
        mapping.target.startsWith('/docs/') ||
          /^https:\/\/github.com\/dakotamurphyucf\/ochat\/blob\/[a-f0-9]{40}\//.test(
            mapping.target,
          ),
      );
  }
});

test('supplemental policy accounts for root prose, prompts, historical sessions, and example companions without granting copies', () => {
  const inventory = supplementalInventory(policy, tracked, assets, examples);
  const bySource = new Map(inventory.map((entry) => [entry.source, entry]));
  assert.equal(bySource.get('Readme.md').disposition, 'hero-source');
  assert.equal(
    bySource.get('prompt-examples/readme.md').disposition,
    'example-review',
  );
  assert.equal(
    bySource.get('real-world-example-session/update-tool-docs/readme.md')
      .disposition,
    'repository-only',
  );
  assert.equal(
    bySource.get('docs-src/examples/tools/custom_tool.ml').disposition,
    'example-download',
  );
  assert.equal(
    inventory.filter((entry) => entry.disposition === 'media').length,
    Object.keys(assets).length,
  );
  assert.ok(!bySource.has('docs-src/README.md'));
  assert.ok(
    ![...bySource.keys()].some((source) => source.startsWith('scratch/')),
  );
  assert.throws(
    () =>
      supplementalInventory(
        policy,
        new Set([...tracked, 'new-guide.md']),
        assets,
        examples,
      ),
    /Missing supplemental disposition/,
  );
  assert.throws(
    () =>
      supplementalInventory(
        policy,
        tracked,
        {
          ...assets,
          'prompt-examples/readme.md': '/media/prompt.md',
        },
        examples,
      ),
    /exact supplemental media approval/,
  );
});

test('supplemental policy rejects untracked, escaping, overlapping, and unclassified source links', () => {
  const rule = {
    source: 'missing.md',
    disposition: 'repository-only',
    reason: 'This is a selected source with an explicit reason.',
  };
  assert.throws(
    () =>
      supplementalInventory(
        { ...policy, files: [...policy.files, rule] },
        tracked,
        assets,
        examples,
      ),
    /Untracked supplemental/,
  );
  assert.throws(
    () =>
      supplementalInventory(
        {
          ...policy,
          files: [...policy.files, { ...rule, source: '../private.md' }],
        },
        tracked,
        assets,
        examples,
      ),
    /Invalid or duplicate/,
  );
  assert.throws(
    () =>
      supplementalInventory(
        {
          ...policy,
          directories: [
            ...policy.directories,
            { ...rule, source: 'lib/chatmd/' },
          ],
        },
        tracked,
        assets,
        examples,
      ),
    /Overlapping/,
  );
  const context = {
    tracked: new Set(['unclassified.ml']),
    assets: {},
    bySource: new Map(),
    supplemental: new Map(),
    sourceUrl: () => 'unused',
  };
  assert.throws(
    () => resolveLink('../unclassified.ml', 'docs-src/README.md', context),
    /Missing supplemental link disposition/,
  );
});

test('all fourteen required capabilities have searchable reader destinations and explicit deferred detail', () => {
  const coverage = capabilityCoverage(capabilities, entries);
  assert.deepEqual(
    coverage.map((c) => c.id).sort(),
    [
      'composition',
      'tools',
      'mcp',
      'custom-tools',
      'retrieval',
      'refinement',
      'compaction',
      'chatml',
      'background',
      'tui',
      'shell',
      'protocol',
      'embedding',
      'commands',
    ].sort(),
  );
  assert.throws(
    () =>
      capabilityCoverage(
        [{ ...capabilities[0], pages: ['renderer-bridge'] }],
        entries,
      ),
    /searchable reader destination/,
  );
  assert.throws(
    () =>
      capabilityCoverage([{ ...capabilities[0], deferred: ['home'] }], entries),
    /unpublished source and reason/,
  );
  assert.throws(
    () => capabilityCoverage([capabilities[0], capabilities[0]], entries),
    /Duplicate capability/,
  );
  assert.ok(
    entries.every((e) => !e.reviewNote.includes('initial technical spike')),
  );
});
