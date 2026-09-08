import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { visit } from 'unist-util-visit';
import {
  parser,
  resolveLink,
  validateManifest,
  transformMarkdown,
  containedFile,
} from '../scripts/content-lib.mjs';
const entry = {
  id: 'a',
  source: 'docs-src/guide/a.md',
  title: 'A',
  description: 'An example.',
  disposition: 'publish',
  route: '/docs/a/',
  navigation: true,
  search: true,
  sitemap: true,
  noindex: false,
  provenance: 'authored',
};
const other = {
  ...entry,
  id: 'b',
  source: 'docs-src/guide/b.md',
  route: '/docs/b/',
};
const context = {
  tracked: new Set([
    entry.source,
    other.source,
    'lib/Io.mli',
    'docs-src/audit.md',
    'assets/example image.png',
  ]),
  bySource: new Map([
    [entry.source, entry],
    [other.source, other],
  ]),
  assets: { 'assets/example image.png': '/media/example.png' },
  sourceUrl: (p) =>
    `https://github.com/example/ochat/blob/revision/${p.split('/').map(encodeURIComponent).join('/')}`,
};
test('C01/C02: relative routes, queries and fragments resolve from original location', () => {
  assert.equal(
    resolveLink('b.md?q=yes#details', entry.source, context),
    '/docs/b/?q=yes#details',
  );
  assert.equal(resolveLink('../guide/b.md', entry.source, context), '/docs/b/');
});
test('C03/C04: real tracked repository files get source links', () => {
  assert.match(
    resolveLink('../../lib/Io.mli#L20', entry.source, context),
    /blob\/revision\/lib\/Io.mli#L20$/,
  );
  assert.match(
    resolveLink('../audit.md', entry.source, context),
    /blob\/revision\/docs-src\/audit.md$/,
  );
});
test('C05: duplicate route ownership fails with both sources', () => {
  assert.throws(
    () =>
      validateManifest(
        [entry, { ...other, route: entry.route }],
        new Set([entry.source, other.source]),
      ),
    /Route collision.*b.md.*a.md/,
  );
});
test('C06: source paths retain case', () => {
  assert.throws(
    () => resolveLink('../../lib/io.mli', entry.source, context),
    /Unknown/,
  );
});
test('C07: fenced examples, inline code and line endings remain byte exact', () => {
  const source =
    '# A\r\n\r\n```xml\r\n<tool src="b.md">\r\n[b](b.md)\r\n</tool>\r\n```\r\n\r\n`[b](b.md)`\r\n\r\n[b](b.md)';
  const out = transformMarkdown(source, entry, context).body;
  assert.ok(
    out.includes('```xml\r\n<tool src="b.md">\r\n[b](b.md)\r\n</tool>\r\n```'),
  );
  assert.ok(out.includes('`[b](b.md)`'));
  assert.ok(out.includes('[b](/docs/b/)'));
});
test('C08/C09: source title alias is retained; duplicate headings are inventoried', () => {
  const result = transformMarkdown(
    '# Original title\n\n## One\n\n## One\n',
    entry,
    context,
  );
  assert.ok(result.body.startsWith('<a id="original-title"></a>'));
  assert.deepEqual(result.headings, ['original-title', 'one', 'one-1']);
});
test('C13/C14: only allowlisted images and encoded paths are published', () => {
  assert.equal(
    resolveLink(
      '../../assets/example%20image.png',
      entry.source,
      context,
      true,
    ),
    '/media/example.png',
  );
  assert.throws(
    () => resolveLink('b.md', entry.source, context, true),
    /allowlist/,
  );
});
test('C15: reference-style definitions transform without touching labels', () => {
  const result = transformMarkdown(
    '# A\n\nUse [the reference][ref].\n\n[ref]: b.md#intro "B"\n',
    entry,
    context,
  );
  assert.ok(result.body.includes('[the reference][ref]'));
  assert.ok(result.body.includes('[ref]: /docs/b/#intro "B"'));
});
test('C16/C22: raw HTML links, explicit anchors and keyboard semantics survive', () => {
  const result = transformMarkdown(
    '# A\n\n<a id="legacy"></a>\n\nPress <kbd>Esc</kbd> or <a href="b.md#next">read more</a>.',
    entry,
    context,
  );
  assert.ok(result.body.includes('<a id="legacy"></a>'));
  assert.ok(result.body.includes('<kbd>Esc</kbd>'));
  assert.ok(result.body.includes('href="/docs/b/#next"'));
});
test('C17: paths cannot escape the repository, even when encoded', () => {
  for (const u of [
    '../../../private.txt',
    '%2Fetc/passwd',
    'javascript:alert(1)',
    '//evil.example/a',
    'file:///etc/passwd',
    'missing.md',
  ])
    assert.throws(() => resolveLink(u, entry.source, context));
});
test('raw HTML scripts and event handlers are rejected; fenced script is data', () => {
  for (const html of [
    '<script>alert(1)</script>',
    '<img src="x" onerror="alert(1)">',
  ])
    assert.throws(() => transformMarkdown('# A\n\n' + html, entry, context));
  assert.ok(
    transformMarkdown(
      '# A\n\n```xml\n<script>hello</script>\n```',
      entry,
      context,
    ).body.includes('<script>hello</script>'),
  );
});
test('C18/C19: every source needs a disposition; deferred pages cannot own routes', () => {
  assert.throws(
    () => validateManifest([entry], new Set([entry.source, other.source])),
    /Missing disposition/,
  );
  assert.throws(
    () =>
      validateManifest(
        [{ ...entry, disposition: 'deferred' }],
        new Set([entry.source]),
      ),
    /Unpublished/,
  );
});
test('C20: transformations are deterministic', () => {
  const source = '# A\n\n[b](b.md)\n';
  assert.deepEqual(
    transformMarkdown(source, entry, context),
    transformMarkdown(source, entry, context),
  );
});
test('C21: generated-from-code sources require an existing generator', () => {
  assert.throws(
    () =>
      validateManifest(
        [{ ...entry, provenance: 'generated-from-code' }],
        new Set([entry.source]),
      ),
    /Missing generator/,
  );
});
test('C23: real nested fence fixture preserves the intended HTML example', async () => {
  const source = await fs.readFile(
    new URL(
      '../../docs-src/lib/webpage_markdown/md_render.doc.md',
      import.meta.url,
    ),
    'utf8',
  );
  const codes = [];
  visit(parser.parse(source), 'code', (n) => {
    codes.push(n);
  });
  assert.equal(
    codes.find((n) => n.lang === 'markdown')?.value,
    '```html\n<original-html/>\n```',
  );
});
test('C25: symlink escape is rejected; Unicode and spaces in contained paths work', async () => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), 'ochat-site-'));
  try {
    await fs.writeFile(path.join(root, 'café file.md'), 'ok');
    assert.equal(
      await containedFile(root, 'café file.md'),
      await fs.realpath(path.join(root, 'café file.md')),
    );
    await fs.symlink(os.tmpdir(), path.join(root, 'outside'));
    await assert.rejects(containedFile(root, 'outside'), /escapes/);
  } finally {
    await fs.rm(root, { recursive: true, force: true });
  }
});
test('C26: bridge policy is explicit and independent', () => {
  assert.throws(
    () =>
      validateManifest(
        [{ ...entry, disposition: 'bridge' }],
        new Set([entry.source]),
      ),
    /Bridge policy/,
  );
  validateManifest(
    [
      {
        ...entry,
        disposition: 'bridge',
        navigation: false,
        search: false,
        sitemap: false,
        noindex: true,
      },
    ],
    new Set([entry.source]),
  );
});

test('all published Markdown retains its original fenced code values', async () => {
  const entries = JSON.parse(
    await fs.readFile(
      new URL('../config/docs-manifest.json', import.meta.url),
      'utf8',
    ),
  );
  function fences(text) {
    const out = [];
    visit(parser.parse(text), 'code', (n) => {
      out.push({ lang: n.lang, value: n.value });
    });
    return out;
  }
  for (const e of entries.filter((e) =>
    ['publish', 'bridge', 'compatibility'].includes(e.disposition),
  )) {
    const source = await fs.readFile(
      new URL(`../../${e.source}`, import.meta.url),
      'utf8',
    );
    const generated = await fs.readFile(
      new URL(
        `../.generated/docs/${e.route.slice(1)}index.md`,
        import.meta.url,
      ),
      'utf8',
    );
    assert.deepEqual(
      fences(generated),
      fences(source),
      `Fenced code changed in ${e.source}`,
    );
  }
});

test('first-agent prompt is identical to the maintained runtime fixture', async () => {
  const tutorial = await fs.readFile(
    new URL(
      '../../docs-src/agent-server/tutorials/local-tui.md',
      import.meta.url,
    ),
    'utf8',
  );
  const fixture = await fs.readFile(
    new URL(
      '../../docs-src/examples/agent-server/prompts/hello.chatmd',
      import.meta.url,
    ),
    'utf8',
  );
  const example = parser
    .parse(tutorial)
    .children.find((n) => n.type === 'code' && n.lang === 'xml');
  assert.equal(example.value, fixture.trimEnd());
});
test('XML-like parser error prose remains literal code', async () => {
  const source = await fs.readFile(
    new URL('../../docs-src/lib/chatmd/chatmd_parser.doc.md', import.meta.url),
    'utf8',
  );
  const codes = [];
  visit(parser.parse(source), 'inlineCode', (n) => {
    codes.push(n.value);
  });
  assert.ok(codes.includes('Mismatching tags: <msg> … </user>'));
});

test('linked source titles keep their anchor without overlapping edits', () => {
  const result = transformMarkdown(
    '# [Original](b.md) title\n\n[Continue](b.md)\n',
    entry,
    context,
  );
  assert.match(result.body, /<a id="original-title"><\/a>/);
  assert.match(result.body, /\[Continue\]\(\/docs\/b\/\)/);
});
test('reference images obey the same asset policy as inline images', () => {
  const source =
    '# A\n\n![Diagram][diagram]\n\n[diagram]: ../../assets/example%20image.png\n';
  assert.match(
    transformMarkdown(source, entry, context).body,
    /\[diagram\]: \/media\/example.png/,
  );
  assert.throws(
    () => transformMarkdown(source, entry, { ...context, assets: {} }),
    /allowlist/,
  );
});

test('reviewed image loading hints are preserved while invalid values and event attributes stay rejected', () => {
  const image =
    '<img src="../../assets/example%20image.png" alt="Historical view" width="2420" height="2076" loading="lazy" decoding="async"/>';
  const output = transformMarkdown('# A\n\n' + image, entry, context).body;
  assert.match(output, /loading="lazy"/);
  assert.match(output, /decoding="async"/);
  assert.throws(
    () =>
      transformMarkdown(
        '# A\n\n' + image.replace('loading="lazy"', 'loading="unknown"'),
        entry,
        context,
      ),
    /Invalid image loading/,
  );
  assert.throws(
    () =>
      transformMarkdown(
        '# A\n\n' + image.replace('loading="lazy"', 'onload="alert(1)"'),
        entry,
        context,
      ),
    /Unsupported HTML attribute/,
  );
});
