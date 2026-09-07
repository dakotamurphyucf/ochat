import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import {
  validateExampleSelection,
  effectiveVerification,
  publishExamples,
  sourceArchive,
  sourceText,
} from '../scripts/examples.mjs';
import { supplementalInventory } from '../scripts/publication-policy.mjs';
import { pageMetadata, languageAliases } from '../scripts/content.mjs';
import { parser } from '../scripts/content-lib.mjs';
import { digest } from '../scripts/provenance.mjs';
const root = fileURLToPath(new URL('../../', import.meta.url));
const read = async (file) =>
  JSON.parse(await fs.readFile(path.join(root, file), 'utf8'));
const input = await read('docs-src/examples/catalog.json');
const entries = await read('website/config/docs-manifest.json');
const curriculum = await read('website/config/tutorials.json');
const report = await read('website/.generated/examples-report.json');
const policy = await read('website/config/supplemental-sources.json');
const assets = await read('website/config/assets.json');
const tracked = new Set(
  execFileSync('git', ['ls-files', '-z'], { cwd: root, encoding: 'utf8' })
    .split('\0')
    .filter(Boolean),
);

test('ChatML documentation fences select OCaml highlighting without changing the source label', async () => {
  assert.equal(languageAliases.chatml, 'ocaml');
  const content = await read('website/.generated/content-report.json');
  const aliases = content.languageFallbacks.filter(
    (f) => f.language === 'chatml',
  );
  assert.ok(aliases.length > 0);
  assert.ok(aliases.every((f) => f.highlightAs === 'ocaml'));
});

test('inline source preserves approved file bytes and refuses malformed UTF-8', async () => {
  for (const example of report.examples) {
    for (const file of example.files) {
      const bytes = await fs.readFile(path.join(root, file.source));
      assert.deepEqual(Buffer.from(file.content, 'utf8'), bytes);
      assert.equal(digest(file.content), file.sha256);
    }
  }
  const literal = Buffer.from(
    '\ufeff<user>café & <script>alert(1)</script></user>\r\n',
  );
  assert.deepEqual(Buffer.from(sourceText(literal)), literal);
  assert.throws(() => sourceText(Buffer.from([0xc3, 0x28])), /encoded data/);
});

test('catalog enforces explicit file ownership, paths, dependency edges, notices and example types', () => {
  assert.equal(validateExampleSelection(input, entries, tracked).length, 14);
  assert.deepEqual(
    ['complete', 'template', 'illustration'].map(
      (kind) => input.filter((e) => e.kind === kind).length,
    ),
    [8, 5, 1],
  );
  const source = input.find((e) => e.id === 'specialist');
  const reject = (mutate, pattern) => {
    const e = structuredClone(source);
    mutate(e);
    assert.throws(
      () => validateExampleSelection([e], entries, tracked),
      pattern,
    );
  };
  reject((e) => e.files.pop(), /license/);
  reject(
    (e) => (e.files = e.files.filter((f) => f.path !== 'docs-reviewer.chatmd')),
    /dependency edge/,
  );
  reject((e) => (e.files[0].path = '../private.chatmd'), /contained relative/);
  reject(
    (e) => (e.files[0].source = 'scratch/private.chatmd'),
    /Unapproved or untracked/,
  );
  reject((e) => e.files.push({ ...e.files[0] }), /Duplicate example file/);
  reject((e) => (e.edges = []), /Undeclared companion/);
  reject((e) => (e.edges[0].reference = 'elsewhere.chatmd'), /dependency edge/);
  reject((e) => (e.kind = 'illustration'), /Illustration must/);
  assert.throws(
    () => validateExampleSelection([source, source], entries, tracked),
    /Duplicate example ID/,
  );
});

test('supplemental exact-file approval and download selection must agree in both directions', () => {
  const all = supplementalInventory(policy, tracked, assets, input);
  assert.equal(
    all.filter((e) => e.disposition === 'example-download').length,
    new Set(input.flatMap((e) => e.files.map((f) => f.source))).size,
  );
  assert.throws(
    () => supplementalInventory(policy, tracked, assets, []),
    /no example destination/,
  );
  const changed = structuredClone(policy);
  changed.files = changed.files.filter(
    (f) =>
      f.source !== 'docs-src/examples/learning/specialist/docs-reviewer.chatmd',
  );
  assert.throws(
    () => supplementalInventory(changed, tracked, assets, input),
    /exact example download approval/,
  );
});

test('every bundle extracts with native tar to exactly the approved source bytes and relative layout', async () => {
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'ochat-bundles-'));
  try {
    for (const e of report.examples.filter((e) => e.bundle)) {
      const archive = path.join(
        root,
        'website/.generated/public',
        e.bundle.href,
      );
      const names = execFileSync('tar', ['-tf', archive], { encoding: 'utf8' })
        .trim()
        .split('\n');
      assert.deepEqual(
        names,
        e.files.map((f) => `${e.id}/${f.path}`),
      );
      execFileSync('tar', ['-xf', archive, '-C', temporary]);
      const sources = [];
      for (const f of e.files) {
        const source = await fs.readFile(path.join(root, f.source));
        assert.deepEqual(
          await fs.readFile(path.join(temporary, e.id, f.path)),
          source,
        );
        assert.deepEqual(
          await fs.readFile(
            path.join(root, 'website/.generated/public', f.href),
          ),
          source,
        );
        assert.equal(digest(source), f.sha256);
        sources.push({ ...f, bytes: source });
      }
      assert.deepEqual(
        sourceArchive(e.id, sources),
        await fs.readFile(archive),
      );
      assert.equal(digest(await fs.readFile(archive)), e.bundle.sha256);
      assert.equal(
        (await fs.stat(path.join(temporary, e.id, e.entry))).mode & 0o111,
        0,
      );
    }
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});

test('verification is invalidated by changed source bytes, revision, or missing evidence and never invents live work', async () => {
  const temporary = await fs.mkdtemp(
    path.join(os.tmpdir(), 'ochat-verification-'),
  );
  try {
    await fs.writeFile(path.join(temporary, 'sample.txt'), 'one');
    const record = {
      ...input[0].verification,
      state: 'offline-checked',
      hashes: { 'sample.txt': digest('one') },
    };
    const context = {
      root: temporary,
      tracked: new Set(['sample.txt']),
      revision: record.baseRevision,
    };
    assert.equal(
      (await effectiveVerification(record, ['sample.txt'], context)).state,
      'offline-checked',
    );
    assert.equal(
      (await effectiveVerification(record, ['extra.txt'], context)).state,
      'not-checked',
    );
    assert.equal(
      (
        await effectiveVerification(record, ['sample.txt'], {
          ...context,
          revision: '0'.repeat(40),
        })
      ).state,
      'not-checked',
    );
    await fs.writeFile(path.join(temporary, 'sample.txt'), 'two');
    assert.equal(
      (await effectiveVerification(record, ['sample.txt'], context)).state,
      'not-checked',
    );
    await assert.rejects(
      () =>
        effectiveVerification(
          { ...record, state: 'live-checked', liveProvider: false },
          [],
          context,
        ),
      /Live verification/,
    );
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});

test('download publication rejects escaping symlinks and uncommitted release sources', async () => {
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'ochat-download-'));
  try {
    const checkout = path.join(temporary, 'repo'),
      stage = path.join(temporary, 'stage');
    const e = structuredClone(input[0]);
    e.verification.hashes = {};
    e.verification.state = 'not-checked';
    const source = e.files[0].source;
    await fs.mkdir(path.dirname(path.join(checkout, source)), {
      recursive: true,
    });
    await fs.writeFile(path.join(temporary, 'outside.txt'), 'outside');
    await fs.symlink(
      path.join(temporary, 'outside.txt'),
      path.join(checkout, source),
    );
    await fs.writeFile(path.join(checkout, 'LICENSE.txt'), 'license');
    const options = {
      root: checkout,
      stage,
      entries,
      tracked: new Set(e.files.map((f) => f.source)),
      supplemental: e.files.map((f) => ({
        source: f.source,
        disposition: 'example-download',
      })),
      facts: {
        revision: e.verification.baseRevision,
        source: () => ({ sourceModified: true }),
      },
      sourceUrl: (s) => `https://example.invalid/${s}`,
      isProduction: false,
    };
    await assert.rejects(
      () => publishExamples([e], options),
      /escapes|outside|symlink/i,
    );
    await fs.unlink(path.join(checkout, source));
    await fs.writeFile(path.join(checkout, source), 'prompt');
    await assert.rejects(
      () => publishExamples([e], { ...options, isProduction: true }),
      /committed example bytes/,
    );
  } finally {
    await fs.rm(temporary, { recursive: true, force: true });
  }
});

test('ten tutorial records drive actual previous and next links without changing existing route owners', () => {
  assert.deepEqual(
    report.tutorials.map((t) => t.id),
    Array.from({ length: 10 }, (_, i) => `T${String(i + 1).padStart(2, '0')}`),
  );
  const byId = new Map(entries.map((e) => [e.id, e]));
  for (const [index, t] of curriculum.entries()) {
    const metadata = pageMetadata(byId.get(t.page), {
      sourceUrl: () => '',
      provenance: {},
      byId,
      tutorials: curriculum,
    });
    assert.equal(
      metadata.prev?.link,
      index ? report.tutorials[index - 1].route : undefined,
    );
    assert.equal(
      metadata.next.link,
      index < 9 ? report.tutorials[index + 1].route : '/docs/examples/',
    );
    assert.ok(report.tutorials[index].examples.length);
  }
});

test('new tutorial code fences preserve the complete maintained prompts, scripts, and data', async () => {
  for (const [tutorial, examples] of [
    ['file-tool', ['file-reader']],
    ['specialist', ['specialist']],
    ['workflow', ['three-turns']],
  ]) {
    const document = await fs.readFile(
      path.join(root, `docs-src/tutorials/${tutorial}.md`),
      'utf8',
    );
    const fences = parser
      .parse(document)
      .children.filter((n) => n.type === 'code')
      .map((n) => n.value);
    for (const e of input.filter((e) => examples.includes(e.id))) {
      for (const f of e.files.filter(
        (f) =>
          f.role === 'entry' ||
          f.role === 'companion' ||
          (tutorial === 'file-tool' && f.role === 'data'),
      )) {
        const source = (
          await fs.readFile(path.join(root, f.source), 'utf8')
        ).replace(/\n$/, '');
        assert.ok(
          fences.includes(source),
          `${tutorial}: fence differs from ${f.source}`,
        );
      }
    }
  }
});

test('new local sources do not invent an immutable GitHub edit link', () => {
  const entry = entries.find((e) => e.id === 'tutorials/file-tool');
  const metadata = pageMetadata(entry, {
    sourceUrl: () => {
      throw new Error('must not invent source link');
    },
    provenance: { sourceCommit: null },
    byId: new Map(entries.map((e) => [e.id, e])),
  });
  assert.equal(metadata.editUrl, false);
});
