import fs from 'node:fs/promises';
import path from 'node:path';
import { z } from 'zod';
import { containedFile, rendered } from './content-lib.mjs';
import { digest } from './provenance.mjs';

const text = z.string().min(1);
// Extensionless files are treated as routes by static preview hosts. The
// download attribute and archive retain the real filename (for example dune).
export const downloadHref = (id, file) =>
  `/downloads/${id}/${file}${path.posix.extname(file) ? '' : '.txt'}`;
// Decode at the same approved-source boundary as downloads. Never silently
// replace malformed bytes or strip a BOM from the displayed source.
export function sourceText(bytes) {
  return new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(
    bytes,
  );
}
const relative = text.refine(
  (s) =>
    /^[a-zA-Z0-9_.\/-]+$/.test(s) &&
    !s.startsWith('/') &&
    !s.split('/').some((p) => !p || p === '.' || p === '..'),
  'Expected a contained relative file path',
);
export const verificationSchema = z
  .object({
    state: z.enum([
      'offline-checked',
      'live-checked',
      'known-limitation',
      'not-checked',
    ]),
    baseRevision: z.string().regex(/^[a-f0-9]{40}$/),
    testedAt: z.iso.date(),
    platform: text,
    toolchain: text,
    liveProvider: z.boolean(),
    commands: z.array(text).min(1),
    observed: text,
    limitations: text,
    hashes: z.record(relative, z.string().regex(/^[a-f0-9]{64}$/)),
  })
  .strict();
const fileSchema = z
  .object({
    source: relative,
    path: relative,
    role: z.enum(['entry', 'companion', 'data', 'build', 'notice']),
  })
  .strict();
export const exampleSchema = z.array(
  z
    .object({
      id: z.string().regex(/^[a-z0-9-]+$/),
      title: text,
      summary: text,
      tutorial: text,
      host: text,
      capabilities: z.array(text).min(1),
      required: z.array(text).min(1),
      kind: z.enum(['complete', 'template', 'illustration']),
      entry: relative.nullable(),
      files: z.array(fileSchema),
      edges: z.array(
        z
          .object({
            fromPath: relative,
            toPath: relative,
            reference: relative,
            kind: z.enum(['agent', 'script', 'import']),
          })
          .strict(),
      ),
      illustrationSource: relative.optional(),
      verification: verificationSchema,
    })
    .strict(),
);
const tutorialSchema = z.array(
  z
    .object({
      id: z.string().regex(/^T\d{2}$/),
      page: text,
      host: text,
      examples: z.array(text).min(1),
      verification: verificationSchema,
    })
    .strict(),
);

export async function effectiveVerification(
  record,
  required,
  { root, tracked, revision },
) {
  verificationSchema.parse(record);
  let current =
    record.baseRevision === revision && required.every((s) => record.hashes[s]);
  for (const [source, expected] of Object.entries(record.hashes)) {
    if (!tracked.has(source))
      throw new Error(`Untracked verification source: ${source}`);
    if (
      digest(await fs.readFile(await containedFile(root, source))) !== expected
    )
      current = false;
  }
  if (record.state === 'live-checked' && !record.liveProvider)
    throw new Error('Live verification requires a recorded live provider run');
  return {
    ...record,
    state: current ? record.state : 'not-checked',
    current,
    scope: current
      ? record.observed
      : 'Source or revision changed; recorded checks need repeating.',
  };
}

// Deterministic POSIX ustar: regular files only, no executable bits, links,
// timestamps, machine usernames, or absolute paths. Source bytes stay untouched.
export function sourceArchive(id, files) {
  const blocks = [];
  for (const file of files) {
    const name = `${id}/${file.path}`;
    if (Buffer.byteLength(name) > 100)
      throw new Error(`Archive path too long: ${name}`);
    const header = Buffer.alloc(512);
    header.write(name);
    const octal = (value, start, length) =>
      header.write(
        value.toString(8).padStart(length - 1, '0') + '\0',
        start,
        length,
        'ascii',
      );
    octal(0o644, 100, 8);
    octal(0, 108, 8);
    octal(0, 116, 8);
    octal(file.bytes.length, 124, 12);
    octal(0, 136, 12);
    header.fill(32, 148, 156);
    header.write('0', 156);
    header.write('ustar\0', 257);
    header.write('00', 263);
    const checksum = header.reduce((sum, byte) => sum + byte, 0);
    header.write(
      checksum.toString(8).padStart(6, '0') + '\0 ',
      148,
      8,
      'ascii',
    );
    blocks.push(
      header,
      file.bytes,
      Buffer.alloc((512 - (file.bytes.length % 512)) % 512),
    );
  }
  return Buffer.concat([...blocks, Buffer.alloc(1024)]);
}

export function validateExampleSelection(input, entries, tracked) {
  const examples = exampleSchema.parse(input),
    ids = new Set();
  for (const e of examples) {
    if (ids.has(e.id)) throw new Error(`Duplicate example ID: ${e.id}`);
    ids.add(e.id);
    const page = entries.find((p) => p.id === e.tutorial);
    if (!page || !rendered(page) || !page.search)
      throw new Error(`Missing example destination: ${e.id}`);
    if (e.kind === 'illustration') {
      if (
        e.files.length ||
        e.entry ||
        e.edges.length ||
        !tracked.has(e.illustrationSource)
      )
        throw new Error(
          `Illustration must remain a tracked reading source: ${e.id}`,
        );
      continue;
    }
    if (e.illustrationSource)
      throw new Error(`Unexpected illustration source: ${e.id}`);
    const paths = new Set(),
      sources = new Set();
    for (const f of e.files) {
      if (
        !tracked.has(f.source) ||
        !(
          f.source.startsWith('docs-src/examples/') ||
          f.source === 'LICENSE.txt'
        )
      )
        throw new Error(`Unapproved or untracked download: ${f.source}`);
      if (paths.has(f.path) || sources.has(f.source))
        throw new Error(`Duplicate example file: ${e.id}/${f.path}`);
      if (
        e.files.some(
          (other) => other !== f && f.path.startsWith(other.path + '/'),
        )
      )
        throw new Error(`Example file/directory collision: ${f.path}`);
      paths.add(f.path);
      sources.add(f.source);
    }
    if (
      !e.files.some((f) => f.path === e.entry && f.role === 'entry') ||
      e.files.filter((f) => f.role === 'entry').length !== 1
    )
      throw new Error(`Example requires one entrypoint: ${e.id}`);
    if (!e.files.some((f) => f.source === 'LICENSE.txt' && f.role === 'notice'))
      throw new Error(`Missing example license: ${e.id}`);
    const targets = new Set();
    for (const edge of e.edges) {
      if (
        !paths.has(edge.fromPath) ||
        !paths.has(edge.toPath) ||
        path.posix.join(path.posix.dirname(edge.fromPath), edge.reference) !==
          edge.toPath
      )
        throw new Error(`Incomplete dependency edge: ${e.id}`);
      targets.add(edge.toPath);
    }
    if (e.files.some((f) => f.role === 'companion' && !targets.has(f.path)))
      throw new Error(`Undeclared companion: ${e.id}`);
  }
  return examples;
}

export async function publishExamples(
  input,
  {
    root,
    stage,
    entries,
    tracked,
    supplemental,
    facts,
    sourceUrl,
    isProduction,
  },
) {
  const examples = validateExampleSelection(input, entries, tracked);
  const output = [];
  for (const e of examples) {
    const page = entries.find((p) => p.id === e.tutorial);
    const files = [];
    for (const f of e.files) {
      if (
        supplemental.find((s) => s.source === f.source)?.disposition !==
        'example-download'
      )
        throw new Error(`Missing exact download approval: ${f.source}`);
      const bytes = await fs.readFile(await containedFile(root, f.source));
      const provenance = facts.source(f.source, bytes);
      if (isProduction && provenance.sourceModified)
        throw new Error(
          `Production requires committed example bytes: ${f.source}`,
        );
      const href = downloadHref(e.id, f.path);
      const target = path.join(stage, 'public', href);
      await fs.mkdir(path.dirname(target), { recursive: true });
      await fs.writeFile(target, bytes, { flag: 'wx' });
      files.push({
        ...f,
        href,
        sha256: digest(bytes),
        size: bytes.length,
        content: sourceText(bytes),
        sourceUrl: sourceUrl(f.source),
        ...provenance,
        bytes,
      });
    }
    let bundle;
    if (files.length) {
      const bytes = sourceArchive(e.id, files),
        href = `/downloads/${e.id}.tar`;
      await fs.writeFile(path.join(stage, 'public', href), bytes, {
        flag: 'wx',
      });
      bundle = {
        href,
        filename: `${e.id}.tar`,
        sha256: digest(bytes),
        size: bytes.length,
      };
    }
    output.push({
      ...e,
      files: files.map(({ bytes, ...file }) => file),
      bundle,
      tutorialTitle: page.title,
      tutorialRoute: page.route,
      illustrationUrl: e.illustrationSource
        ? sourceUrl(e.illustrationSource)
        : undefined,
      verification: await effectiveVerification(
        e.verification,
        e.files.map((f) => f.source),
        { root, tracked, revision: facts.revision },
      ),
    });
  }
  return output;
}

export async function tutorialCurriculum(input, examples, entries, context) {
  const tutorials = tutorialSchema.parse(input);
  if (
    tutorials.length !== 10 ||
    tutorials.some((t, i) => t.id !== `T${String(i + 1).padStart(2, '0')}`) ||
    new Set(tutorials.map((t) => t.page)).size !== 10
  )
    throw new Error(
      'Tutorial curriculum must own T01–T10 exactly once in order',
    );
  const pages = tutorials.map((t) => {
    const page = entries.find((e) => e.id === t.page);
    if (!page || !rendered(page) || !page.search || page.kind !== 'tutorial')
      throw new Error(`Unpublished tutorial: ${t.id}`);
    return page;
  });
  return Promise.all(
    tutorials.map(async (t, index) => {
      const page = pages[index];
      const selected = t.examples.map((id) => {
        const example = examples.find((e) => e.id === id);
        if (!example)
          throw new Error(`Missing tutorial example: ${t.id}/${id}`);
        return example;
      });
      return {
        ...t,
        title: page.title,
        route: page.route,
        source: page.source,
        previous: index
          ? { route: pages[index - 1].route, title: pages[index - 1].title }
          : null,
        next:
          index < 9
            ? { route: pages[index + 1].route, title: pages[index + 1].title }
            : {
                route: '/docs/examples/',
                title: 'Explore the example catalog',
              },
        verification: await effectiveVerification(
          t.verification,
          [
            page.source,
            ...selected.flatMap((e) => e.files.map((f) => f.source)),
          ],
          context,
        ),
      };
    }),
  );
}
