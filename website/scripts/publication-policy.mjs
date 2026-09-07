import path from 'node:path';
import { z } from 'zod';
import { rendered } from './content-lib.mjs';

const ruleSchema = z
  .object({
    source: z.string().min(1),
    disposition: z.enum([
      'repository-only',
      'example-review',
      'hero-source',
      'media',
      'example-download',
    ]),
    reason: z.string().min(20),
  })
  .strict();
const schema = z
  .object({
    version: z.literal(1),
    files: z.array(ruleSchema),
    directories: z.array(
      ruleSchema.extend({
        disposition: z.enum(['repository-only', 'example-review']),
      }),
    ),
  })
  .strict();

// Directory rules authorize repository links only. Copies always require an
// exact-file media/download rule plus the separate destination allowlist.
export function supplementalInventory(input, tracked, assets, examples = []) {
  const policy = schema.parse(input);
  const seen = new Set();
  for (const rule of [...policy.files, ...policy.directories]) {
    if (
      seen.has(rule.source) ||
      path.posix.normalize(rule.source) !== rule.source ||
      rule.source.startsWith('/') ||
      rule.source.startsWith('../') ||
      rule.source.includes('\\')
    )
      throw new Error(`Invalid or duplicate supplemental rule: ${rule.source}`);
    seen.add(rule.source);
  }
  for (const file of policy.files) {
    if (!tracked.has(file.source))
      throw new Error(`Untracked supplemental file: ${file.source}`);
    if (file.source.startsWith('docs-src/') && file.source.endsWith('.md'))
      throw new Error(
        `Canonical docs belong to the docs manifest: ${file.source}`,
      );
  }
  for (const directory of policy.directories) {
    if (
      !directory.source.endsWith('/') ||
      ![...tracked].some((s) => s.startsWith(directory.source))
    )
      throw new Error(`Missing supplemental directory: ${directory.source}`);
    if (
      policy.directories.some(
        (other) =>
          other !== directory && directory.source.startsWith(other.source),
      )
    )
      throw new Error(
        `Overlapping supplemental directories: ${directory.source}`,
      );
  }
  const sources = [];
  for (const source of [...tracked].sort()) {
    if (source.startsWith('docs-src/') && source.endsWith('.md')) continue;
    const rule =
      policy.files.find((r) => r.source === source) ||
      policy.directories.find((r) => source.startsWith(r.source));
    const required =
      (!source.includes('/') && source.endsWith('.md')) ||
      source.startsWith('prompt-examples/') ||
      source.startsWith('real-world-example-session/') ||
      source.startsWith('assets/') ||
      (source.startsWith('docs-src/') && !source.endsWith('.md'));
    if (!rule && required)
      throw new Error(`Missing supplemental disposition: ${source}`);
    if (rule)
      sources.push({
        source,
        disposition: rule.disposition,
        reason: rule.reason,
        rule: rule.source,
      });
  }
  const bySource = new Map(sources.map((source) => [source.source, source]));
  const downloads = new Set(
    examples.flatMap((e) => e.files.map((f) => f.source)),
  );
  for (const source of downloads)
    if (bySource.get(source)?.disposition !== 'example-download')
      throw new Error(`Missing exact example download approval: ${source}`);
  for (const source of sources)
    if (
      source.disposition === 'example-download' &&
      !downloads.has(source.source)
    )
      throw new Error(
        `Supplemental download has no example destination: ${source.source}`,
      );
  if (bySource.get('Readme.md')?.disposition !== 'hero-source')
    throw new Error('Readme.md must own the selected homepage example');
  for (const source of Object.keys(assets)) {
    if (bySource.get(source)?.disposition !== 'media')
      throw new Error(
        `Copied asset requires exact supplemental media approval: ${source}`,
      );
  }
  for (const source of sources) {
    if (source.disposition === 'media' && !assets[source.source])
      throw new Error(
        `Supplemental media has no destination: ${source.source}`,
      );
  }
  return sources;
}

const capabilitySchema = z.array(
  z
    .object({
      id: z.string().regex(/^[a-z0-9-]+$/),
      title: z.string().min(1),
      pages: z.array(z.string()).min(1),
      qualification: z.string().min(20),
      benefit: z.string().min(20),
      example: z.string().min(20),
      deferred: z.array(z.string()).default([]),
    })
    .strict(),
);
export function capabilityCoverage(input, entries) {
  const capabilities = capabilitySchema.parse(input);
  const ids = new Set();
  const byId = new Map(entries.map((entry) => [entry.id, entry]));
  return capabilities.map((capability) => {
    if (ids.has(capability.id))
      throw new Error(`Duplicate capability: ${capability.id}`);
    ids.add(capability.id);
    const pages = capability.pages.map((id) => {
      const page = byId.get(id);
      if (
        !page ||
        !rendered(page) ||
        page.disposition === 'bridge' ||
        !page.search
      )
        throw new Error(
          `Capability needs a searchable reader destination: ${capability.id} / ${id}`,
        );
      return { id, title: page.title, route: page.route };
    });
    for (const id of capability.deferred) {
      const page = byId.get(id);
      if (!page || rendered(page) || !page.reviewNote)
        throw new Error(
          `Capability deferral needs an unpublished source and reason: ${id}`,
        );
    }
    return { ...capability, pages };
  });
}
