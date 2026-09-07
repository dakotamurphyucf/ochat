import { deploymentHeaders } from './deployment-policy.mjs';
import { applicationReport } from './applications.mjs';
import fs from 'node:fs/promises';
import { publishingAssets } from './publishing-assets.mjs';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { stringify } from 'yaml';
import { bundledLanguages } from 'shiki';
import {
  repoRoot,
  repository,
  branch,
  production,
  origin,
} from '../config/site.mjs';
import {
  validateManifest,
  rendered,
  containedFile,
  transformMarkdown,
  parser,
  encodePath,
  bridgeMappings,
} from './content-lib.mjs';
import { publishSnapshot } from './snapshot.mjs';
import { gitFacts, digest } from './provenance.mjs';
import { migrationReport } from './migration-report.mjs';
import {
  publishExamples,
  tutorialCurriculum,
  validateExampleSelection,
  downloadHref,
} from './examples.mjs';
import { buildSidebar, documentationPaths } from '../config/navigation.mjs';
import {
  supplementalInventory,
  capabilityCoverage,
} from './publication-policy.mjs';
export const websiteRoot = fileURLToPath(new URL('../', import.meta.url));
export const languageAliases = {
  chatml: 'ocaml',
  console: 'shellsession',
  mermaid: 'text',
};
export function pageMetadata(
  entry,
  { isProduction = false, sourceUrl, provenance, byId, tutorials = [] },
) {
  const neighbor = (id) => {
    const e = byId.get(id);
    if (!e || !rendered(e))
      throw new Error(`Unpublished related page ${id} from ${entry.source}`);
    return { link: e.route, label: e.title };
  };
  const related = entry.related || [];
  const lesson = tutorials.findIndex((t) => t.page === entry.id);
  return {
    title: entry.title,
    description: entry.description,
    slug: entry.route.slice(1, -1),
    editUrl:
      provenance.sourceCommit === null ? false : sourceUrl(entry.source, true),
    sidebar: { hidden: !entry.navigation },
    pagefind: entry.search,
    lastUpdated: provenance.lastUpdated
      ? new Date(provenance.lastUpdated)
      : false,
    prev: lesson > 0 ? neighbor(tutorials[lesson - 1].page) : false,
    next:
      lesson >= 0 && lesson < tutorials.length - 1
        ? neighbor(tutorials[lesson + 1].page)
        : related.length
          ? neighbor(related[0])
          : false,
    head:
      !isProduction || entry.noindex
        ? [
            {
              tag: 'meta',
              attrs: { name: 'robots', content: 'noindex, nofollow' },
            },
          ]
        : [],
  };
}
export async function copyPublic(source, destination) {
  if ((await fs.lstat(source)).isSymbolicLink())
    throw new Error(`Public asset directory must not be a symlink: ${source}`);
  await fs.mkdir(destination, { recursive: true });
  for (const item of await fs.readdir(source, { withFileTypes: true })) {
    if (item.isSymbolicLink())
      throw new Error(`Public asset must not be a symlink: ${item.name}`);
    const from = path.join(source, item.name),
      to = path.join(destination, item.name);
    if (item.isDirectory()) await copyPublic(from, to);
    else if (item.isFile()) await fs.copyFile(from, to);
  }
}
export async function generate({
  root = repoRoot,
  siteRoot = websiteRoot,
  isProduction = production,
  siteOrigin = origin,
  checkpoint,
} = {}) {
  return publishSnapshot(
    siteRoot,
    async (stage) => {
      const entries = JSON.parse(
        await fs.readFile(
          path.join(siteRoot, 'config/docs-manifest.json'),
          'utf8',
        ),
      );
      const assets = JSON.parse(
        await fs.readFile(path.join(siteRoot, 'config/assets.json'), 'utf8'),
      );
      const tracked = new Set(
        execFileSync('git', ['ls-files', '-z'], { cwd: root, encoding: 'utf8' })
          .split('\0')
          .filter(Boolean),
      );
      validateManifest(entries, tracked);
      const exampleInput = validateExampleSelection(
        JSON.parse(
          await fs.readFile(
            path.join(root, 'docs-src/examples/catalog.json'),
            'utf8',
          ),
        ),
        entries,
        tracked,
      );
      const tutorialInput = JSON.parse(
        await fs.readFile(path.join(siteRoot, 'config/tutorials.json'), 'utf8'),
      );
      const supplemental = supplementalInventory(
        JSON.parse(
          await fs.readFile(
            path.join(siteRoot, 'config/supplemental-sources.json'),
            'utf8',
          ),
        ),
        tracked,
        assets,
        exampleInput,
      );
      const capabilities = capabilityCoverage(
        JSON.parse(
          await fs.readFile(
            path.join(siteRoot, 'config/capabilities.json'),
            'utf8',
          ),
        ),
        entries,
      );
      buildSidebar(entries);
      documentationPaths(entries);
      const facts = gitFacts(root);
      const sourceUrl = (source, edit = false) =>
        `${repository}/${edit ? 'edit' : 'blob'}/${edit ? branch : facts.revision}/${encodePath(source)}`;
      const context = {
        tracked,
        assets,
        sourceUrl,
        bySource: new Map(entries.map((e) => [e.source, e])),
        supplemental: new Map(supplemental.map((e) => [e.source, e])),
        downloads: new Map(
          exampleInput.flatMap((e) =>
            e.files
              .filter((f) => f.role !== 'notice')
              .map((f) => [f.source, downloadHref(e.id, f.path)]),
          ),
        ),
      };
      const byId = new Map(entries.map((e) => [e.id, e]));
      for (const e of entries) await containedFile(root, e.source);
      const rendererInputs = [
        'astro.config.mjs',
        'config/search.mjs',
        'config/presentation.mjs',
        'config/media.json',
        'scripts/publishing-assets.mjs',
        'scripts/search-index.mjs',
        'package-lock.json',
        'scripts/code-accessibility.mjs',
        'scripts/rehype-reading.mjs',
      ];
      const renderFingerprint = digest(
        (
          await Promise.all(
            rendererInputs.map((file) =>
              fs.readFile(path.join(siteRoot, file), 'utf8'),
            ),
          )
        ).join('\n'),
      );
      const pages = [],
        languageFallbacks = [];
      for (const e of entries.filter(rendered)) {
        const text = await fs.readFile(
          await containedFile(root, e.source),
          'utf8',
        );
        const provenance = facts.source(e.source, text);
        if (isProduction && provenance.sourceModified)
          throw new Error(
            `Production requires committed source bytes: ${e.source}`,
          );
        if (e.sourceCommit && e.sourceCommit !== provenance.sourceCommit)
          throw new Error(`Stale sourceCommit in manifest: ${e.source}`);
        const result = transformMarkdown(text, e, context);
        const metadata = pageMetadata(e, {
          isProduction,
          sourceUrl,
          provenance,
          byId,
          tutorials: tutorialInput,
        });
        const content = `---\n${stringify(metadata)}---\n\n<!-- Renderer: ${renderFingerprint} -->\n\n${result.body}`;
        const file = path.join(stage, 'docs', e.route.slice(1), 'index.md');
        await fs.mkdir(path.dirname(file), { recursive: true });
        await fs.writeFile(file, content);
        for (const fence of result.fences) {
          if (
            languageAliases[fence.lang] ||
            (!bundledLanguages[fence.lang] &&
              !['text', 'txt', 'plaintext'].includes(fence.lang))
          )
            languageFallbacks.push({
              source: e.source,
              language: fence.lang,
              highlightAs: languageAliases[fence.lang] || 'text',
            });
        }
        pages.push({
          ...e,
          ...provenance,
          sha256: digest(text),
          headings: result.headings,
          bridgeMappings: bridgeMappings(text, e, context),
          diagrams: result.fences
            .filter((f) => f.lang === 'mermaid')
            .map((f) => f.value),
          fences: result.fences.map((f) => f.lang),
          effectiveVerification:
            !provenance.sourceModified && e.verifiedCommit === facts.revision
              ? e.verification
              : 'not-checked',
        });
      }
      await copyPublic(
        path.join(siteRoot, 'public'),
        path.join(stage, 'public'),
      );
      await publishingAssets(stage, pages);
      const examples = await publishExamples(exampleInput, {
        root,
        stage,
        entries,
        tracked,
        supplemental,
        facts,
        sourceUrl,
        isProduction,
      });
      await fs.writeFile(
        path.join(stage, 'applications-report.json'),
        JSON.stringify(
          await applicationReport({
            root,
            siteRoot,
            entries,
            examples,
            facts,
            isProduction,
          }),
          null,
          2,
        ) + '\n',
      );
      const tutorials = await tutorialCurriculum(
        tutorialInput,
        examples,
        entries,
        { root, tracked, revision: facts.revision },
      );
      for (const tutorial of tutorials) {
        const page = pages.find((p) => p.id === tutorial.page);
        page.effectiveVerification = tutorial.verification.state;
        page.verificationEvidence = `tutorial:${tutorial.id}`;
      }
      await fs.writeFile(
        path.join(stage, 'examples-report.json'),
        JSON.stringify(
          { revision: facts.revision, examples, tutorials },
          null,
          2,
        ) + '\n',
      );
      const destinations = new Set();
      for (const [source, target] of Object.entries(assets)) {
        if (
          !tracked.has(source) ||
          !/^\/media\/[a-z0-9/_.-]+$/.test(target) ||
          target.includes('..') ||
          destinations.has(target)
        )
          throw new Error(`Invalid or duplicate asset mapping: ${source}`);
        destinations.add(target);
        const file = path.join(stage, 'public', target);
        try {
          await fs.lstat(file);
          throw new Error(
            `Asset collides with authored public file: ${target}`,
          );
        } catch (e) {
          if (e.code !== 'ENOENT') throw e;
        }
        await fs.mkdir(path.dirname(file), { recursive: true });
        await fs.copyFile(await containedFile(root, source), file);
      }
      await fs.writeFile(
        path.join(stage, 'public/_headers'),
        deploymentHeaders(isProduction),
      );
      const readme = await fs.readFile(
        await containedFile(root, 'Readme.md'),
        'utf8',
      );
      const example = parser
        .parse(readme)
        .children.find((n) => n.type === 'code' && n.lang === 'xml')?.value;
      if (!example?.includes('<developer>') || !example.includes('read_file'))
        throw new Error('Homepage README example requires review');
      if (isProduction && facts.source('Readme.md', readme).sourceModified)
        throw new Error('Production requires committed homepage example');
      await fs.writeFile(
        path.join(stage, 'hero.json'),
        JSON.stringify({
          code: example,
          source: 'Readme.md',
          sha256: digest(readme),
          ...facts.source('Readme.md', readme),
        }) + '\n',
      );
      const counts = Object.fromEntries(
        [
          'publish',
          'bridge',
          'compatibility',
          'repository-only',
          'deferred',
        ].map((d) => [d, entries.filter((e) => e.disposition === d).length]),
      );
      const report = {
        renderFingerprint,
        revision: facts.revision,
        shallow: facts.shallow,
        origin: siteOrigin,
        environment: isProduction ? 'production' : 'preview',
        counts,
        supplemental,
        capabilities,
        examples,
        tutorials,
        assets: Object.keys(assets),
        languageFallbacks,
        pages,
      };
      await fs.writeFile(
        path.join(stage, 'content-report.json'),
        JSON.stringify(report, null, 2) + '\n',
      );
      const migration = migrationReport(entries, report);
      await fs.writeFile(
        path.join(stage, 'migration-report.json'),
        JSON.stringify(migration.report, null, 2) + '\n',
      );
      await fs.writeFile(
        path.join(stage, 'migration-report.md'),
        migration.markdown,
      );
      console.log(
        `Content: ${pages.length} rendered / ${entries.length} accounted for; ${counts.deferred} deferred.`,
      );
      return report;
    },
    { checkpoint },
  );
}
if (
  process.argv[1] &&
  path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)
)
  await generate();
