import { rendered } from './content-lib.mjs';

// Generated inside the content transaction, never copied into public/.
// Editorial review and live verification are separate from successful rendering.
export function migrationReport(entries, content) {
  const pages = new Map(content.pages.map((page) => [page.id, page]));
  const sources = entries.map((entry) => {
    const page = pages.get(entry.id);
    return {
      id: entry.id,
      source: entry.source,
      disposition: entry.disposition,
      title: entry.title,
      route: entry.route ?? null,
      section: entry.section ?? null,
      reviewNote: entry.reviewNote ?? 'No editorial review note recorded.',
      provenance: entry.provenance,
      generatedBy: entry.generatedBy ?? null,
      status: entry.status ?? null,
      navigation: entry.navigation ?? false,
      search: entry.search ?? false,
      sitemap: entry.sitemap ?? false,
      noindex: rendered(entry) ? entry.noindex : null,
      sha256: page?.sha256 ?? null,
      sourceModified: page?.sourceModified ?? null,
      effectiveVerification: page?.effectiveVerification ?? 'not-checked',
      bridgeMappings: page?.bridgeMappings ?? [],
    };
  });
  const report = {
    schemaVersion: 1,
    revision: content.revision,
    scope:
      'Manifest inventory and generated content; HTML graph checks are a separate build gate. Publication is not proof of runtime or live-provider verification.',
    total: entries.length,
    rendered: content.pages.length,
    counts: content.counts,
    supplemental: content.supplemental ?? [],
    capabilities: content.capabilities ?? [],
    tutorialVerification: (content.tutorials ?? []).map((t) => ({
      id: t.id,
      source: t.source,
      route: t.route,
      state: t.verification.state,
      scope: t.verification.scope,
    })),
    exampleDownloads: (content.examples ?? []).map((e) => ({
      id: e.id,
      kind: e.kind,
      files: e.files.map((f) => ({
        source: f.source,
        href: f.href,
        sha256: f.sha256,
      })),
      bundle: e.bundle ?? null,
    })),
    compatibilityPolicy: {
      bridges:
        'Rendered with preserved source heading anchors; excluded from sidebar, search, and sitemap, with noindex.',
      aliases:
        'Route and fragment alias declarations are rejected until an adapter exists. Existing canonical routes are retained.',
      repositoryTargets:
        'Unpublished tracked destinations link to the Git revision on GitHub. They have no website routes.',
    },
    sourceCorrections: sources
      .filter((source) => source.sourceModified)
      .map(({ source, sha256 }) => ({ source, sha256 })),
    sourceCorrectionNotes:
      'Local modified published sources are listed above; the Git diff describes the changes. Editorial reasons and unchanged deferred-content issues are recorded in planning/p05-completion-review.md and per-source review notes. This list is not a cumulative change history.',
    languageFallbacks: content.languageFallbacks,
    sources,
  };
  const cell = (value) =>
    String(value ?? '—')
      .replaceAll('|', '\\|')
      .replace(/\r?\n/g, ' ');
  const table = (selected) =>
    [
      '| Source | Disposition | Route | Review note |',
      '| --- | --- | --- | --- |',
      ...selected.map(
        (source) =>
          `| ${[source.source, source.disposition, source.route, source.reviewNote].map(cell).join(' | ')} |`,
      ),
    ].join('\n');
  const markdown =
    [
      '# Documentation migration inventory',
      `Revision: ${report.revision}`,
      report.scope,
      `${report.total} sources accounted for; ${report.rendered} rendered.`,
      ...Object.entries(report.counts).map(
        ([disposition, count]) => `- ${disposition}: ${count}`,
      ),
      '## Tutorials and source downloads',
      `${report.tutorialVerification.length} curriculum records; ${report.exampleDownloads.filter((e) => e.bundle).length} approved source bundles. Exact bytes, dependency layouts, source provenance, and scoped verification are recorded in examples-report.json.`,
      ...report.tutorialVerification.map(
        (t) => `- ${t.id}: ${t.route} — ${t.state}. ${t.scope}`,
      ),
      '## Published and compatibility sources',
      table(sources.filter((source) => rendered(source))),
      '## Deferred and repository-only sources',
      table(sources.filter((source) => !rendered(source))),
      '## Compatibility rules',
      ...Object.values(report.compatibilityPolicy).map(
        (policy) => `- ${policy}`,
      ),
      ...sources
        .filter((s) => s.disposition === 'bridge')
        .flatMap((s) =>
          s.bridgeMappings.map(
            (m) => `- ${s.route}#${m.fragment} → ${m.target}`,
          ),
        ),
      '## Capability coverage',
      ...report.capabilities.map(
        (c) =>
          `- ${c.title}: ${c.pages.map((p) => p.route).join(', ')} — ${c.qualification}${c.deferred.length ? ` Repository/deferred detail: ${c.deferred.join(', ')}.` : ''}`,
      ),
      '## Supplemental sources (never automatic documents or downloads)',
      '| Source | Disposition | Rule | Reason |',
      '| --- | --- | --- | --- |',
      ...report.supplemental.map(
        (s) =>
          `| ${[s.source, s.disposition, s.rule, s.reason].map(cell).join(' | ')} |`,
      ),
      '## Local source corrections',
      report.sourceCorrectionNotes,
      ...(report.sourceCorrections.length
        ? report.sourceCorrections.map(
            ({ source, sha256 }) => `- ${source} — SHA-256 ${sha256}`,
          )
        : ['No modified published source bytes in this snapshot.']),
      '## Highlighting fallbacks',
      ...(report.languageFallbacks.length
        ? report.languageFallbacks.map(
            ({ source, language, highlightAs }) =>
              `- ${source}: ${language} → ${highlightAs}`,
          )
        : ['None.']),
    ].join('\n\n') + '\n';
  return { report, markdown };
}
