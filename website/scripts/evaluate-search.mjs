import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from 'parse5';
import { chromium } from 'playwright-core';
import { preview } from 'astro';
const site = fileURLToPath(new URL('../', import.meta.url));
const read = async (name) =>
  JSON.parse(await fs.readFile(path.join(site, name), 'utf8'));
const queries = await read('config/search-queries.json');
const manifest = await read('config/docs-manifest.json');
const artifact = await read('.generated/build-evidence.json');
const indexing = await read('.generated/search-index-report.json');
const failures = [];
let server, browser;
try {
  // Own an ephemeral local preview, independent of an already-running dev server.
  server = await preview({
    root: site,
    server: { host: '127.0.0.1', port: 0 },
  });
  const address = server.server.address();
  const base = `http://127.0.0.1:${address.port}`;
  browser = await chromium.launch();
  const page = await browser.newPage();
  await page.goto(base + '/docs/');
  const measured = await page.evaluate(async (queries) => {
    const engine = await import('/pagefind/pagefind.js');
    await engine.options({ excerptLength: 28 });
    const all = await engine.search(null);
    const indexed = await Promise.all(all.results.map((r) => r.data()));
    const results = [];
    for (const record of queries) {
      const start = performance.now();
      const found = await engine.search(record.query);
      const data = await Promise.all(
        found.results.slice(0, 5).map((r) => r.data()),
      );
      results.push({
        ...record,
        count: found.results.length,
        warmMilliseconds: performance.now() - start,
        top: data.map((r) => ({
          url: r.url,
          title: r.meta.title,
          section: r.meta.section,
          status: r.meta.status,
          excerpt: r.plain_excerpt,
          headings: r.sub_results.map((s) => ({ url: s.url, title: s.title })),
        })),
      });
    }
    return { indexed, results };
  }, queries);
  const expected = manifest
    .filter((e) => e.search)
    .map((e) => e.route)
    .sort();
  const actual = measured.indexed.map((e) => e.url).sort();
  if (JSON.stringify(expected) !== JSON.stringify(actual))
    failures.push(
      'Actual index does not exactly match manifest search ownership.',
    );
  if (
    indexing.passes !== 1 ||
    indexing.stage !== 'astro:build:done' ||
    indexing.htmlFilesProcessed !== artifact.htmlPages
  )
    failures.push('Index pass or final HTML inventory mismatch.');
  const ids = new Map();
  for (const route of expected) {
    const tree = parse(
      await fs.readFile(path.join(site, 'dist', route, 'index.html'), 'utf8'),
    );
    const found = new Set();
    const walk = (node) => {
      for (const a of node.attrs || [])
        if (a.name === 'id' || (node.tagName === 'a' && a.name === 'name'))
          found.add(a.value);
      for (const child of node.childNodes || []) walk(child);
    };
    walk(tree);
    ids.set(route, found);
  }
  let anchorsChecked = 0;
  const checkUrl = (href) => {
    const url = new URL(href, base);
    if (
      url.origin !== base ||
      !ids.has(url.pathname) ||
      url.search ||
      (url.hash &&
        !ids.get(url.pathname).has(decodeURIComponent(url.hash.slice(1))))
    )
      failures.push(`Invalid indexed result/heading: ${href}`);
    if (url.hash) anchorsChecked++;
  };
  for (const row of measured.indexed) {
    checkUrl(row.url);
    for (const anchor of row.anchors)
      checkUrl(row.url + '#' + encodeURIComponent(anchor.id));
    const owner = manifest.find((e) => e.route === row.url);
    if (!owner) continue;
    const status =
      owner.disposition === 'compatibility'
        ? 'Compatibility'
        : owner.status === 'experimental'
          ? 'Experimental'
          : 'Current';
    if (row.meta.section !== owner.section || row.meta.status !== status)
      failures.push(`Missing result context: ${row.url}`);
    for (const chrome of [
      'View committed Markdown source',
      'View Markdown source ↗',
      'Base revision',
      'Checks apply to the recorded source hashes',
      'Read the source here. Expand a filename',
    ])
      if (row.content.includes(chrome))
        failures.push(`Index contains excluded chrome: ${row.url}: ${chrome}`);
  }
  for (const row of measured.results) {
    row.rank = row.top.findIndex((r) => row.expected.includes(r.url)) + 1;
    row.passed = row.rank > 0;
    row.excerptPresent = row.passed && !!row.top[row.rank - 1].excerpt.trim();
    for (const result of row.top) {
      checkUrl(result.url);
      for (const sub of result.headings) checkUrl(sub.url);
    }
    if (row.passed && !row.excerptPresent)
      failures.push(`Missing useful excerpt: ${row.query}`);
  }
  const passed = measured.results.filter((r) => r.passed).length;
  if (passed / queries.length < 0.9)
    failures.push(`Top-five benchmark below 90%: ${passed}/${queries.length}`);
  for (const query of ['install', 'first agent', 'MCP', 'save session']) {
    const row = measured.results.find((r) => r.query === query);
    const legacy = row.top.findIndex((r) => r.status === 'Compatibility');
    if (!row.passed || (legacy >= 0 && legacy < row.rank - 1))
      failures.push(
        `Compatibility outranks intended current entry for ${query}`,
      );
  }
  const report = {
    artifactSha256: artifact.sha256,
    environment: artifact.environment,
    indexing,
    indexedPages: actual.length,
    expectedPages: expected.length,
    indexedRoutes: actual,
    anchorsChecked,
    benchmark: {
      passed,
      total: queries.length,
      ratio: passed / queries.length,
      minimum: 0.9,
    },
    queries: measured.results,
    failures,
    result: failures.length ? 'fail' : 'pass',
  };
  await fs.writeFile(
    path.join(site, '.generated/search-report.json'),
    JSON.stringify(report, null, 2) + '\n',
  );
  const rows = report.queries.map(
    (r) =>
      `| ${r.query.replaceAll('|', '\\|')} | ${r.rank || 'miss'} | ${r.rank ? r.top[r.rank - 1].url : '—'} | ${r.excerptPresent ? 'present' : 'missing'} |`,
  );
  await fs.writeFile(
    path.join(site, '.generated/search-report.md'),
    `# Search evaluation\n\nArtifact: ${artifact.sha256}. ${passed}/${queries.length} expected families in the top five. ${actual.length} indexed pages; ${anchorsChecked} fragment destinations checked.\n\n| Query | Rank | Expected destination found | Excerpt |\n|---|---:|---|---|\n${rows.join('\n')}\n\n${failures.length ? failures.join('\n') : 'All automated search gates passed.'}\n`,
  );
  console.log(
    `${passed}/${queries.length} queries in top five; ${actual.length} indexed pages; ${anchorsChecked} anchors checked.`,
  );
  if (failures.length) throw new Error(failures.join('\n'));
} finally {
  await browser?.close();
  await server?.stop();
}
