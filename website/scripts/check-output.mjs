import { inspectCapacity } from './deployment-policy.mjs';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from 'parse5';
import { createHash } from 'node:crypto';
import { socialPath, sitemapIncluded } from '../config/presentation.mjs';
import { origin, production, sourceUrl } from '../config/site.mjs';
import {
  apiReference,
  assertApiReferenceExcluded,
} from '../config/api-reference.mjs';
const root = fileURLToPath(new URL('../dist/', import.meta.url));
const report = JSON.parse(
  await fs.readFile(
    new URL('../.generated/content-report.json', import.meta.url),
    'utf8',
  ),
);
async function walk(dir) {
  const out = [];
  for (const e of await fs.readdir(dir, { withFileTypes: true })) {
    const p = path.join(dir, e.name);
    if (e.isSymbolicLink()) throw new Error(`Unexpected output symlink: ${p}`);
    if (e.isDirectory()) out.push(...(await walk(p)));
    else out.push(p);
  }
  return out;
}
export function inspect(html) {
  const tree = parse(html),
    ids = new Set(),
    duplicateIds = [],
    links = [],
    meta = {},
    nodes = [];
  function walk(n) {
    const attrs = Object.fromEntries(
      (n.attrs || []).map((a) => [a.name, a.value]),
    );
    if (attrs.id) {
      if (ids.has(attrs.id)) duplicateIds.push(attrs.id);
      ids.add(attrs.id);
    }
    if (n.tagName === 'a' && attrs.name) ids.add(attrs.name);
    if (n.tagName === 'meta')
      meta[attrs.name || attrs.property] = attrs.content;
    if (n.tagName) nodes.push({ tag: n.tagName, attrs });
    for (const a of ['href', 'src']) if (attrs[a]) links.push(attrs[a]);
    for (const child of n.childNodes || []) walk(child);
  }
  walk(tree);
  return { ids, duplicateIds, links, meta, nodes };
}
const files = await walk(root),
  byPath = new Map(),
  failures = [];
assertApiReferenceExcluded({ files: files.map((f) => path.relative(root, f)) });
for (const f of files.filter((f) => f.endsWith('.html')))
  byPath.set(f, inspect(await fs.readFile(f, 'utf8')));
for (const [file, page] of byPath) {
  const route = '/' + path.relative(root, file).replace(/index\.html$/, '');
  assertApiReferenceExcluded({
    urls: page.links.map((href) => new URL(href, origin + route).href),
    origin,
  });
  if (page.duplicateIds.length)
    failures.push(`${route}: duplicate IDs ${page.duplicateIds.join(',')}`);
  if (page.nodes.filter((n) => n.tag === 'h1').length !== 1)
    failures.push(`${route}: expected one h1`);
  if (!production && page.meta.robots !== 'noindex, nofollow')
    failures.push(`${route}: preview must be noindex`);
  if (page.nodes.some((n) => n.tag === 'pre' && n.attrs.tabindex !== '0'))
    failures.push(`${route}: code must be keyboard scrollable`);
  for (const href of page.links) {
    if (/^(mailto:|tel:|data:)/.test(href)) continue;
    let url;
    try {
      url = new URL(href, origin + route);
    } catch {
      failures.push(`${route}: invalid link ${href}`);
      continue;
    }
    if (url.origin !== origin) continue;
    let pathname;
    try {
      pathname = decodeURIComponent(url.pathname);
    } catch {
      failures.push(`${route}: bad encoding ${href}`);
      continue;
    }
    let dest = path.join(root, pathname);
    if (pathname.endsWith('/')) dest = path.join(dest, 'index.html');
    if (!files.includes(dest)) {
      failures.push(`${route}: missing local target ${href}`);
      continue;
    }
    if (url.hash && byPath.has(dest)) {
      let fragment;
      try {
        fragment = decodeURIComponent(url.hash.slice(1));
      } catch {
        fragment = url.hash.slice(1);
      }
      if (!byPath.get(dest).ids.has(fragment))
        failures.push(`${route}: missing fragment ${href}`);
    }
  }
}
const sitemap = (
  await Promise.all(
    files
      .filter((f) => /sitemap-\d+\.xml$/.test(f))
      .map((f) => fs.readFile(f, 'utf8')),
  )
).join('');
assertApiReferenceExcluded({
  urls: [...sitemap.matchAll(/<loc>([^<]+)<\/loc>/g)].map((m) => m[1]),
  origin,
});
for (const e of report.pages) {
  const page = byPath.get(path.join(root, e.route, 'index.html'));
  if (!page) {
    failures.push(`Missing manifest route: ${e.route}`);
    continue;
  }
  for (const heading of e.headings) {
    if (!page.ids.has(heading))
      failures.push(`Lost source heading: ${e.route}#${heading}`);
  }
  for (const mapping of e.bridgeMappings || []) {
    if (!page.links.includes(mapping.target))
      failures.push(`Lost bridge destination: ${e.route}#${mapping.fragment}`);
  }
  if (
    e.sourceCommit === null &&
    (!page.nodes.some((n) => n.attrs['data-source-pending'] === e.source) ||
      page.links.includes(sourceUrl(e.source)) ||
      page.links.includes(sourceUrl(e.source, true)))
  )
    failures.push(
      `New local source must not invent a committed link: ${e.route}`,
    );
  if (
    e.sourceCommit !== null &&
    (!page.links.includes(sourceUrl(e.source, true)) ||
      !page.links.includes(sourceUrl(e.source)))
  )
    failures.push(`Source attribution mismatch: ${e.route}`);
  const updated = page.nodes.filter(
    (n) => n.tag === 'time' && n.attrs.datetime,
  );
  if (
    e.lastUpdated
      ? !updated.some(
          (n) => n.attrs.datetime === new Date(e.lastUpdated).toISOString(),
        )
      : updated.length > 0
  )
    failures.push(`Source-history date mismatch: ${e.route}`);
  const indexed = page.nodes.some((n) =>
    Object.hasOwn(n.attrs, 'data-pagefind-body'),
  );
  if (indexed !== e.search) failures.push(`Search policy mismatch: ${e.route}`);
  if (
    sitemap.includes(`<loc>${origin}${e.route}</loc>`) !==
    sitemapIncluded(e.route, report.pages, production)
  )
    failures.push(`Sitemap policy mismatch: ${e.route}`);
  if (e.noindex && !page.meta.robots?.includes('noindex'))
    failures.push(`Missing noindex: ${e.route}`);
}
if (!production && files.some((f) => /sitemap.*\.xml$/.test(f)))
  failures.push('Preview must not emit a sitemap');
const titles = new Set();
for (const [file, page] of byPath) {
  const route = '/' + path.relative(root, file).replace(/index\.html$/, '');
  const html = await fs.readFile(file, 'utf8');
  const title = html.match(/<title>(.*?)<\/title>/s)?.[1];
  if (!title || titles.has(title))
    failures.push(`Missing or duplicate title: ${route}`);
  titles.add(title);
  if (!page.meta.description?.trim())
    failures.push(`Missing description: ${route}`);
  const canonicals = page.nodes.filter(
    (n) => n.tag === 'link' && n.attrs.rel === 'canonical',
  );
  if (route === '/404.html') {
    if (canonicals.length || !page.meta.robots?.includes('noindex'))
      failures.push('404 must be noindex without a canonical');
    continue;
  }
  if (canonicals.length !== 1 || canonicals[0].attrs.href !== origin + route)
    failures.push(`Incorrect canonical: ${route}`);
  if (
    page.meta['og:url'] !== origin + route ||
    page.meta['og:image'] !== origin + socialPath(route) ||
    page.meta['twitter:image'] !== page.meta['og:image']
  )
    failures.push(`Social URL mismatch: ${route}`);
  if (
    page.meta['og:image:width'] !== '1200' ||
    page.meta['og:image:height'] !== '630' ||
    !page.meta['og:image:alt'] ||
    page.meta['twitter:card'] !== 'summary_large_image'
  )
    failures.push(`Incomplete social metadata: ${route}`);
  if (!files.includes(path.join(root, socialPath(route))))
    failures.push(`Missing social card: ${route}`);
  const owner = report.pages.find((e) => e.route === route);
  if (production && !owner?.noindex && page.meta.robots?.includes('noindex'))
    failures.push(`Production indexable page remains noindex: ${route}`);
}
const publishing = JSON.parse(
  await fs.readFile(
    new URL('../.generated/publishing-assets.json', import.meta.url),
  ),
);
for (const asset of publishing.assets) {
  const data = await fs.readFile(path.join(root, asset.url));
  if (createHash('sha256').update(data).digest('hex') !== asset.sha256)
    failures.push(`Changed publishing asset: ${asset.url}`);
  if (!files.includes(path.join(root, asset.license)))
    failures.push(`Missing asset notice: ${asset.url}`);
}
const media = {
  revision: report.revision,
  publishing: publishing.assets,
  repository: [],
  fonts: [],
  inline: publishing.policy.inline,
  generators: publishing.policy.generators,
};
const assetMappings = JSON.parse(
  await fs.readFile(new URL('../config/assets.json', import.meta.url)),
);
for (const record of publishing.policy.repository) {
  if (assetMappings[record.source] !== record.url)
    failures.push(`Repository media approval mismatch: ${record.source}`);
  const bytes = await fs.readFile(path.join(root, record.url));
  media.repository.push({
    ...record,
    bytes: bytes.length,
    sha256: createHash('sha256').update(bytes).digest('hex'),
  });
}
if (Object.keys(assetMappings).length !== media.repository.length)
  failures.push('Repository media missing from license inventory');
for (const file of files.filter((f) => /\.woff2?$/.test(f))) {
  const relative = path.relative(root, file);
  const license = relative.includes('manrope-')
    ? '/licenses/manrope.txt'
    : relative.includes('ibm-plex-mono-')
      ? '/licenses/ibm-plex-mono.txt'
      : null;
  if (!license || !files.includes(path.join(root, license)))
    failures.push(`Font without a reviewed notice: ${relative}`);
  const bytes = await fs.readFile(file);
  media.fonts.push({
    url: '/' + relative,
    license,
    bytes: bytes.length,
    sha256: createHash('sha256').update(bytes).digest('hex'),
  });
}
await fs.writeFile(
  new URL('../.generated/media-report.json', import.meta.url),
  JSON.stringify(media, null, 2) + '\n',
);
const expectedHtml = new Set([
  'index.html',
  '404.html',
  ...report.pages.map((e) => e.route.slice(1) + 'index.html'),
]);
const expectedDownloads = new Map();
for (const example of report.examples) {
  for (const file of example.files)
    expectedDownloads.set(file.href.slice(1), file.sha256);
  if (example.bundle)
    expectedDownloads.set(example.bundle.href.slice(1), example.bundle.sha256);
}
for (const [relative, expected] of expectedDownloads) {
  const file = path.join(root, relative);
  if (
    !files.includes(file) ||
    createHash('sha256')
      .update(await fs.readFile(file))
      .digest('hex') !== expected
  )
    failures.push(`Missing or changed source download: ${relative}`);
}
for (const file of files) {
  const relative = path.relative(root, file);
  if (relative.startsWith('downloads/') && !expectedDownloads.has(relative))
    failures.push(`Download has no approved owner: ${relative}`);
}
for (const file of byPath.keys()) {
  if (!expectedHtml.has(path.relative(root, file)))
    failures.push(
      `HTML has no publication owner: ${path.relative(root, file)}`,
    );
}
const sizes = await Promise.all(
  files.map(async (f) => ({
    path: path.relative(root, f),
    bytes: (await fs.stat(f)).size,
  })),
);
const capacity = inspectCapacity(
  sizes,
  await fs.readFile(path.join(root, '_headers'), 'utf8'),
  await fs.readFile(path.join(root, '_redirects'), 'utf8').catch((e) => {
    if (e.code === 'ENOENT') return '';
    throw e;
  }),
);
failures.push(...capacity.failures);
await fs.writeFile(
  new URL('../.generated/capacity-report.json', import.meta.url),
  JSON.stringify(capacity, null, 2) + '\n',
);
if (files.some((f) => /\/(scratch|node_modules|planning)\//.test(f)))
  failures.push('Private implementation material included in output');
if (failures.length) {
  console.error(failures.join('\n'));
  throw new Error(`${failures.length} output validation failures`);
}
const hash = createHash('sha256');
for (const f of files.sort()) {
  hash.update(path.relative(root, f));
  hash.update(await fs.readFile(f));
}
const evidence = {
  environment: report.environment,
  revision: report.revision,
  origin,
  apiReference,
  htmlPages: byPath.size,
  files: files.length,
  bytes: sizes.reduce((n, f) => n + f.bytes, 0),
  largest: sizes.sort((a, b) => b.bytes - a.bytes).slice(0, 5),
  sha256: hash.digest('hex'),
  result: 'pass',
};
await fs.writeFile(
  new URL('../.generated/build-evidence.json', import.meta.url),
  JSON.stringify(evidence, null, 2) + '\n',
);
console.log(
  `Output checks passed: ${byPath.size} HTML pages, ${files.length} files; internal links, fragments, indexing policy, and capacity.`,
);
