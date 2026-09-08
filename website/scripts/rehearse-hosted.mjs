// Read-only public HTTPS checks against an independently retained artifact.
import fs from 'node:fs/promises';
import path from 'node:path';
import { createHash } from 'node:crypto';
import { verifyArtifact } from './release-artifact.mjs';

const [directory, output] = process.argv.slice(2);
if (!directory || !output)
  throw new Error(
    'Usage: node scripts/rehearse-hosted.mjs ARTIFACT REPORT.json',
  );
const artifact = await verifyArtifact(path.resolve(directory));
const origin = new URL(artifact.build.origin);
if (origin.protocol !== 'https:' || artifact.build.environment !== 'preview')
  throw new Error('Hosted rehearsal requires an HTTPS preview artifact');
const headers = await fs.readFile(
  path.join(directory, 'dist/_headers'),
  'utf8',
);
const baseline = headers.includes('X-Ochat-Rehearsal: baseline');
const report = {
  startedAt: new Date().toISOString(),
  origin: origin.origin,
  artifactSha256: artifact.build.sha256,
  revision: artifact.build.revision,
  baseline,
  scope:
    'Public HTTPS from this machine; exact served bytes, routing, headers and conditional caching. Browser interactions are recorded separately.',
  result: 'fail',
  checks: [],
  responses: [],
};
const hash = (bytes) => createHash('sha256').update(bytes).digest('hex');
function check(pass, message) {
  report.checks.push({ pass: Boolean(pass), message });
  if (!pass) throw new Error(message);
}
async function get(route, options = {}) {
  const response = await fetch(new URL(route, origin), {
    signal: AbortSignal.timeout(30000),
    redirect: 'manual',
    ...options,
  });
  report.responses.push({
    route,
    status: response.status,
    headers: Object.fromEntries(
      [...response.headers].filter(([name]) =>
        [
          'content-type',
          'cache-control',
          'etag',
          'cf-cache-status',
          'cf-ray',
          'age',
          'location',
          'x-robots-tag',
          'x-ochat-rehearsal',
          'server',
        ].includes(name),
      ),
    ),
  });
  return response;
}
try {
  async function checkFile(file) {
    const route = '/' + file.path.replace(/index\.html$/, '');
    const response = await get(route);
    check(response.status === 200, `${route}: HTTP 200`);
    check(
      hash(Buffer.from(await response.arrayBuffer())) === file.sha256,
      `${route}: exact retained bytes`,
    );
    check(
      response.headers.get('x-content-type-options') === 'nosniff',
      `${route}: nosniff`,
    );
    check(
      response.headers.get('x-robots-tag') === 'noindex, nofollow',
      `${route}: preview indexing header`,
    );
    check(
      response.headers.get('x-ochat-rehearsal') ===
        (baseline ? 'baseline' : null),
      `${route}: expected rehearsal version marker`,
    );
    const cache = response.headers.get('cache-control') || '';
    if (file.path.startsWith('_astro/'))
      check(
        cache.includes('immutable') && !cache.includes('max-age=0'),
        `${route}: immutable hashed asset`,
      );
    else
      check(
        cache.includes('must-revalidate') && !cache.includes('immutable'),
        `${route}: mutable asset revalidation`,
      );
    if (file.path.endsWith('.js'))
      check(
        /javascript/.test(response.headers.get('content-type')),
        `${route}: JavaScript MIME`,
      );
    if (file.path.endsWith('.wasm'))
      check(
        /application\/wasm/.test(response.headers.get('content-type')),
        `${route}: WASM MIME`,
      );
  }
  // Bound remote traffic and settle in-flight checks before writing a report.
  const files = artifact.files.filter(
    (file) => !['_headers', '_redirects', '404.html'].includes(file.path),
  );
  for (let index = 0; index < files.length; index += 4) {
    const results = await Promise.allSettled(
      files.slice(index, index + 4).map(checkFile),
    );
    const failures = results.filter((result) => result.status === 'rejected');
    if (failures.length)
      throw new AggregateError(
        failures.map((failure) => failure.reason),
        failures.map((failure) => failure.reason.message).join('; '),
      );
  }
  const robots = await get('/robots.txt');
  check(
    /Disallow:\s*\//.test(await robots.text()),
    'robots: disallow crawling',
  );
  for (const route of [
    '/docs/start/first-agent',
    '/docs/start/first-agent?rehearsal=1',
  ]) {
    const response = await get(route);
    check(
      [301, 307, 308].includes(response.status),
      `${route}: slash redirect`,
    );
    const target = new URL(response.headers.get('location'), origin);
    check(
      target.origin === origin.origin &&
        target.pathname === '/docs/start/first-agent/' &&
        target.search === new URL(route, origin).search,
      `${route}: same-host slash target preserves query`,
    );
  }
  const query = await get('/docs/start/first-agent/?rehearsal=1');
  check(
    query.status === 200 &&
      hash(Buffer.from(await query.arrayBuffer())) ===
        artifact.files.find(
          (f) => f.path === 'docs/start/first-agent/index.html',
        ).sha256,
    'Query string preserves document',
  );
  const missing = artifact.files.find((f) => f.path === '404.html');
  for (const route of [
    '/missing-ochat-route/',
    '/docs/missing-ochat-route/',
    '/_headers',
    '/scratch/ochat-website-implementation-notes.md',
    '/planning/implementation-spec.md',
    '/.generated/build-evidence.json',
    '/api/',
  ]) {
    const response = await get(route);
    check(response.status === 404, `${route}: real 404`);
    check(
      hash(Buffer.from(await response.arrayBuffer())) === missing.sha256,
      `${route}: exact branded 404`,
    );
    check(
      response.headers.get('x-robots-tag') === 'noindex, nofollow',
      `${route}: noindex 404`,
    );
  }
  for (const route of [
    '/',
    '/pagefind/pagefind.js',
    '/' +
      artifact.files.find(
        (f) => f.path.startsWith('_astro/') && f.path.endsWith('.js'),
      ).path,
  ]) {
    const first = await get(route);
    const etag = first.headers.get('etag');
    await first.arrayBuffer();
    if (!etag && route === '/') {
      // Cloudflare may strip HTML validators at the edge. Verify a fresh full
      // response instead; do not claim a conditional HTML hit or disable CDN
      // compression merely to manufacture an ETag.
      const fresh = await get(route, {
        headers: { 'Cache-Control': 'no-cache' },
      });
      check(
        fresh.status === 200 &&
          hash(Buffer.from(await fresh.arrayBuffer())) ===
            artifact.files.find((f) => f.path === 'index.html').sha256,
        'HTML without ETag: explicit revalidation returns the complete current document',
      );
      report.htmlValidator = 'Absent at edge; full 200 revalidation verified';
      continue;
    }
    check(Boolean(etag), `${route}: ETag present`);
    const conditional = await get(route, {
      headers: { 'If-None-Match': etag },
    });
    check(conditional.status === 304, `${route}: conditional ETag 304`);
  }
  check(
    report.responses.some((r) => r.headers['cf-ray']),
    'Cloudflare edge response observed over verified HTTPS',
  );
  report.result = 'pass';
} catch (error) {
  report.error = error.message;
  throw error;
} finally {
  report.finishedAt = new Date().toISOString();
  await fs.mkdir(path.dirname(path.resolve(output)), { recursive: true });
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
}
console.log(
  `Hosted rehearsal: ${report.checks.length} checks passed; ${output}`,
);
