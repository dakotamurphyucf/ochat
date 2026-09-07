// Exercises the actual pinned Workers asset runtime locally. Never deploys.
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import net from 'node:net';
import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { verifyArtifact } from './release-artifact.mjs';
const site = fileURLToPath(new URL('../', import.meta.url));
const [output, ...directories] = process.argv.slice(2);
if (!output || !directories.length)
  throw new Error(
    'Usage: node scripts/rehearse-static.mjs REPORT.json ARTIFACT [ARTIFACT ...]',
  );
const report = {
  scope:
    'Local pinned Wrangler/workerd HTTP rehearsal. No public HTTPS, CDN cache, account configuration or hosted rollback claim.',
  runs: [],
  result: 'fail',
};
const wait = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const freePort = () =>
  new Promise((resolve, reject) => {
    const server = net.createServer();
    server.once('error', reject);
    server.listen(0, '127.0.0.1', () => {
      const port = server.address().port;
      server.close(() => resolve(port));
    });
  });
try {
  for (const directory of directories.map((d) => path.resolve(d))) {
    const artifact = await verifyArtifact(directory);
    const modernCachePolicy = (
      await fs.readFile(path.join(directory, 'dist/_headers'), 'utf8')
    ).includes('must-revalidate');
    const temporary = await fs.mkdtemp(
      path.join(os.tmpdir(), 'ochat-static-rehearsal-'),
    );
    const config = JSON.parse(
      await fs.readFile(path.join(directory, 'wrangler.jsonc'), 'utf8'),
    );
    delete config.$schema;
    config.assets.directory = path.join(directory, 'dist');
    await fs.writeFile(
      path.join(temporary, 'wrangler.json'),
      JSON.stringify(config),
    );
    const port = await freePort();
    const base = `http://127.0.0.1:${port}`;
    const child = spawn(
      process.execPath,
      [
        path.join(site, 'node_modules/wrangler/bin/wrangler.js'),
        'dev',
        '--local',
        '--config',
        path.join(temporary, 'wrangler.json'),
        '--ip',
        '127.0.0.1',
        '--port',
        String(port),
        '--inspector-port',
        '0',
      ],
      {
        cwd: site,
        env: { ...process.env, WRANGLER_SEND_METRICS: 'false' },
        stdio: ['ignore', 'pipe', 'pipe'],
      },
    );
    let log = '';
    child.stdout.on('data', (b) => (log += b));
    child.stderr.on('data', (b) => (log += b));
    const row = {
      artifactSha256: artifact.build.sha256,
      environment: artifact.build.environment,
      origin: artifact.build.origin,
      cachePolicy: modernCachePolicy
        ? 'explicit revalidation and immutable hashed assets'
        : 'previous artifact defaults',
      checks: [],
    };
    report.runs.push(row);
    const assert = (condition, message) => {
      row.checks.push({ message, pass: Boolean(condition) });
      if (!condition) throw new Error(message);
    };
    try {
      const deadline = Date.now() + 45000;
      let ready = false;
      while (Date.now() < deadline && child.exitCode === null) {
        try {
          if (
            (await fetch(base, { signal: AbortSignal.timeout(1000) }))
              .status === 200
          ) {
            ready = true;
            break;
          }
        } catch {}
        await wait(200);
      }
      if (!ready) throw new Error(`Workers runtime did not start: ${log}`);
      for (const route of [
        '/',
        '/docs/start/first-agent/',
        '/docs/applications/',
        '/docs/applications/documentation-review/',
        '/docs/reference/chatml/',
      ]) {
        const response = await fetch(base + route);
        assert(response.status === 200, `${route}: 200`);
        const html = await response.text();
        assert(html.includes('<h1'), `${route}: rendered HTML`);
        const expected = artifact.files.find(
          (f) => f.path === route.slice(1) + 'index.html',
        );
        assert(
          createHash('sha256').update(html).digest('hex') === expected.sha256,
          `${route}: exact retained HTML`,
        );
        assert(
          response.headers.get('x-content-type-options') === 'nosniff',
          `${route}: nosniff`,
        );
        if (modernCachePolicy)
          assert(
            response.headers.get('cache-control')?.includes('must-revalidate'),
            `${route}: mutable document revalidates`,
          );
        assert(
          (response.headers.get('x-robots-tag') || '').includes('noindex') ===
            (artifact.build.environment === 'preview'),
          `${route}: correct indexing header`,
        );
      }
      const slash = await fetch(base + '/docs/start/first-agent', {
        redirect: 'manual',
      });
      assert(
        [301, 307, 308].includes(slash.status),
        'Trailing slash redirects',
      );
      assert(
        new URL(slash.headers.get('location'), base).pathname ===
          '/docs/start/first-agent/',
        'Trailing slash target',
      );
      for (const route of [
        '/missing-ochat-route/',
        '/docs/missing-ochat-route/',
        '/_headers',
        '/scratch/private.txt',
        '/api/',
      ])
        assert(
          (await fetch(base + route)).status === 404,
          `${route}: real 404`,
        );
      const notFound = await (
        await fetch(base + '/missing-ochat-route/')
      ).text();
      assert(
        notFound.includes('noindex') &&
          notFound.includes('Search documentation'),
        '404 branding and noindex',
      );
      for (const file of artifact.files.filter((f) =>
        f.path.startsWith('downloads/'),
      )) {
        const response = await fetch(base + '/' + file.path);
        const bytes = Buffer.from(await response.arrayBuffer());
        assert(
          response.status === 200 &&
            createHash('sha256').update(bytes).digest('hex') === file.sha256,
          `${file.path}: exact downloaded bytes`,
        );
      }
      for (const file of [
        'pagefind/pagefind.js',
        'pagefind/pagefind-worker.js',
      ].filter((name) => artifact.files.some((f) => f.path === name))) {
        const response = await fetch(base + '/' + file);
        assert(
          response.status === 200 &&
            /javascript/.test(response.headers.get('content-type')),
          `${file}: JavaScript MIME`,
        );
        assert(
          !response.headers.get('cache-control')?.includes('immutable'),
          `${file}: mutable search entry point`,
        );
      }
      const chunk = artifact.files.find(
        (f) => f.path.startsWith('_astro/') && f.path.endsWith('.js'),
      );
      const resource = await fetch(base + '/' + chunk.path);
      if (modernCachePolicy)
        assert(
          resource.headers.get('cache-control')?.includes('immutable'),
          'Hashed script: immutable cache policy',
        );
      if (modernCachePolicy)
        assert(
          !resource.headers.get('cache-control')?.includes('max-age=0'),
          'Hashed script: no conflicting mutable cache policy',
        );
      const etag = resource.headers.get('etag');
      assert(Boolean(etag), 'Hashed script: ETag');
      assert(
        (
          await fetch(base + '/' + chunk.path, {
            headers: { 'If-None-Match': etag },
          })
        ).status === 304,
        'ETag conditional request: 304',
      );
      assert(
        (await fetch(base + '/robots.txt')).status === 200,
        'robots.txt served',
      );
      row.result = 'pass';
    } finally {
      child.kill('SIGTERM');
      await Promise.race([
        new Promise((resolve) => child.once('exit', resolve)),
        wait(5000),
      ]);
      if (child.exitCode === null) child.kill('SIGKILL');
      await fs.rm(temporary, { recursive: true, force: true });
    }
  }
  report.result = 'pass';
} finally {
  await fs.mkdir(path.dirname(path.resolve(output)), { recursive: true });
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
}
console.log(
  `Static rehearsal: ${report.runs.length} artifacts passed; ${output}`,
);
