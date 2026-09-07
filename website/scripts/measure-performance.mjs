import fs from 'node:fs/promises';
import path from 'node:path';
import { gzipSync } from 'node:zlib';
import { chromium } from 'playwright-core';
import { preview } from 'astro';
const root = new URL('../', import.meta.url).pathname;
const output = path.resolve(
  process.argv[2] || path.join(root, '.generated/performance-report.json'),
);
const artifact = JSON.parse(
  await fs.readFile(path.join(root, '.generated/build-evidence.json')),
);
const routes = [
  '/',
  '/docs/applications/',
  '/docs/applications/documentation-review/',
  '/docs/start/first-agent/',
  '/docs/reference/chatml/',
  '/docs/reference/agent-server/protocol-types/',
  '/docs/library/webpage-markdown/driver/',
];
const server = await preview({ root, server: { host: '127.0.0.1', port: 0 } });
const base = `http://127.0.0.1:${server.server.address().port}`;
const browser = await chromium.launch();
const report = {
  artifactSha256: artifact.sha256,
  conditions:
    'Cold Chromium, 390×844, 4× CPU slowdown, 150ms latency, 1.6 Mbps download; gzip sizes computed from response bodies; JS/CSS budgets also include inline scripts/style tags. Not hosted wire bytes. No scroll or search. One diagnostic sample per page, not field Web Vitals.',
  pages: [],
  failures: [],
};
try {
  for (const route of routes) {
    const context = await browser.newContext({
      viewport: { width: 390, height: 844 },
    });
    const page = await context.newPage();
    const cdp = await context.newCDPSession(page);
    await cdp.send('Network.enable');
    await cdp.send('Network.setCacheDisabled', { cacheDisabled: true });
    await cdp.send('Network.emulateNetworkConditions', {
      offline: false,
      latency: 150,
      downloadThroughput: 200000,
      uploadThroughput: 100000,
    });
    await cdp.send('Emulation.setCPUThrottlingRate', { rate: 4 });
    await page.addInitScript(() => {
      window.__performanceSample = { cls: 0, lcp: null };
      new PerformanceObserver((list) => {
        for (const e of list.getEntries())
          if (!e.hadRecentInput) window.__performanceSample.cls += e.value;
      }).observe({ type: 'layout-shift', buffered: true });
      new PerformanceObserver((list) => {
        for (const e of list.getEntries())
          window.__performanceSample.lcp = e.startTime;
      }).observe({ type: 'largest-contentful-paint', buffered: true });
    });
    const bodies = [];
    page.on('response', (response) =>
      bodies.push(
        (async () => {
          try {
            const body = await response.body();
            return {
              path: new URL(response.url()).pathname,
              type: response.request().resourceType(),
              bytes: body.length,
              gzip: gzipSync(body).length,
            };
          } catch {
            return null;
          }
        })(),
      ),
    );
    await page.goto(base + route);
    await page.evaluate(() => document.fonts.ready);
    await page.waitForTimeout(1200);
    const metrics = await page.evaluate(() => ({
      ...window.__performanceSample,
      domNodes: document.querySelectorAll('*').length,
      overflow: document.documentElement.scrollWidth > innerWidth,
    }));
    const resources = (await Promise.all(bodies)).filter(Boolean);
    const sum = (type, field) =>
      resources
        .filter((r) => !type || r.type === type)
        .reduce((n, r) => n + r[field], 0);
    const inline = await page.evaluate(() => ({
      scripts: [...document.querySelectorAll('script:not([src])')]
        .filter(
          (s) => !s.type || s.type === 'module' || s.type === 'text/javascript',
        )
        .map((s) => s.textContent)
        .join('\n'),
      styles: [...document.querySelectorAll('style')]
        .map((s) => s.textContent)
        .join('\n'),
    }));
    const inlineJsGzip = inline.scripts ? gzipSync(inline.scripts).length : 0;
    const inlineCssGzip = inline.styles ? gzipSync(inline.styles).length : 0;
    const row = {
      route,
      ...metrics,
      inlineJsGzip,
      inlineCssGzip,
      eagerJsGzip: sum('script', 'gzip') + inlineJsGzip,
      cssGzip: sum('stylesheet', 'gzip') + inlineCssGzip,
      fontBytes: sum('font', 'bytes'),
      initialGzip: resources.reduce(
        (n, r) => n + (['font', 'image'].includes(r.type) ? r.bytes : r.gzip),
        0,
      ),
      resources,
    };
    const checks = {
      eagerJs: row.eagerJsGzip <= (route === '/' ? 100000 : 150000),
      css: row.cssGzip <= 100000,
      fonts: row.fontBytes <= 150000,
      layoutShift: row.cls <= 0.1,
      reflow: !row.overflow,
      ...(route === '/' ? { initialTransfer: row.initialGzip <= 800000 } : {}),
    };
    report.pages.push({ ...row, checks });
    for (const [name, passed] of Object.entries(checks))
      if (!passed) report.failures.push(`${route}: ${name}`);
    await context.close();
  }
  report.result = report.failures.length ? 'fail' : 'pass';
  await fs.mkdir(path.dirname(output), { recursive: true });
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
  console.log(
    JSON.stringify(
      report.pages.map(({ resources, ...row }) => row),
      null,
      2,
    ),
  );
  if (report.failures.length) throw new Error(report.failures.join('\n'));
} finally {
  await browser.close();
  await server.stop();
}
