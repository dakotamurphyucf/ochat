// Reproducible local visual evidence; never part of the public website build.
import { chromium } from 'playwright-core';
import AxeBuilder from '@axe-core/playwright';
import { mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
const output = resolve(
  process.argv[2] || '../scratch/ochat-website-evidence/p03',
);
await mkdir(output, { recursive: true });
const browser = await chromium.launch();
const evidence = {
  capturedAt: new Date().toISOString(),
  browser: browser.version(),
  pages: [],
  contrast: [],
  nonTextContrast: [],
  limitations: [
    '320 CSS px models 400% reflow from a 1280px window; browser chrome zoom and assistive-technology review remain release checks.',
  ],
};
const routes = [
  ['home', '/'],
  ['tutorial', '/docs/start/first-agent/'],
  ['reference', '/docs/reference/chatml/'],
];
const luminance = (rgb) =>
  rgb
    .slice(0, 3)
    .map((v) => v / 255)
    .map((v) => (v <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4))
    .reduce((n, v, i) => n + v * [0.2126, 0.7152, 0.0722][i], 0);
const contrast = (a, b) => {
  const [hi, lo] = [luminance(a), luminance(b)].sort((a, b) => b - a);
  return (hi + 0.05) / (lo + 0.05);
};
try {
  for (const theme of ['light', 'dark'])
    for (const width of [1440, 390]) {
      const context = await browser.newContext({
        viewport: { width, height: 1000 },
        colorScheme: theme,
        reducedMotion: 'reduce',
      });
      const page = await context.newPage();
      for (const [name, route] of routes) {
        await page.goto(
          (process.env.PLAYWRIGHT_BASE_URL || 'http://127.0.0.1:4321') + route,
        );
        await page.evaluate(() => document.fonts.ready);
        const stem = `${name}-${theme}-${width}`;
        await page.screenshot({
          path: resolve(output, `${stem}.png`),
          fullPage: name !== 'reference',
        });
        const axe = await new AxeBuilder({ page })
          .withTags(['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'])
          .analyze();
        const overflow = await page.evaluate(
          () => document.documentElement.scrollWidth > innerWidth,
        );
        evidence.pages.push({
          route,
          theme,
          width,
          overflow,
          violations: axe.violations,
          screenshot: `${stem}.png`,
        });
        if (width === 1440) {
          const selectors =
            name === 'home'
              ? [
                  'h1',
                  '.hero-description',
                  '.primary',
                  '.launch-note',
                  '.code-caption',
                  '#copy-agent',
                  '.hero-code pre',
                ]
              : [
                  'h1',
                  '.article-description',
                  '.source-link',
                  '.sl-markdown-content p',
                  '.sl-markdown-content a',
                  '.section-label',
                  '.expressive-code pre',
                ];
          for (const selector of selectors) {
            const el = page.locator(selector).first();
            const colors = await el.evaluate((el) => {
              const rgba = (value) => value.match(/[\d.]+/g).map(Number);
              const layers = [];
              for (let node = el; node; node = node.parentElement)
                layers.unshift(rgba(getComputedStyle(node).backgroundColor));
              let bg = [255, 255, 255];
              for (const layer of layers) {
                const alpha = layer[3] ?? 1;
                bg = bg.map((v, i) => layer[i] * alpha + v * (1 - alpha));
              }
              const s = getComputedStyle(el);
              return {
                foreground: rgba(s.color),
                background: bg,
                fontSize: s.fontSize,
              };
            });
            evidence.contrast.push({
              route,
              theme,
              selector,
              ...colors,
              ratio: Number(
                contrast(colors.foreground, colors.background).toFixed(2),
              ),
              minimum: 4.5,
            });
          }
          const focusSelectors =
            name === 'home'
              ? ['.primary', '#copy-agent', '.hero-code pre']
              : [
                  '.source-link',
                  'site-search button[data-open-modal]',
                  '.expressive-code pre',
                  '.theme-control select',
                ];
          for (const selector of focusSelectors) {
            const el = page.locator(selector).first();
            // Expressive Code intentionally removes fitting blocks from tab order.
            if (
              selector.includes('pre') &&
              !(await el.evaluate((e) => e.scrollWidth > e.clientWidth))
            )
              continue;
            await el.focus();
            const colors = await el.evaluate((el) => {
              const rgba = (value) => value.match(/[\d.]+/g).map(Number);
              const layers = [];
              for (
                let node =
                  parseFloat(getComputedStyle(el).outlineOffset) < 0
                    ? el
                    : el.parentElement;
                node;
                node = node.parentElement
              )
                layers.unshift(rgba(getComputedStyle(node).backgroundColor));
              let bg = [255, 255, 255];
              for (const layer of layers) {
                const alpha = layer[3] ?? 1;
                bg = bg.map((v, i) => layer[i] * alpha + v * (1 - alpha));
              }
              const s = getComputedStyle(el);
              return {
                foreground: rgba(s.outlineColor),
                background: bg,
                outline: s.outlineStyle,
                width: s.outlineWidth,
              };
            });
            evidence.nonTextContrast.push({
              route,
              theme,
              selector,
              ...colors,
              ratio: Number(
                contrast(colors.foreground, colors.background).toFixed(2),
              ),
              minimum: 3,
            });
          }
        }
        if (name === 'reference') {
          const content = page.locator('.sl-markdown-content h2').nth(2);
          await content.scrollIntoViewIfNeeded();
          await page.screenshot({
            path: resolve(output, `${stem}-reading.png`),
          });
        }
      }
      if (width === 390) {
        await page.goto('http://127.0.0.1:4321/docs/start/first-agent/');
        await page.getByRole('button', { name: 'Menu', exact: true }).click();
        await page.screenshot({
          path: resolve(output, `navigation-${theme}.png`),
        });
        await page.keyboard.press('Escape');
        await page.locator('mobile-starlight-toc summary').click();
        await page.screenshot({
          path: resolve(output, `contents-${theme}.png`),
        });
      }
      await context.close();
    }
} finally {
  await browser.close();
}
await writeFile(
  resolve(output, 'review.json'),
  JSON.stringify(evidence, null, 2) + '\n',
);
const failures =
  evidence.pages.filter((p) => p.overflow || p.violations.length).length +
  [...evidence.contrast, ...evidence.nonTextContrast].filter(
    (p) => p.ratio < p.minimum || p.outline === 'none',
  ).length;
console.log(
  `Design review: ${evidence.pages.length} page/theme/width combinations, ${evidence.contrast.length} text pairs, ${evidence.nonTextContrast.length} focus pairs, ${failures} failures. Evidence: ${output}`,
);
if (failures) process.exitCode = 1;
