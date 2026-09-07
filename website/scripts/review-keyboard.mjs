import { chromium } from 'playwright-core';
import { mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
const output = resolve(
  process.argv[2] || '../scratch/ochat-website-evidence/p03',
);
await mkdir(output, { recursive: true });
const browser = await chromium.launch();
const evidence = [];
try {
  for (const width of [1440, 320])
    for (const route of [
      '/',
      '/docs/start/first-agent/',
      '/docs/reference/chatml/',
    ]) {
      const page = await browser.newPage({
        viewport: { width, height: 800 },
        reducedMotion: 'reduce',
      });
      await page.goto(
        (process.env.PLAYWRIGHT_BASE_URL || 'http://127.0.0.1:4321') + route,
      );
      await page.evaluate(() => document.fonts.ready);
      const seen = new Set();
      const stops = [];
      let complete = false;
      for (let i = 0; i < 1000; i++) {
        await page.keyboard.press('Tab');
        const state = await page.evaluate(() => {
          const el = document.activeElement;
          if (!el || el === document.body) return null;
          const style = getComputedStyle(el);
          const visible = [...el.getClientRects()].some((rect) => {
            const left = Math.max(rect.left, 0),
              right = Math.min(rect.right, innerWidth);
            const top = Math.max(rect.top, 0),
              bottom = Math.min(rect.bottom, innerHeight);
            return (
              right > left &&
              bottom > top &&
              [0.1, 0.5, 0.9].some((fraction) => {
                const hit = document.elementFromPoint(
                  left + (right - left) / 2,
                  top + (bottom - top) * fraction,
                );
                return hit && el.contains(hit);
              })
            );
          });
          return {
            index: [...document.querySelectorAll('*')].indexOf(el),
            tag: el.tagName,
            name: (el.getAttribute('aria-label') || el.textContent || '')
              .trim()
              .slice(0, 90),
            href: el.getAttribute('href'),
            visible,
            outline: style.outlineStyle,
          };
        });
        if (!state) continue;
        if (seen.has(state.index)) {
          complete = true;
          break;
        }
        seen.add(state.index);
        stops.push(state);
      }
      evidence.push({ width, route, complete, stops });
      await page.close();
    }
} finally {
  await browser.close();
}
await writeFile(
  resolve(output, 'keyboard-traversal.json'),
  JSON.stringify(evidence, null, 2) + '\n',
);
const results = evidence.map((e) => ({
  width: e.width,
  route: e.route,
  complete: e.complete,
  stops: e.stops.length,
  obscured: e.stops.filter((s) => !s.visible),
}));
console.log(JSON.stringify(results, null, 2));
if (results.some((r) => !r.complete || r.obscured.length)) process.exitCode = 1;
