import { chromium } from 'playwright-core';
import AxeBuilder from '@axe-core/playwright';
import fs from 'node:fs/promises';
import path from 'node:path';
const [directory, evidenceFile] = process.argv.slice(2);
if (!directory || !evidenceFile)
  throw new Error(
    'Usage: node scripts/review-release.mjs OUTPUT BUILD-EVIDENCE.json',
  );
const output = path.resolve(directory);
const artifact = JSON.parse(await fs.readFile(evidenceFile, 'utf8'));
const base = process.env.PLAYWRIGHT_BASE_URL || 'http://127.0.0.1:4321';
await fs.mkdir(output, { recursive: true });
const browser = await chromium.launch();
const report = {
  artifactSha256: artifact.sha256,
  base,
  scope:
    'Automated screenshots, axe, ARIA snapshots, and reflow. Does not replace actual screen-reader, browser chrome zoom, or mobile software-keyboard review.',
  pages: [],
  result: 'fail',
};
try {
  for (const theme of ['light', 'dark'])
    for (const width of [1440, 390]) {
      const context = await browser.newContext({
        colorScheme: theme,
        viewport: { width, height: 1000 },
        reducedMotion: 'reduce',
      });
      const page = await context.newPage();
      for (const [name, route] of [
        ['home', '/'],
        ['first-agent', '/docs/start/first-agent/'],
        ['chatmd', '/docs/reference/chatmd/'],
        ['chatml', '/docs/reference/chatml/'],
        ['library', '/docs/library/webpage-markdown/driver/'],
        ['404', '/not-an-ochat-page/'],
        ['search', '/docs/'],
        ...(width === 390 ? [['menu', '/docs/start/first-agent/']] : []),
      ]) {
        await page.goto(base + route);
        await page.evaluate(() => document.fonts.ready);
        if (name === 'search') {
          await page.getByRole('button', { name: /Search/ }).click();
          await page.locator('dialog input').fill('read_file');
          await page.locator('.pagefind-ui__result-link').first().waitFor();
        }
        if (name === 'menu')
          await page.getByRole('button', { name: 'Menu', exact: true }).click();
        const stem = `${name}-${theme}-${width}`;
        await page.screenshot({
          path: path.join(output, stem + '.png'),
          fullPage: ['home', 'first-agent'].includes(name),
        });
        const violations = (
          await new AxeBuilder({ page })
            .withTags(['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'])
            .analyze()
        ).violations;
        const overflow = await page.evaluate(
          () => document.documentElement.scrollWidth > innerWidth,
        );
        await fs.writeFile(
          path.join(output, stem + '.aria.txt'),
          await page.locator('body').ariaSnapshot(),
        );
        report.pages.push({
          name,
          route,
          theme,
          width,
          overflow,
          violations,
          screenshot: stem + '.png',
        });
        if (overflow || violations.length)
          throw new Error(`Release visual/accessibility failure: ${stem}`);
      }
      await context.close();
    }
  report.result = 'pass';
} finally {
  await browser.close();
  await fs.writeFile(
    path.join(output, 'report.json'),
    JSON.stringify(report, null, 2) + '\n',
  );
}
console.log(`Release visual audit: ${report.pages.length} combinations pass`);
