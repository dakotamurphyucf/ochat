import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
import fs from 'node:fs';
import { createHash } from 'node:crypto';
const report = JSON.parse(
  fs.readFileSync(
    new URL('../../.generated/examples-report.json', import.meta.url),
    'utf8',
  ),
);

test('all ten lessons expose their host, verification, source bundles, and real previous/next links without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    await page.goto(report.tutorials[0].route);
    for (const [index, t] of report.tutorials.entries()) {
      await expect(page).toHaveURL(t.route);
      const record = page.getByRole('complementary', {
        name: 'Tutorial context and verification',
      });
      await expect(record).toContainText(t.host);
      await record.locator('summary').click();
      await expect(record).toContainText('No live provider calls');
      await expect(record).toContainText(t.verification.platform);
      if (index)
        await expect(
          page.getByRole('link', {
            name: new RegExp(
              'Previous.*' +
                report.tutorials[index - 1].title.replace(
                  /[.*+?^${}()|[\]\\]/g,
                  '\\$&',
                ),
            ),
          }),
        ).toHaveAttribute('href', report.tutorials[index - 1].route);
      await page.getByRole('link', { name: /Next / }).click();
    }
    await expect(page).toHaveURL('/docs/examples/');
  } finally {
    await context.close();
  }
});

test('catalog distinguishes complete examples, configured templates and illustrative output without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({
    javaScriptEnabled: false,
    viewport: { width: 390, height: 844 },
  });
  try {
    const page = await context.newPage();
    await page.goto('/docs/examples/#source-catalog');
    await expect(page.locator('.example-card')).toHaveCount(
      report.examples.length,
    );
    await page
      .getByRole('navigation', { name: 'Example types' })
      .getByRole('link', { name: 'Templates', exact: true })
      .click();
    await expect(page.locator('#template-examples')).toBeVisible();
    const specialist = page.locator('#example-specialist');
    await specialist.locator('.source-details > summary').click();
    await expect(
      specialist.getByRole('link', {
        name: 'docs-reviewer.chatmd',
        exact: true,
      }),
    ).toHaveAttribute('download', 'docs-reviewer.chatmd');
    await expect(specialist).toContainText('reference/project.txt');
    await expect(specialist).toContainText('LICENSE.txt');
    await expect(
      page.locator('#example-search-output a[download]'),
    ).toHaveCount(0);
    await expect(page.locator('#example-narrow-shell')).toContainText(
      'Do not combine native --local',
    );
  } finally {
    await context.close();
  }
});

test('every served source file and archive matches the declared bytes and has a working download', async ({
  page,
  request,
}) => {
  for (const e of report.examples) {
    for (const f of [...e.files, ...(e.bundle ? [e.bundle] : [])]) {
      const response = await request.get(f.href);
      expect(response.status(), f.href).toBe(200);
      expect(
        createHash('sha256')
          .update(await response.body())
          .digest('hex'),
      ).toBe(f.sha256);
    }
  }
  await page.goto('/docs/examples/#example-specialist');
  const pending = page.waitForEvent('download');
  await page.locator('#example-specialist .bundle-link').click();
  const download = await pending;
  expect(download.suggestedFilename()).toBe('specialist.tar');
  const downloaded = await download.path();
  expect(downloaded).toBeTruthy();
  expect(
    createHash('sha256').update(fs.readFileSync(downloaded!)).digest('hex'),
  ).toBe(
    report.examples.find((e: { id: string }) => e.id === 'specialist').bundle
      .sha256,
  );
});

test('new lessons and catalog retain readable narrow layouts and accessible expanded evidence in both themes', async ({
  page,
}) => {
  test.slow();
  await page.setViewportSize({ width: 320, height: 800 });
  for (const theme of ['light', 'dark'] as const) {
    await page.emulateMedia({ colorScheme: theme });
    for (const route of [
      '/docs/tutorials/file-tool/',
      '/docs/tutorials/specialist/',
      '/docs/tutorials/workflow/',
      '/docs/examples/',
    ]) {
      await page.goto(route);
      const summary = page.locator(
        route === '/docs/examples/'
          ? '#example-specialist .source-details > summary'
          : '.tutorial-record summary',
      );
      await summary.click();
      if (route === '/docs/examples/')
        await page
          .locator('#example-specialist .example-source > summary')
          .click();
      const source = page.locator('.example-source[open]').first();
      await source
        .locator('.source-file[data-source-path="LICENSE.txt"] > summary')
        .click();
      await page.evaluate(() => document.fonts.ready);
      expect(
        await page.evaluate(
          () => document.documentElement.scrollWidth <= innerWidth,
        ),
      ).toBe(true);
      const result = await new AxeBuilder({ page })
        .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
        .analyze();
      expect(result.violations).toEqual([]);
    }
  }
});

test('tutorial instructions preserve tool, batch, and source-context qualifications', async ({
  page,
}) => {
  for (const [route, expected] of [
    ['/docs/tutorials/file-tool/', 'does not create or constrain a capability'],
    [
      '/docs/tutorials/specialist/',
      'not through an arbitrary working-directory fallback',
    ],
    ['/docs/tutorials/workflow/', 'not a dollar spending cap'],
    ['/docs/tutorials/stdio-client/', 'before the binary initializes the RNG'],
    ['/docs/tutorials/http-client/', 'loopback, not public HTTPS'],
  ]) {
    await page.goto(route);
    await expect(page.locator('.sl-markdown-content')).toContainText(expected);
  }
});

test('all example files can be read inline without JavaScript or downloads, with literal source preserved', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    const downloads: string[] = [];
    page.on('download', (download) =>
      downloads.push(download.suggestedFilename()),
    );
    await page.goto('/docs/examples/');
    for (const example of report.examples.filter(
      (e: { files: unknown[] }) => e.files.length,
    )) {
      const viewer = page.locator(`[data-example-source="${example.id}"]`);
      await viewer.locator(':scope > summary').click();
      for (const file of example.files) {
        const panel = viewer.locator(`[data-source-path="${file.path}"]`);
        if ((await panel.getAttribute('open')) === null)
          await panel.locator(':scope > summary').click();
        const code = panel.locator('pre');
        await expect(code).toBeVisible();
        expect(await code.locator('code').textContent()).toBe(file.content);
        await expect(code).toHaveAttribute('tabindex', '0');
        if (file.path.endsWith('.chatml')) {
          await expect(code).toHaveAttribute('data-language', 'ocaml');
          expect(await code.locator('span[style]').count()).toBeGreaterThan(0);
        }
        await expect(code).toHaveAccessibleName(
          `${example.title}: ${file.path} source`,
        );
        await expect(code.locator('user, tool, developer, script')).toHaveCount(
          0,
        );
      }
    }
    for (const tutorial of report.tutorials) {
      await page.goto(tutorial.route);
      for (const [index, id] of tutorial.examples.entries()) {
        const example = report.examples.find(
          (e: { id: string }) => e.id === id,
        );
        const viewer = page.locator(`[data-example-source="${id}"]`);
        if (index) await viewer.locator(':scope > summary').click();
        const code = viewer.locator(
          `[data-source-path="${example.entry}"] pre code`,
        );
        await expect(code).toBeVisible();
        expect(await code.textContent()).toBe(
          example.files.find((f: { path: string }) => f.path === example.entry)
            .content,
        );
      }
    }
    for (const example of report.examples.filter(
      (e: { files: unknown[]; tutorialRoute: string }) =>
        e.files.length &&
        !report.tutorials.some(
          (t: { route: string }) => t.route === e.tutorialRoute,
        ),
    )) {
      await page.goto(example.tutorialRoute);
      const code = page.locator(
        `[data-example-source="${example.id}"] [data-source-path="${example.entry}"] pre code`,
      );
      await expect(code).toBeVisible();
      expect(await code.textContent()).toBe(
        example.files.find((f: { path: string }) => f.path === example.entry)
          .content,
      );
    }
    expect(downloads).toEqual([]);
  } finally {
    await context.close();
  }
});

test('inline reader supports keyboard file expansion and horizontal source scrolling', async ({
  page,
}) => {
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto('/docs/tutorials/specialist/');
  const companion = page.locator('[data-source-path="docs-reviewer.chatmd"]');
  await companion.locator('summary').focus();
  await page.keyboard.press('Enter');
  await expect(companion.locator('pre')).toBeVisible();
  await page.keyboard.press('Tab');
  const code = companion.locator('pre');
  await expect(code).toBeFocused();
  await page.keyboard.press('ArrowRight');
  await expect
    .poll(() => code.evaluate((node) => node.scrollLeft))
    .toBeGreaterThan(0);
  await companion.locator('summary').focus();
  await page.keyboard.press('Space');
  await expect(code).not.toBeVisible();
});
