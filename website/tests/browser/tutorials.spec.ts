import { test, expect, type Browser, type Page } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
import fs from 'node:fs';
import { createHash } from 'node:crypto';
const report = JSON.parse(
  fs.readFileSync(
    new URL('../../.generated/examples-report.json', import.meta.url),
    'utf8',
  ),
);

test('all lessons expose host, verification and learning-path navigation without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    for (const t of report.tutorials) {
      await page.goto(t.route);
      await expect(page).toHaveURL(t.route);
      const record = page.getByRole('complementary', {
        name: 'Tutorial context and verification',
      });
      await expect(record).toContainText(t.host);
      await record.locator('summary').click();
      await expect(record).toContainText('No live provider calls');
      await expect(record).toContainText(t.verification.platform);
      if (t.previous)
        await expect(
          page.getByRole('link', {
            name: new RegExp(
              'Previous.*' +
                t.previous.title.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'),
            ),
          }),
        ).toHaveAttribute('href', t.previous.route);
      else
        await expect(page.getByRole('link', { name: /Previous / })).toHaveCount(
          0,
        );
      await page.getByRole('link', { name: /Next / }).click();
      await expect(page).toHaveURL(t.next.route);
    }
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
      'Use --local --authorize-shell-manifest',
    );
  } finally {
    await context.close();
  }
});

test('verification retains earlier observations separately from current check status without JavaScript', async ({
  browser,
}) => {
  await withNativeSourceReader(browser, async (page) => {
    const tutorial = report.tutorials.find(
      (item: { route: string }) => item.route === '/docs/tutorials/file-tool/',
    );
    await page.goto(tutorial.route);
    await page.locator('.tutorial-record summary').click();
    const evidence = page.locator('.tutorial-record .verification-evidence');
    await expect(evidence).toContainText('Current check status:');
    await expect(evidence).toContainText('Recorded evidence:');
    await expect(evidence).toContainText(tutorial.verification.scope);
    await expect(evidence).toContainText(tutorial.verification.observed);
    await expect(evidence).toContainText(tutorial.verification.limitations);

    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto('/docs/examples/#example-documentation-lab');
    const application = report.examples.find(
      (item: { id: string }) => item.id === 'documentation-lab',
    );
    const card = page.locator('#example-documentation-lab');
    await card.locator('.source-details > summary').click();
    const recorded = card.locator('.verification-evidence');
    await expect(recorded).toContainText(application.verification.scope);
    await expect(recorded).toContainText(application.verification.observed);
    await expect(recorded).toContainText(application.verification.limitations);
    expect(
      await page.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
    ).toBe(true);
  });
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
      '/docs/tutorials/chatml-program/',
      '/docs/tutorials/chatml-tool/',
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
      await source.getByRole('combobox').selectOption('LICENSE.txt');
      await expect(
        source.locator('[data-source-path="LICENSE.txt"] pre'),
      ).toBeVisible();
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
    [
      '/docs/tutorials/stdio-client/',
      'it does not keep a process running after the client exits',
    ],
    ['/docs/tutorials/http-client/', 'loopback, not public HTTPS'],
  ]) {
    await page.goto(route);
    await expect(page.locator('.sl-markdown-content')).toContainText(expected);
  }
});

// Keep catalog and guide checks independent as the example inventory grows.
// Every case retains literal-byte, native-reading, and no-download coverage.
async function withNativeSourceReader(
  browser: Browser,
  read: (page: Page) => Promise<void>,
) {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    const downloads: string[] = [];
    page.on('download', (download) =>
      downloads.push(download.suggestedFilename()),
    );
    await read(page);
    expect(downloads).toEqual([]);
  } finally {
    await context.close();
  }
}

for (const example of report.examples.filter(
  (e: { files: unknown[] }) => e.files.length,
)) {
  test(`catalog ${example.id} preserves all inline files without JavaScript or downloads`, async ({
    browser,
  }) => {
    // This is a whole-bundle reading check, not a single interaction benchmark.
    test.setTimeout(30_000 + example.files.length * 2_000);
    await withNativeSourceReader(browser, async (page) => {
      await page.goto('/docs/examples/');
      const viewer = page.locator(`[data-example-source="${example.id}"]`);
      await viewer.locator(':scope > summary').click();
      for (const file of example.files) {
        const panel = viewer.locator(`[data-source-path="${file.path}"]`);
        if ((await panel.getAttribute('open')) === null)
          await panel.locator(':scope > summary').click();
        const code = panel.locator('pre');
        await expect(code).toBeVisible();
        // The no-JS document is static. Read its source properties together to
        // avoid repeated protocol trips and trace snapshots of the large catalog.
        const rendered = await code.evaluate((node) => ({
          text: node.querySelector('code')?.textContent,
          tabindex: node.getAttribute('tabindex'),
          language: node.dataset.language,
          highlighted: node.querySelectorAll('span[style]').length > 0,
          nestedMarkup: node.querySelectorAll('user, tool, developer, script')
            .length,
        }));
        expect(rendered.text).toBe(file.content);
        expect(rendered.tabindex).toBe('0');
        if (file.path.endsWith('.chatml')) {
          expect(rendered.language).toBe('ocaml');
          expect(rendered.highlighted).toBe(true);
        }
        await expect(code).toHaveAccessibleName(
          `${example.title}: ${file.path} source`,
        );
        expect(rendered.nestedMarkup).toBe(0);
        await panel.locator(':scope > summary').click();
      }
    });
  });
}

test('tutorial entrypoints preserve inline source without JavaScript or downloads', async ({
  browser,
}) => {
  await withNativeSourceReader(browser, async (page) => {
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
  });
});

test('associated guide entrypoints preserve inline source without JavaScript or downloads', async ({
  browser,
}) => {
  await withNativeSourceReader(browser, async (page) => {
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
  });
});

test('inline reader supports keyboard file expansion and horizontal source scrolling', async ({
  page,
  browserName,
}) => {
  // macOS WebKit uses Option+Tab to include buttons and links in keyboard navigation.
  const next =
    browserName === 'webkit' && process.platform === 'darwin'
      ? 'Alt+Tab'
      : 'Tab';
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto('/docs/tutorials/specialist/');
  const picker = page.locator('.example-source select');
  await picker.selectOption('docs-reviewer.chatmd');
  const companion = page.locator('[data-source-path="docs-reviewer.chatmd"]');
  await companion.locator('summary').focus();
  await page.keyboard.press('Enter');
  await expect(companion.locator('pre')).not.toBeVisible();
  await page.keyboard.press('Enter');
  await expect(companion.locator('pre')).toBeVisible();
  await page.keyboard.press(next);
  await expect(
    companion.getByRole('button', { name: 'Copy source' }),
  ).toBeFocused();
  await page.keyboard.press(next);
  await expect(
    companion.getByRole('button', { name: 'Wrap lines' }),
  ).toBeFocused();
  await page.keyboard.press(next);
  await page.keyboard.press(next);
  await page.keyboard.press(next);
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

test('catalog keeps readers collapsed and expands only one source at a time', async ({
  page,
}) => {
  await page.setViewportSize({ width: 1440, height: 950 });
  await page.goto('/docs/examples/');
  await expect(
    page.locator('.example-source[data-enhanced="true"]').first(),
  ).toBeAttached();
  await expect(page.locator('.example-source[open]')).toHaveCount(0);
  for (const id of ['guarded-engineering', 'documentation-lab']) {
    const viewer = page.locator(`[data-example-source="${id}"]`);
    await viewer.locator(':scope > summary').click();
    await viewer.getByRole('button', { name: 'Expand reader' }).click();
    await expect(
      page.locator('.example-source[data-expanded="true"]'),
    ).toHaveCount(1);
    await expect(viewer).toHaveAttribute('data-expanded', 'true');
  }
  const active = page.locator('[data-example-source="documentation-lab"]');
  await expect(page.locator('.right-sidebar-container')).not.toBeVisible();
  await active.locator(':scope > summary').click();
  await expect(page.locator('.right-sidebar-container')).toBeVisible();
});

test('expanded source preserves the selected file, wrapping, exact text and prose width', async ({
  page,
}) => {
  await page.setViewportSize({ width: 1440, height: 950 });
  await page.goto('/docs/applications/documentation-lab/');
  const viewer = page.locator('.tutorial-sources .example-source');
  await viewer.locator('[data-file-link="scripts/coordinator.chatml"]').click();
  const file = viewer.locator(
    '[data-source-path="scripts/coordinator.chatml"]',
  );
  const code = file.locator('pre');
  const original = await file.locator('pre code').textContent();
  const fragment = new URL(page.url()).hash;
  const width = (await code.boundingBox())!.width;
  const proseWidth = await page
    .locator('.sl-markdown-content')
    .evaluate((node) => node.getBoundingClientRect().width);
  await file.getByRole('button', { name: 'Wrap lines' }).click();
  const expand = viewer.getByRole('button', { name: 'Expand reader' });
  await expand.click();
  await expect(
    viewer.getByRole('button', { name: 'Restore width' }),
  ).toHaveAttribute('aria-pressed', 'true');
  await expect
    .poll(async () => (await code.boundingBox())!.width)
    .toBeGreaterThan(width + 100);
  expect(new URL(page.url()).hash).toBe(fragment);
  await expect(file).toHaveAttribute('data-wrap', 'true');
  expect(await file.locator('pre code').textContent()).toBe(original);
  expect(
    await page
      .locator('.sl-markdown-content')
      .evaluate((node) => node.getBoundingClientRect().width),
  ).toBeCloseTo(proseWidth, 0);
  expect(
    await page.evaluate(
      () => document.documentElement.scrollWidth > innerWidth,
    ),
  ).toBe(false);
  const actions = (await file.locator('.file-actions').boundingBox())!;
  expect(actions.y + actions.height).toBeLessThanOrEqual(
    (await code.boundingBox())!.y + 1,
  );
  await viewer.getByRole('button', { name: 'Restore width' }).click();
  await expect
    .poll(async () => (await code.boundingBox())!.width)
    .toBeCloseTo(width, 0);
  await page.setViewportSize({ width: 390, height: 950 });
  await expect(
    viewer.getByRole('button', { name: 'Expand reader' }),
  ).not.toBeVisible();
  await viewer.getByRole('combobox').selectOption('schemas/watch.json');
  const schema = viewer.locator('[data-source-path="schemas/watch.json"]');
  await expect(schema.locator('pre')).toBeVisible();
  await expect(file).not.toBeVisible();
  expect(await schema.locator('pre code .line span').count()).toBeGreaterThan(
    1,
  );
  const expected = report.examples
    .find((e: { id: string }) => e.id === 'documentation-lab')!
    .files.find((f: { path: string }) => f.path === 'schemas/watch.json')!;
  expect(await schema.locator('pre code').textContent()).toBe(expected.content);
  expect(
    await page.evaluate(
      () => document.documentElement.scrollWidth > innerWidth,
    ),
  ).toBe(false);
});
