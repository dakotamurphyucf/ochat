import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

test('application discovery filters by task and restores the filter through back navigation', async ({
  page,
}) => {
  await page.goto('/');
  await page.getByRole('link', { name: 'See what you can build' }).click();
  await expect(page).toHaveURL('/docs/applications/');
  await expect(page.locator('[data-application]:visible')).toHaveCount(6);
  await page.getByRole('button', { name: 'Research', exact: true }).click();
  await expect(page.locator('[data-application]:visible')).toHaveCount(1);
  await expect(page).toHaveURL(/category=Research/);
  await page.getByRole('button', { name: 'Code', exact: true }).click();
  await expect(page.locator('[data-application]:visible')).toHaveCount(2);
  await page.goBack();
  await expect(
    page.getByRole('button', { name: 'Research', exact: true }),
  ).toHaveAttribute('aria-pressed', 'true');
  await page.locator('[data-application="research-brief"] h3 a').click();
  await expect(
    page.getByRole('region', { name: 'Application overview' }),
  ).toContainText('Setup');
  await page.getByRole('link', { name: 'Inspect the agent files' }).click();
  await expect(page.locator('.source-file:target pre')).toBeVisible();
});

test('tutorial hub exposes the curriculum and lessons explain outcomes and next steps', async ({
  page,
}) => {
  await page.goto('/');
  await page
    .getByRole('navigation', { name: 'Main navigation' })
    .getByRole('link', { name: 'Tutorials', exact: true })
    .click();
  await expect(page).toHaveURL('/docs/tutorials/');
  const curriculum = page.getByRole('navigation', {
    name: 'Tutorial curriculum',
  });
  await expect(curriculum.locator('li')).toHaveCount(10);
  await curriculum.getByRole('link').first().click();
  await expect(
    page.getByRole('complementary', { name: 'Lesson outcome and setup' }),
  ).toContainText('You’ll build');
  await expect(
    page.getByRole('complementary', { name: 'What comes next' }),
  ).toContainText('file tool');
});

test('recording supports inspectable calls, explicit playback, source links and the original transcript', async ({
  page,
}) => {
  await page.goto('/docs/applications/documentation-review/');
  const demo = page.getByRole('region', {
    name: 'Recorded documentation review',
  });
  await expect(
    demo.getByRole('region', { name: 'Recorded review report' }),
  ).toContainText('Node');
  await demo.getByRole('button', { name: 'Execution', exact: true }).click();
  await expect(demo.locator('[data-step-panel="0"]')).toBeVisible();
  await demo.locator('[data-step="1"]').click();
  await expect(demo.locator('[data-step-panel="1"]')).toContainText(
    'documentation',
  );
  await demo.getByRole('button', { name: 'Play walkthrough' }).click();
  await expect(
    demo.getByRole('button', { name: 'Pause walkthrough' }),
  ).toBeVisible();
  await demo.getByRole('button', { name: 'Pause walkthrough' }).click();
  await demo.getByRole('button', { name: 'Agent files', exact: true }).click();
  await demo
    .getByRole('navigation', { name: 'Agent composition' })
    .getByRole('link', { name: /Documentation reviewer/ })
    .click();
  const file = page.locator('.source-file:target');
  await expect(file).toContainText('no file, editing, or shell tools');
  await expect(file.locator('pre')).toBeVisible();
  await page.reload();
  await expect(page.locator('.source-file:target pre')).toBeVisible();
  await page.setViewportSize({ width: 390, height: 844 });
  const viewer = page.locator('.tutorial-sources .example-source');
  await viewer.getByRole('combobox').selectOption('recorded-run.chatmd');
  await expect(page.locator('.source-file:target pre')).toContainText(
    'review_docs',
  );
});

test('application and learning views reflow and remain accessible in both themes', async ({
  page,
}) => {
  test.slow();
  for (const route of [
    '/docs/applications/',
    '/docs/applications/documentation-review/',
    '/docs/tutorials/',
  ]) {
    for (const colorScheme of ['light', 'dark'] as const) {
      await page.emulateMedia({ colorScheme });
      await page.setViewportSize({ width: 390, height: 844 });
      await page.goto(route);
      expect(
        (
          await new AxeBuilder({ page })
            .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
            .analyze()
        ).violations,
      ).toEqual([]);
      await page.setViewportSize({ width: 320, height: 844 });
      await page.evaluate(
        () => (document.documentElement.style.fontSize = '200%'),
      );
      expect(
        await page.evaluate(
          () => document.documentElement.scrollWidth <= innerWidth,
        ),
      ).toBe(true);
    }
  }
});

test('application evidence and source files remain readable without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    await page.goto('/docs/applications/');
    await expect(page.locator('[data-application]')).toHaveCount(6);
    await page.goto('/docs/applications/documentation-review/');
    await expect(
      page.getByRole('region', { name: 'Recorded review report' }),
    ).toBeVisible();
    await expect(page.locator('[data-step-panel="1"]')).toBeVisible();
    await page
      .locator(
        '.source-file[data-source-path="docs-reviewer.chatmd"] > summary',
      )
      .click();
    await expect(
      page.locator('.source-file[data-source-path="docs-reviewer.chatmd"] pre'),
    ).toBeVisible();
  } finally {
    await context.close();
  }
});

test('search labels learning and application results by their document kind', async ({
  page,
}) => {
  await page.goto('/');
  await page.getByRole('button', { name: 'Search documentation' }).click();
  await page.locator('dialog input').fill('documentation review');
  const result = page.locator('dialog li').filter({
    has: page.getByRole('link', {
      name: 'Find the gaps in your documentation',
      exact: true,
    }),
  });
  await expect(result.locator('.search-context')).toContainText('Guide');
});

test('application filters keep their positions when the web font loads', async ({
  page,
}) => {
  await page.setViewportSize({ width: 390, height: 844 });
  let releaseFonts!: () => void;
  const fontsReady = new Promise<void>((resolve) => {
    releaseFonts = resolve;
  });
  await page.route('**/*.woff2', async (route) => {
    await fontsReady;
    await route.continue();
  });
  try {
    await page.goto('/docs/applications/', { waitUntil: 'domcontentloaded' });
    const filters = page.getByRole('group', { name: 'Filter applications' });
    await expect(filters).toBeVisible();
    const positions = () =>
      filters.evaluate((group) => {
        const origin = group.getBoundingClientRect();
        return [...group.querySelectorAll('button')].map((button) => {
          const box = button.getBoundingClientRect();
          return {
            x: box.x - origin.x,
            y: box.y - origin.y,
            width: box.width,
            height: box.height,
          };
        });
      });
    const before = await positions();
    releaseFonts();
    await page.evaluate(() => document.fonts.ready);
    const after = await positions();
    for (let i = 0; i < before.length; i++) {
      for (const key of ['x', 'y', 'width', 'height'] as const) {
        expect(
          Math.abs(after[i][key] - before[i][key]),
          `Filter ${i} ${key}`,
        ).toBeLessThan(1);
      }
    }
  } finally {
    releaseFonts();
  }
});
