import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

test('documentation paths lead through search setup to labeled sample output', async ({
  page,
}) => {
  await page.goto('/docs/');
  const paths = page.getByRole('navigation', { name: 'Documentation paths' });
  await expect(paths.getByRole('link')).toHaveCount(4);
  await paths.getByRole('link', { name: /Build something useful/ }).click();
  await page.locator('[data-application="research-brief"] h3 a').click();
  await page
    .locator('.sl-markdown-content')
    .getByRole('link', { name: 'Markdown indexing and retrieval' })
    .click();
  await expect(page).toHaveURL('/docs/guides/search-and-indexing/');
  await page
    .locator('.sl-markdown-content')
    .getByRole('link', { name: /ochat query.*example/ })
    .click();
  await expect(page).toHaveURL(
    '/docs/guides/search-and-indexing/examples/code/',
  );
  await expect(page.locator('.sl-markdown-content')).toContainText(
    'illustrative',
  );
  await expect(
    page.locator('pre').filter({ hasText: '**Result 1:**' }),
  ).toContainText('```ocaml');
  await expect(
    page.getByRole('link', { name: /Next.*Search and indexing/ }),
  ).toBeVisible();
});

test('new topic pages and docs paths reflow in both themes and pass axe', async ({
  page,
}) => {
  await page.setViewportSize({ width: 320, height: 800 });
  for (const route of [
    '/docs/',
    '/docs/reference/agent-server/environment/',
    '/docs/operations/',
  ]) {
    await page.goto(route);
    for (const theme of ['light', 'dark']) {
      await page.emulateMedia({ colorScheme: theme as 'light' | 'dark' });
      await expect(page.locator('html')).toHaveAttribute('data-theme', theme);
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

test('docs paths and hosting links work without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    await page.goto('/docs/');
    await page
      .getByRole('navigation', { name: 'Documentation paths' })
      .getByRole('link', { name: /Build something useful/ })
      .click();
    await page.locator('[data-application="background-workflow"] h3 a').click();
    await page
      .locator('.sl-markdown-content')
      .getByRole('link', { name: 'agent hosting overview' })
      .click();
    await expect(page.getByRole('heading', { level: 1 })).toHaveText(
      'Agent hosting',
    );
    await page
      .locator('.sl-markdown-content')
      .getByRole('link', { name: /Operations/ })
      .click();
    await expect(page).toHaveURL('/docs/operations/');
    await expect(
      page.getByRole('heading', { name: 'Backup and restore' }),
    ).toBeVisible();
    await expect(
      page
        .locator('.sl-markdown-content')
        .getByRole('link', { name: 'Troubleshooting', exact: true }),
    ).toHaveAttribute('href', '/docs/start/troubleshooting/');
  } finally {
    await context.close();
  }
});

test('website search finds newly published environment documentation', async ({
  page,
}) => {
  await page.goto('/docs/');
  await page.getByRole('button', { name: /Search/ }).click();
  await page.getByRole('textbox').fill('OCHAT_OPENAI_IDLE_TIMEOUT_SECONDS');
  const result = page
    .locator('.pagefind-ui__result-link')
    .filter({ hasText: 'Environment settings' });
  await expect(result).toBeVisible();
  await result.click();
  await expect(page).toHaveURL(
    /\/docs\/reference\/agent-server\/environment\//,
  );
});
