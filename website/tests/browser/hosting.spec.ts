import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

test('hosting tutorials share local setup and lead to configuration and transport contracts', async ({
  page,
}) => {
  await page.goto('/docs/guides/agent-server/');
  await page
    .locator('.sl-markdown-content')
    .getByRole('link', { name: 'start a Unix daemon and connect the TUI' })
    .click();
  await expect(page).toHaveURL('/docs/tutorials/unix-daemon/');
  await page
    .locator('.sl-markdown-content')
    .getByRole('link', { name: 'example setup', exact: true })
    .click();
  await expect(page).toHaveURL('/docs/examples/agent-server/');
  await expect(page.locator('.sl-markdown-content')).toContainText(
    'Setup refuses a nonempty directory',
  );
  await expect(
    page
      .locator('.sl-markdown-content')
      .getByRole('link', { name: 'hello prompt' }),
  ).toHaveAttribute('href', '/downloads/hello/hello.chatmd');
  await page
    .getByRole('link', { name: /Next.*Run a private Unix daemon/ })
    .click();
  await page.getByRole('link', { name: /Next.*background/ }).click();
  await page.getByRole('link', { name: /Next.*stdio client/ }).click();
  await page.getByRole('link', { name: /Next.*HTTP client/ }).click();
  await expect(page).toHaveURL('/docs/tutorials/http-client/');
  await page
    .locator('.sl-markdown-content')
    .getByRole('link', { name: 'route/header and SSE reference' })
    .click();
  await expect(page).toHaveURL('/docs/reference/agent-server/transports/http/');
  await page
    .locator('.sl-markdown-content')
    .getByRole('link', { name: 'configuration', exact: true })
    .click();
  await expect(page).toHaveURL('/docs/reference/agent-server/configuration/');
});

test('protocol anchors and source excerpts remain usable without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    await page.goto('/docs/reference/agent-server/protocol-types/#session');
    const section = page.locator('#session');
    await expect(section).toBeVisible();
    expect(
      await section.evaluate((element) => element.getBoundingClientRect().top),
    ).toBeGreaterThan(60);
    await expect(
      page.locator(
        '.sl-markdown-content a[href$="/lib/agent_protocol/session.ml"]',
      ),
    ).toHaveText('JSON codec');
    await expect(
      page.locator('pre').filter({ hasText: 'module Delete_history_request' }),
    ).toContainText('expected_revision : int64');
    await expect(page.locator('#workspace')).toBeAttached();
  } finally {
    await context.close();
  }
});

test('dense protocol references reflow and retain keyboard scrolling in both themes', async ({
  page,
}) => {
  // Full axe scans traverse ~19,000 syntax-highlighted DOM elements twice.
  // Keep the complete scan; this timeout is not a page-load performance budget.
  test.slow();
  await page.setViewportSize({ width: 320, height: 800 });
  for (const theme of ['light', 'dark'] as const) {
    await page.emulateMedia({ colorScheme: theme });
    await page.goto('/docs/reference/agent-server/protocol-types/#session');
    await expect(page.locator('html')).toHaveAttribute('data-theme', theme);
    expect(
      await page.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
    ).toBe(true);
    const block = page
      .locator('pre')
      .filter({ hasText: 'module Delete_history_request' });
    await block.focus();
    await expect(block).toBeFocused();
    const before = await block.evaluate((element) => element.scrollLeft);
    await page.keyboard.press('ArrowRight');
    await expect
      .poll(() => block.evaluate((element) => element.scrollLeft))
      .toBeGreaterThan(before);
    const result = await new AxeBuilder({ page })
      .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
      .analyze();
    expect(result.violations).toEqual([]);
  }
});

test('protocol method search reaches the published contract and current daemon flags are visible', async ({
  page,
}) => {
  await page.goto('/docs/');
  await page.getByRole('button', { name: /Search/ }).click();
  await page.getByRole('textbox').fill('session.delete_history');
  const result = page
    .locator('.pagefind-ui__result-link')
    .filter({ hasText: 'Agent protocol' })
    .first();
  await expect(result).toBeVisible();
  await result.click();
  await expect(page).toHaveURL(/\/docs\/reference\/agent-server\/protocol\//);
  await page.goto(
    '/docs/reference/agent-server/operator-contracts/#ochat_agent_serverml-flag-inventory',
  );
  await expect(page.locator('.sl-markdown-content')).toContainText(
    '-validate-only',
  );
  await expect(page.locator('.sl-markdown-content')).toContainText(
    'Generated help/version options',
  );
});
