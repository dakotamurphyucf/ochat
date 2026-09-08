import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

test('batch preparation and continuation remain distinct at the historical anchor', async ({
  page,
}) => {
  await page.goto(
    '/docs/reference/commands/chat-completion/#130-second-smoke-test',
  );
  await expect(page.locator('[id="130-second-smoke-test"]')).toBeAttached();
  const preparation = page.locator('pre').filter({ hasText: 'mktemp -d' });
  await expect(preparation).toContainText(
    'cp docs-src/examples/agent-server/prompts/hello.chatmd "$OCHAT_BATCH/prompt.chatmd"',
  );
  await expect(preparation).toContainText('<user>Greet a new Ochat user');
  const continuation = page
    .locator('pre')
    .filter({ hasText: 'Now describe ChatMD' });
  await expect(continuation).toContainText('chat-completion -output-file');
  await expect(continuation).not.toContainText('-prompt-file');
  await expect(page.locator('.sl-markdown-content')).toContainText(
    'every invocation with this flag appends it again',
  );
  await page
    .locator('.sl-markdown-content')
    .getByRole('link', {
      name: 'ochat shell runtime management',
    })
    .click();
  await expect(page).toHaveURL('/docs/reference/commands/shell-management/');
  await expect(page.locator('.sl-markdown-content')).toContainText(
    'Legacy Session_store management does not accept daemon IDs',
  );
});

test('shell readers can follow runtime, tools, security and persistence without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    await page.goto('/docs/concepts/shell-access/');
    await page
      .locator('.sl-markdown-content')
      .getByRole('link', {
        name: 'Runtime reference',
        exact: true,
      })
      .click();
    await expect(page).toHaveURL('/docs/reference/shell-runtime/');
    await page.getByRole('link', { name: /Next.*Declare shell tools/ }).click();
    await expect(page).toHaveURL('/docs/reference/shell-tools/');
    await page.getByRole('link', { name: /Next.*Shell security/ }).click();
    await expect(page).toHaveURL('/docs/guides/shell-security/');
    await expect(page.locator('#sanitized-live-progress')).toBeAttached();
    await page
      .getByRole('link', { name: /Next.*Shell persistence and audit/ })
      .click();
    await expect(page).toHaveURL('/docs/guides/shell-persistence/');
    await page.getByRole('link', { name: /Next.*Manage shell state/ }).click();
    await expect(page).toHaveURL('/docs/reference/commands/shell-management/');
  } finally {
    await context.close();
  }
});

test('shell environment search reaches the published management reference', async ({
  page,
}) => {
  await page.goto('/docs/');
  await page.getByRole('button', { name: /Search/ }).click();
  await page.getByRole('textbox').fill('OCHAT_SHELL_SIGNATURE_AUDIENCE');
  const result = page
    .locator('.pagefind-ui__result-link')
    .filter({ hasText: 'Manage shell state' })
    .first();
  await expect(result).toBeVisible();
  await result.click();
  await expect(page).toHaveURL(
    /\/docs\/reference\/commands\/shell-management\//,
  );
});

for (const route of [
  '/docs/reference/commands/chat-completion/',
  '/docs/guides/shell-extensions/',
]) {
  test(`shell content reflows and passes accessibility checks in both themes: ${route}`, async ({
    page,
  }) => {
    await page.setViewportSize({ width: 320, height: 800 });
    for (const theme of ['light', 'dark'] as const) {
      await page.emulateMedia({ colorScheme: theme });
      await page.goto(route);
      await expect(page.locator('html')).toHaveAttribute('data-theme', theme);
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
  });
}
