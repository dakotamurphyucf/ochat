import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
import fs from 'node:fs';
const hero = JSON.parse(
  fs.readFileSync(
    new URL('../../.generated/hero.json', import.meta.url),
    'utf8',
  ),
);
test('homepage leads to the first-agent tutorial', async ({ page }) => {
  await page.goto('/');
  await expect(page.getByRole('heading', { level: 1 })).toContainText(
    'Just text files.',
  );
  await page
    .getByRole('link', { name: 'Build your first agent' })
    .first()
    .click();
  await expect(page).toHaveURL(/\/docs\/start\/first-agent\//);
  await expect(page.getByRole('heading', { level: 1 })).toHaveText(
    'Run your first local agent',
  );
  await expect(
    page.getByRole('link', { name: /View (committed )?Markdown source/ }),
  ).toHaveAttribute(
    'href',
    /blob\/[a-f0-9]{40}\/docs-src\/agent-server\/tutorials\/local-tui.md/,
  );
});
test('themes persist and docs remain usable with blocked storage', async ({
  page,
}) => {
  await page.goto('/');
  const before = await page.locator('html').getAttribute('data-theme');
  await page.getByRole('button', { name: 'Dark color theme' }).click();
  await expect(page.locator('html')).toHaveAttribute(
    'data-theme',
    before === 'dark' ? 'light' : 'dark',
  );
  await page.goto('/docs/');
  await expect(page.locator('html')).toHaveAttribute(
    'data-theme',
    before === 'dark' ? 'light' : 'dark',
  );
  await page.addInitScript(() => {
    Object.defineProperty(window, 'localStorage', {
      get() {
        throw new DOMException('blocked', 'SecurityError');
      },
    });
  });
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.reload();
  await page
    .getByRole('combobox', { name: 'Color theme' })
    .selectOption('light');
  await expect(page.locator('html')).toHaveAttribute('data-theme', 'light');
  expect(errors).toEqual([]);
});
test('search finds a language identifier and opens a documentation page', async ({
  page,
}) => {
  await page.goto('/docs/');
  await page.getByRole('button', { name: /Search/ }).click();
  const input = page.getByRole('textbox');
  await input.fill('read_file');
  const result = page.locator('.pagefind-ui__result-link').first();
  await expect(result).toBeVisible();
  await result.click();
  await expect(page).toHaveURL(/\/docs\//);
  await expect(page.getByRole('heading', { level: 1 })).toBeVisible();
});
test('search closes with Escape and returns focus', async ({ page }) => {
  await page.goto('/docs/');
  const trigger = page.getByRole('button', { name: /Search/ });
  await trigger.click();
  await page.getByRole('textbox').fill('ChatMD');
  await page.keyboard.press('Escape');
  await expect(trigger).toBeFocused();
});
test('legacy fragments and nested Markdown remain visible', async ({
  page,
}) => {
  await page.goto('/docs/compatibility/chat-tui/renderer/#layout');
  await expect(page.locator('#layout')).toBeVisible();
  await page.goto('/docs/library/webpage-markdown/md-render/');
  await expect(
    page.locator('pre').filter({ hasText: '<original-html/>' }),
  ).toContainText('```html');
});
test('mobile homepage and long reference have no body overflow', async ({
  page,
}) => {
  await page.setViewportSize({ width: 390, height: 844 });
  for (const route of [
    '/',
    '/docs/start/first-agent/',
    '/docs/reference/chatml/',
  ]) {
    await page.goto(route);
    expect(
      await page.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
    ).toBe(true);
  }
  await page.getByRole('button', { name: 'Menu', exact: true }).click();
  await expect(
    page
      .getByRole('link', { name: 'Run your first local agent', exact: true })
      .first(),
  ).toBeVisible();
});
test('homepage and first tutorial pass automated accessibility checks', async ({
  page,
}) => {
  for (const route of ['/', '/docs/start/first-agent/']) {
    await page.goto(route);
    const result = await new AxeBuilder({ page })
      .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
      .analyze();
    expect(result.violations).toEqual([]);
  }
});
test('main content and links work without JavaScript', async ({ browser }) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  const page = await context.newPage();
  await page.goto('/');
  await page
    .getByRole('link', { name: 'Build your first agent' })
    .first()
    .click();
  await expect(page.getByRole('heading', { level: 1 })).toBeVisible();
  await expect(page.locator('pre').first()).toBeVisible();
  await context.close();
});
test('homepage copy preserves the maintained agent definition', async ({
  page,
  browserName,
}) => {
  test.skip(
    browserName !== 'chromium',
    'Clipboard permission support differs; other engines cover rendered code.',
  );
  await page.context().grantPermissions(['clipboard-read', 'clipboard-write']);
  await page.goto('/');
  await page.getByRole('button', { name: 'Copy agent definition' }).click();
  expect(await page.evaluate(() => navigator.clipboard.readText())).toBe(
    hero.code,
  );
  await expect(page.getByRole('status')).toHaveText('Agent definition copied');
  await page.goto('/docs/start/first-agent/');
  const code = page.locator('.expressive-code').first();
  await code.getByRole('button', { name: 'Copy to clipboard' }).focus();
  await page.keyboard.press('Enter');
  const prompt = fs
    .readFileSync(
      new URL(
        '../../../docs-src/examples/agent-server/prompts/hello.chatmd',
        import.meta.url,
      ),
      'utf8',
    )
    .trimEnd();
  await expect
    .poll(() => page.evaluate(() => navigator.clipboard.readText()))
    .toBe(prompt);
  await expect(code.locator('[aria-live="polite"]')).toHaveText('Copied!');
  await page.goto('/');
  await page.evaluate(() =>
    Object.defineProperty(navigator.clipboard, 'writeText', {
      value: async () => {
        throw new DOMException('Blocked', 'NotAllowedError');
      },
    }),
  );
  await page.getByRole('button', { name: 'Copy agent definition' }).click();
  await expect(page.getByRole('status')).toHaveText(
    'Copy unavailable. Select and copy the code above.',
  );
  await expect(page.locator('.hero-code pre')).toBeVisible();
});

test('installation, troubleshooting, and first-agent pages form a complete reading path', async ({
  page,
}) => {
  await page.goto('/docs/start/installation/');
  await expect(
    page.getByRole('heading', { name: 'Before you start', exact: true }),
  ).toBeVisible();
  const startLinks = page
    .locator('#starlight__sidebar summary')
    .filter({ hasText: /^Start here$/ })
    .locator('..')
    .locator('a');
  await expect(startLinks).toHaveText([
    'Explore Ochat',
    'Build and configure Ochat',
    'Build troubleshooting',
    'Run your first local agent',
  ]);
  await page.getByRole('link', { name: /Next.*Build troubleshooting/ }).click();
  await expect(page).toHaveURL('/docs/start/build-troubleshooting/');
  await page
    .getByRole('link', { name: /Next.*Run your first local agent/ })
    .click();
  await expect(page).toHaveURL('/docs/start/first-agent/');
  await expect(
    page.locator('.expressive-code pre').filter({ hasText: '<developer>' }),
  ).toBeVisible();
  await expect(
    page.getByRole('heading', { name: '4. Quit cleanly', exact: true }),
  ).toBeVisible();
  await page
    .getByRole('link', { name: /Next.*Give an agent a file tool/ })
    .click();
  await expect(page).toHaveURL('/docs/tutorials/file-tool/');
});
test('Mermaid renders while its exact source remains available', async ({
  page,
}) => {
  await page.goto('/docs/library/webpage-markdown/driver/');
  const code = page.locator('pre[data-language="mermaid"]');
  await page.getByRole('button', { name: 'Show rendered diagram' }).click();
  await expect(page.locator('.ochat-diagram svg')).toBeVisible({
    timeout: 15000,
  });
  await expect(code).toContainText('flowchart TD');
});
test('dark theme and narrow reading layouts remain accessible', async ({
  page,
}) => {
  await page.setViewportSize({ width: 320, height: 800 });
  for (const route of [
    '/',
    '/docs/start/first-agent/',
    '/docs/reference/chatml/',
  ]) {
    await page.goto(route);
    await page.evaluate(
      () => (document.documentElement.dataset.theme = 'dark'),
    );
    expect(
      await page.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
    ).toBe(true);
  }
  await page.goto('/docs/start/first-agent/');
  await page.evaluate(() => (document.documentElement.dataset.theme = 'dark'));
  const result = await new AxeBuilder({ page })
    .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
    .analyze();
  expect(result.violations).toEqual([]);
});

test('homepage follows system theme changes until the reader makes a choice', async ({
  page,
}) => {
  await page.emulateMedia({ colorScheme: 'light' });
  await page.goto('/');
  const toggle = page.getByRole('button', { name: 'Dark color theme' });
  await expect(toggle).toHaveAttribute('aria-pressed', 'false');
  await page.emulateMedia({ colorScheme: 'dark' });
  await expect(page.locator('html')).toHaveAttribute('data-theme', 'dark');
  await expect(toggle).toHaveAttribute('aria-pressed', 'true');
  await toggle.click();
  await page.emulateMedia({ colorScheme: 'light' });
  await page.emulateMedia({ colorScheme: 'dark' });
  await expect(page.locator('html')).toHaveAttribute('data-theme', 'light');
  await expect(toggle).toHaveAttribute('aria-pressed', 'false');
});
