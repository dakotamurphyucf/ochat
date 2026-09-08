import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

test('selected Graphite identity ignores old palette settings across pages and themes', async ({
  page,
}) => {
  await page.addInitScript(() => {
    localStorage.setItem('ochat-palette-preview', 'stone');
    localStorage.setItem('starlight-theme', 'light');
  });
  await page.goto('/?palette=stone');
  await expect(
    page.getByRole('complementary', { name: 'Design comparison' }),
  ).toHaveCount(0);
  await expect(page.locator('html')).not.toHaveAttribute('data-palette');
  expect(
    await page.evaluate(() =>
      getComputedStyle(document.documentElement)
        .getPropertyValue('--ochat-accent')
        .trim(),
    ),
  ).toBe('#245bd6');
  const original = await page.locator('.hero-code code').textContent();
  await page.getByRole('button', { name: 'Wrap agent code' }).click();
  await expect(
    page.getByRole('button', { name: 'Wrap agent code' }),
  ).toHaveAttribute('aria-pressed', 'false');
  expect(await page.locator('.hero-code code').textContent()).toBe(original);
  await page
    .getByRole('link', { name: 'Build your first agent' })
    .first()
    .click();
  for (const theme of ['light', 'dark']) {
    await page
      .getByRole('combobox', { name: 'Color theme', exact: true })
      .selectOption(theme);
    await expect(page.locator('html')).toHaveAttribute('data-theme', theme);
    expect(
      await page.evaluate(
        () => getComputedStyle(document.body).backgroundColor,
      ),
    ).toBe(theme === 'light' ? 'rgb(255, 255, 255)' : 'rgb(18, 21, 27)');
    expect(
      (
        await new AxeBuilder({ page })
          .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
          .analyze()
      ).violations,
    ).toEqual([]);
  }
  const readingTop = await page
    .locator('.sl-markdown-content')
    .evaluate((el) => el.getBoundingClientRect().top + scrollY);
  const resourcesTop = await page
    .locator('#lesson-resources')
    .evaluate((el) => el.getBoundingClientRect().top + scrollY);
  expect(resourcesTop).toBeGreaterThan(readingTop);
  await page
    .getByRole('link', { name: 'View example files & verification' })
    .click();
  await expect(page).toHaveURL(/#lesson-resources$/);
  for (const width of [390, 320]) {
    await page.setViewportSize({ width, height: 844 });
    await page.evaluate(() => {
      document.documentElement.style.fontSize = '200%';
    });
    expect(
      await page.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
    ).toBe(true);
  }
});

test('selected design keeps navigation and fragment reading usable with blocked storage', async ({
  page,
}) => {
  await page.addInitScript(() => {
    Object.defineProperty(window, 'localStorage', {
      get() {
        throw new DOMException('blocked', 'SecurityError');
      },
    });
  });
  const errors: string[] = [];
  page.on('pageerror', (error) => errors.push(error.message));
  await page.goto(
    '/docs/start/first-agent/?palette=stone#1-read-the-agent-definition',
  );
  await expect(
    page.getByRole('complementary', { name: 'Design comparison' }),
  ).toHaveCount(0);
  await expect(page).toHaveURL(/#1-read-the-agent-definition$/);
  await page.setViewportSize({ width: 320, height: 844 });
  await page.getByRole('button', { name: 'Menu', exact: true }).click();
  await page
    .getByRole('combobox', { name: 'Color theme', exact: true })
    .selectOption('dark');
  await expect(page.locator('html')).toHaveAttribute('data-theme', 'dark');
  await page.keyboard.press('Escape');
  expect(errors).toEqual([]);
});

test('source picker and wrapping keep exact inline file contents and readable copy failure', async ({
  page,
}) => {
  await page.goto('/docs/tutorials/workflow/');
  const source = page.locator('.tutorial-sources .example-source').first();
  const target = source.locator('.source-file[data-source-path$=".chatml"]');
  const file = await target.getAttribute('data-source-path');
  await source.locator('[data-file-link]').filter({ hasText: file! }).click();
  const code = target.locator('pre code');
  await expect(code).toBeVisible();
  const original = await code.textContent();
  await expect(target.locator('pre')).toHaveAttribute('data-language', 'ocaml');
  await target.getByRole('button', { name: 'Wrap lines' }).click();
  await expect(target).toHaveAttribute('data-wrap', 'true');
  expect(await code.textContent()).toBe(original);
  await page.evaluate(() => {
    Object.defineProperty(navigator, 'clipboard', {
      value: {
        writeText: async () => {
          throw new Error('unavailable');
        },
      },
    });
  });
  await target.getByRole('button', { name: 'Copy source' }).click();
  await expect(source.getByRole('status')).toContainText(
    'Select and copy the source text',
  );
  await expect(code).toBeVisible();
});
