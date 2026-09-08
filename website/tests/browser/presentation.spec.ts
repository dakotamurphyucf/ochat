import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
import fs from 'node:fs';
const artifact = JSON.parse(
  fs.readFileSync(
    new URL('../../.generated/build-evidence.json', import.meta.url),
    'utf8',
  ),
);
test('static demonstration, diagram explanation and contribution links remain useful without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  const page = await context.newPage();
  await page.goto('/');
  await expect(
    page.getByText('Static source example · no recorded model output.'),
  ).toBeVisible();
  await expect(
    page.getByRole('navigation', { name: 'Agent composition' }),
  ).toBeVisible();
  await expect(
    page.getByRole('link', { name: 'Connect two agents' }),
  ).toHaveAttribute('href', '/docs/tutorials/specialist/');
  await expect(page.getByRole('link', { name: 'Contribute' })).toHaveAttribute(
    'href',
    /#documentation-and-contributing$/,
  );
  await page.goto('/docs/library/webpage-markdown/driver/');
  await expect(page.locator('pre[data-language="mermaid"]')).toContainText(
    'flowchart TD',
  );
  await expect(
    page.getByRole('navigation', { name: 'Project links' }),
  ).toBeVisible();
  await context.close();
});
test('404 adapts to both themes, reflows and provides working search and navigation', async ({
  page,
}) => {
  await page.setViewportSize({ width: 320, height: 700 });
  for (const colorScheme of ['light', 'dark'] as const) {
    await page.emulateMedia({ colorScheme });
    const response = await page.goto('/not-an-ochat-page/');
    expect(response?.status()).toBe(404);
    await expect(page.locator('html')).toHaveAttribute(
      'data-theme',
      colorScheme,
    );
    await expect(page.locator('meta[name="robots"]')).toHaveAttribute(
      'content',
      'noindex, nofollow',
    );
    await expect(page.locator('link[rel="canonical"]')).toHaveCount(0);
    expect(
      await page.evaluate(
        () => document.documentElement.scrollWidth > innerWidth,
      ),
    ).toBe(false);
    expect(
      (
        await new AxeBuilder({ page })
          .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
          .analyze()
      ).violations,
    ).toEqual([]);
  }
  await page.getByRole('button', { name: 'Search documentation' }).click();
  await page.locator('dialog input').fill('install');
  await expect(page.locator('.pagefind-ui__result-link').first()).toContainText(
    'Build and configure',
  );
  await page.keyboard.press('Escape');
  await page.getByRole('link', { name: 'Explore documentation' }).click();
  await expect(page).toHaveURL(/\/docs\/$/);
});
test('social metadata resolves to a complete image without eagerly fetching it', async ({
  page,
  request,
}) => {
  const images: string[] = [];
  page.on('request', (req) => {
    if (req.url().includes('/social/')) images.push(req.url());
  });
  await page.goto('/docs/reference/chatml/');
  await expect(page).toHaveTitle(/ChatML.*Ochat/);
  const href = await page
    .locator('meta[property="og:image"]')
    .getAttribute('content');
  expect(new URL(href!).origin).toBe(artifact.origin);
  expect(new URL(href!).pathname).toMatch(/^\/social\/.+\.png$/);
  expect(images).toEqual([]);
  const response = await request.get(new URL(href!).pathname);
  expect(response.ok()).toBe(true);
  expect(response.headers()['content-type']).toContain('image/png');
  expect((await response.body()).subarray(1, 4).toString()).toBe('PNG');
});
test('diagram is opt-in, scrollable and preserves source if renderer fails', async ({
  page,
}) => {
  const chunks: string[] = [];
  page.on('request', (req) => {
    if (req.url().includes('mermaid')) chunks.push(req.url());
  });
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto('/docs/library/webpage-markdown/driver/');
  const button = page.getByRole('button', { name: 'Show rendered diagram' });
  await button.scrollIntoViewIfNeeded();
  expect(chunks).toEqual([]);
  await page.route('**/*mermaid*.js', (route) => route.abort());
  await button.click();
  await expect(page.locator('.ochat-diagram')).toContainText(
    'Diagram unavailable',
  );
  await expect(page.locator('pre[data-language="mermaid"]')).toContainText(
    'GitHub fast-match',
  );
  await expect(
    page.getByRole('button', { name: 'Hide rendered diagram' }),
  ).toHaveAttribute('aria-expanded', 'true');
  expect(
    await page.evaluate(
      () => document.documentElement.scrollWidth > innerWidth,
    ),
  ).toBe(false);
});

test('a slow diagram load can be collapsed and finishes without reopening the figure', async ({
  page,
}) => {
  let release!: () => void;
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  await page.route('**/*mermaid*.js', async (route) => {
    await gate;
    await route.continue();
  });
  try {
    await page.goto('/docs/library/webpage-markdown/driver/');
    await page.getByRole('button', { name: 'Show rendered diagram' }).click();
    await expect(page.locator('.ochat-diagram')).toContainText(
      'Rendering the diagram',
    );
    await page.getByRole('button', { name: 'Hide rendered diagram' }).click();
    await expect(page.locator('.ochat-diagram')).toBeHidden();
    release();
    await expect(page.locator('.ochat-diagram svg')).toBeAttached();
    await expect(page.locator('.ochat-diagram')).toBeHidden();
    await page.getByRole('button', { name: 'Show rendered diagram' }).click();
    await expect(page.locator('.ochat-diagram svg')).toBeVisible();
  } finally {
    release();
  }
});
