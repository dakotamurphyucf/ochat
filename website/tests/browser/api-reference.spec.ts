import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

test('OCaml entry explains API deferral and exposes current source contracts', async ({
  page,
}) => {
  await page.goto('/docs/integrations/ocaml/');
  const article = page.locator('.sl-markdown-content');
  await expect(article).toContainText(
    'Generated OCaml API pages are deferred for this release.',
  );
  const interfaces = article.getByRole('link', {
    name: 'interface',
    exact: true,
  });
  await expect(interfaces).toHaveCount(9);
  for (const link of await interfaces.all())
    await expect(link).toHaveAttribute(
      'href',
      /https:\/\/github.com\/dakotamurphyucf\/ochat\/blob\/[a-f0-9]{40}\/lib\/agent_[^#]+\.mli$/,
    );
  await expect(
    article.getByRole('link', { name: 'library overview', exact: true }),
  ).toHaveAttribute('href', '/docs/library/');
  await expect(page.locator('a[href^="/api/"]')).toHaveCount(0);
  await expect(article.locator('pre')).toContainText('dune build @doc');
  expect((await new AxeBuilder({ page }).analyze()).violations).toEqual([]);
  await page.setViewportSize({ width: 320, height: 800 });
  await page.evaluate(() => {
    document.documentElement.style.fontSize = '200%';
  });
  expect(
    await page.evaluate(
      () => document.documentElement.scrollWidth <= innerWidth,
    ),
  ).toBe(true);
  const response = await page.goto('/api/');
  expect(response?.status()).toBe(404);
  await expect(page.locator('meta[name="robots"]')).toHaveAttribute(
    'content',
    /noindex/,
  );
});
