import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
import fs from 'node:fs';

const report = JSON.parse(
  fs.readFileSync(
    new URL('../../.generated/content-report.json', import.meta.url),
    'utf8',
  ),
);

test('all nine TUI bridges retain every historical heading without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    const bridges = report.pages.filter(
      (p: { disposition: string }) => p.disposition === 'bridge',
    );
    expect(bridges).toHaveLength(9);
    for (const bridge of bridges) {
      await page.goto(bridge.route);
      const ids = await page
        .locator('[id]')
        .evaluateAll((nodes) => nodes.map((node) => node.id));
      for (const heading of bridge.headings) expect(ids).toContain(heading);
      const links = await page
        .locator('.sl-markdown-content a[href]')
        .evaluateAll((nodes) => nodes.map((node) => node.getAttribute('href')));
      for (const mapping of bridge.bridgeMappings)
        expect(links).toContain(mapping.target);
      await expect(page.locator('meta[name="robots"]')).toHaveAttribute(
        'content',
        'noindex, nofollow',
      );
    }
    await page.goto(
      '/docs/compatibility/chat-tui/app/#event-loop-and-streaming-architecture',
    );
    await page
      .locator(
        '.sl-markdown-content a[href="/docs/library/chat-tui/app/#event-loop-and-streaming-architecture"]',
      )
      .click();
    await expect(
      page.locator('#event-loop-and-streaming-architecture'),
    ).toBeVisible();
  } finally {
    await context.close();
  }
});

test('the framework map exposes all fourteen capabilities and reaches maintained MCP integration', async ({
  browser,
}) => {
  const context = await browser.newContext({
    javaScriptEnabled: false,
    viewport: { width: 390, height: 844 },
  });
  try {
    const page = await context.newPage();
    await page.goto('/docs/');
    await page.getByText('Explore the framework', { exact: true }).click();
    const navigation = page.getByRole('navigation', {
      name: 'Framework capabilities',
    });
    await expect(navigation.getByRole('link')).toHaveCount(14);
    await navigation
      .getByRole('link', { name: 'MCP tools and authentication' })
      .click();
    await expect(page).toHaveURL('/docs/library/mcp/client/');
    await expect(page.locator('.sl-markdown-content')).toContainText(
      'maintained MCP tool/client infrastructure',
    );
    await page.goto('/docs/library/');
    await page
      .locator('.sl-markdown-content')
      .getByRole('link', { name: 'Custom tools', exact: true })
      .click();
    await expect(page).toHaveURL('/docs/library/custom-tools/');
  } finally {
    await context.close();
  }
});

test('search distinguishes OAuth integration and compatibility session APIs', async ({
  page,
}) => {
  for (const [query, title, route] of [
    [
      'credential-isolated',
      'MCP OAuth token caching',
      '/docs/library/mcp/oauth/',
    ],
    [
      'Prompt_session',
      'Legacy prompt sessions (Prompt_session)',
      '/docs/compatibility/prompt-sessions/',
    ],
  ]) {
    await page.goto('/docs/');
    await page.getByRole('button', { name: /Search/ }).click();
    await page.getByRole('textbox').fill(query);
    const result = page
      .locator('.pagefind-ui__result-link')
      .filter({ hasText: title })
      .first();
    await expect(result).toBeVisible();
    await result.click();
    expect(new URL(page.url()).pathname).toBe(route);
  }
});

test('library tables and expanded capability navigation remain readable at 320px', async ({
  page,
}) => {
  await page.setViewportSize({ width: 320, height: 800 });
  for (const theme of ['light', 'dark'] as const) {
    await page.emulateMedia({ colorScheme: theme });
    for (const route of ['/docs/', '/docs/library/']) {
      await page.goto(route);
      if (route === '/docs/')
        await page.getByText('Explore the framework', { exact: true }).click();
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
  }
});
