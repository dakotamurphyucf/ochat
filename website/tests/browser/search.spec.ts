import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
// Gate the worker entry: Firefox's Playwright network interception does not
// observe dynamic import() requests inside module workers. Fetches remain observable.

test('homepage and docs share immediate focus, keyboard shortcuts, and close restoration', async ({
  page,
}) => {
  for (const route of ['/', '/docs/']) {
    await page.goto(route);
    const trigger = page.getByRole('button', {
      name: 'Search documentation',
      exact: true,
    });
    await trigger.focus();
    await page.keyboard.press('Control+k');
    await expect(page.getByRole('textbox')).toBeFocused();
    await expect(page.locator('dialog')).toHaveAccessibleName(
      'Search documentation',
    );
    await page.keyboard.press('Control+k');
    await expect(page.locator('dialog')).toBeVisible(); // Typing controls keep their native shortcut.
    await page.keyboard.press('Escape');
    await expect(trigger).toBeFocused();
    await trigger.click();
    await page
      .getByRole('button', { name: 'Close search', exact: true })
      .click();
    await expect(trigger).toBeFocused();
  }
});

test('index loads only for a nonempty query and loading keeps a usable input and fallback', async ({
  page,
}) => {
  let requested = false;
  let release!: () => void;
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  await page.route('**/_astro/pagefind-worker-*.js', async (route) => {
    requested = true;
    await gate;
    await route.continue();
  });
  await page.goto('/');
  await page.waitForLoadState('networkidle');
  expect(requested).toBe(false);
  await page
    .getByRole('button', { name: 'Search documentation', exact: true })
    .click();
  await expect(page.locator('dialog').getByRole('status')).toHaveText(
    'Search by topic, command, or code identifier.',
  );
  expect(requested).toBe(false);
  await page.getByRole('textbox').fill('install');
  await expect.poll(() => requested).toBe(true);
  await expect(page.locator('dialog').getByRole('status')).toHaveText(
    'Searching documentation…',
  );
  await expect(page.getByRole('textbox')).toBeFocused();
  await expect(
    page.locator('dialog').getByRole('link', { name: 'Browse all docs' }),
  ).toBeVisible();
  release();
  await expect(page.locator('site-search')).toHaveAttribute(
    'data-state',
    'results',
  );
  await expect(
    page.locator('.pagefind-ui__result-link').first(),
  ).toHaveAttribute('href', '/docs/start/installation/');
});

test('literal identifiers, current guidance, compatibility labels and heading links stay useful', async ({
  page,
}) => {
  await page.goto('/docs/');
  await page
    .getByRole('button', { name: 'Search documentation', exact: true })
    .click();
  const input = page.getByRole('textbox');
  await input.fill('${workspace}');
  await expect(
    page
      .locator('.pagefind-ui__result-link')
      .filter({ hasText: 'ChatMD language reference' }),
  ).toBeVisible();
  await input.fill('MCP');
  await expect(
    page
      .locator('.pagefind-ui__result-link')
      .filter({ hasText: 'MCP clients' }),
  ).toBeVisible();
  await expect(
    page.locator('#ochat-search-results > li').first(),
  ).not.toContainText('Compatibility');
  await input.fill('Prompt_session');
  await expect(page.locator('#ochat-search-results')).toContainText(
    'Compatibility',
  );
  await expect(page.locator('#ochat-search-results')).toContainText(
    'different owners',
  );
  await input.fill('read_file');
  await expect(page.locator('.pagefind-ui__result-link').first()).toHaveText(
    'Built-in tool catalog',
  );
  const heading = page
    .locator('.search-sections a')
    .filter({ hasText: '5.2 Configured read_file roots' });
  await expect(heading).toBeVisible();
  const href = await heading.getAttribute('href');
  await heading.click();
  await expect(page).toHaveURL(href!);
  await expect(
    page.locator('[id="52-configured-read_file-roots"]'),
  ).toBeVisible();
});

test('results retain query on back navigation and support Enter, more results, and clear', async ({
  page,
}) => {
  await page.goto('/');
  await page
    .getByRole('button', { name: 'Search documentation', exact: true })
    .click();
  await page.getByRole('textbox').fill('install');
  await expect(page.locator('site-search')).toHaveAttribute(
    'data-state',
    'results',
  );
  await page.getByRole('button', { name: 'Show more results' }).click();
  await expect(page.locator('#ochat-search-results > li')).toHaveCount(10);
  await expect(
    page.locator('#ochat-search-results > li').nth(5).locator('a').first(),
  ).toBeFocused();
  await page.getByRole('textbox').focus();
  await page.keyboard.press('Enter');
  await expect(page).toHaveURL('/docs/start/installation/');
  await page.goBack();
  await expect(page.getByRole('textbox')).toHaveValue('install');
  await expect(page.getByRole('textbox')).toBeFocused();
  await page.getByRole('button', { name: 'Clear search', exact: true }).click();
  await expect(page.getByRole('textbox')).toHaveValue('');
  await expect(page.locator('#ochat-search-results > li')).toHaveCount(0);
  await expect(page.locator('dialog').getByRole('status')).toHaveText(
    'Search by topic, command, or code identifier.',
  );
});

test('no-results and clearing an in-flight search do not restore obsolete results', async ({
  page,
}) => {
  let requested = false;
  let release!: () => void;
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  await page.route('**/_astro/pagefind-worker-*.js', async (route) => {
    requested = true;
    await gate;
    await route.continue();
  });
  await page.goto('/docs/');
  await page
    .getByRole('button', { name: 'Search documentation', exact: true })
    .click();
  await page.getByRole('textbox').fill('install');
  await expect(page.locator('site-search')).toHaveAttribute(
    'data-state',
    'loading',
  );
  await expect.poll(() => requested).toBe(true);
  await page.getByRole('button', { name: 'Clear search', exact: true }).click();
  release();
  await page.getByRole('textbox').fill('"zzzzqvxk"');
  await expect(page.locator('site-search')).toHaveAttribute(
    'data-state',
    'empty',
  );
  await expect(page.locator('dialog').getByRole('status')).toContainText(
    'No results',
  );
  await expect(page.locator('#ochat-search-results > li')).toHaveCount(0);
  await page
    .locator('dialog')
    .getByRole('link', { name: 'Browse all docs' })
    .click();
  await expect(page).toHaveURL('/docs/');
});

for (const asset of ['module', 'index', 'fragment'] as const) {
  test(`failed ${asset} loading gives a recoverable error and navigation`, async ({
    page,
  }) => {
    const pattern =
      asset === 'module'
        ? '**/_astro/pagefind-worker-*.js'
        : asset === 'index'
          ? '**/*.pf_index'
          : '**/*.pf_fragment';
    await page.route(pattern, (route) => route.abort());
    await page.goto('/docs/');
    await page
      .getByRole('button', { name: 'Search documentation', exact: true })
      .click();
    await page.getByRole('textbox').fill('install');
    await expect(page.locator('site-search')).toHaveAttribute(
      'data-state',
      'error',
      { timeout: 16000 },
    );
    await expect(page.locator('dialog').getByRole('status')).toContainText(
      'Search could not load',
    );
    await expect(
      page.locator('dialog').getByRole('link', { name: 'Browse all docs' }),
    ).toBeVisible();
    await page.unroute(pattern);
    await page.getByRole('button', { name: 'Retry search' }).click();
    await expect(page.locator('site-search')).toHaveAttribute(
      'data-state',
      'results',
      { timeout: 16000 },
    );
    await expect(page.getByRole('textbox')).toBeFocused();
  });
}

test('query and excerpt markup remain text and unsafe result URLs are rejected', async ({
  page,
}) => {
  const literal = '<img src=x onerror="window.searchInjected=true">';
  let maliciousUrl = false;
  await page.route('**/_astro/pagefind-worker-*.js', (route) =>
    route.fulfill({
      contentType: 'text/javascript',
      body: `self.onmessage = ({data:{id,method}}) => {
      const payload = ${JSON.stringify({ url: maliciousUrl ? 'javascript:alert(1)' : '/docs/', meta: { title: literal, section: 'Reference', status: 'Current' }, plain_excerpt: '&lt;tool name=&quot;read_file&quot;/&gt;', sub_results: [] })};
      self.postMessage({id,result:method === 'search' ? [{sequence:1,index:0}] : method === 'data' ? payload : null});
    };`,
    }),
  );
  await page.goto('/');
  await page
    .getByRole('button', { name: 'Search documentation', exact: true })
    .click();
  await page.getByRole('textbox').fill(literal);
  await expect(page.locator('.pagefind-ui__result-link')).toHaveText(literal);
  await expect(page.locator('.pagefind-ui__result-excerpt')).toHaveText(
    '<tool name="read_file"/>',
  );
  await expect(
    page.locator('dialog img, dialog script, dialog tool'),
  ).toHaveCount(0);
  expect(
    await page.evaluate(
      () => (window as unknown as { searchInjected?: boolean }).searchInjected,
    ),
  ).toBeUndefined();
  maliciousUrl = true;
  await page.reload();
  await page
    .getByRole('button', { name: 'Search documentation', exact: true })
    .click();
  await page.getByRole('textbox').fill('unsafe result');
  await expect(page.locator('site-search')).toHaveAttribute(
    'data-state',
    'error',
  );
  await expect(page.locator('#ochat-search-results a')).toHaveCount(0);
});

test('search states reflow with reduced viewport, increased text, both themes, and keyboard focus containment', async ({
  page,
}) => {
  test.slow();
  for (const theme of ['light', 'dark'] as const) {
    await page.emulateMedia({ colorScheme: theme, reducedMotion: 'reduce' });
    await page.setViewportSize({ width: 320, height: 480 });
    await page.goto('/');
    await page.evaluate(() => {
      document.documentElement.style.fontSize = '200%';
    });
    await page
      .getByRole('button', { name: 'Search documentation', exact: true })
      .click();
    for (const query of ['', 'read_file', '"zzzzqvxk"']) {
      await page.getByRole('textbox').fill(query);
      await expect(page.locator('site-search')).toHaveAttribute(
        'data-state',
        query === '' ? 'initial' : query === 'read_file' ? 'results' : 'empty',
      );
      expect(
        await page
          .locator('dialog')
          .evaluate((el) => el.scrollWidth <= el.clientWidth),
      ).toBe(true);
      expect(
        await page.evaluate(
          () => document.documentElement.scrollWidth <= innerWidth,
        ),
      ).toBe(true);
      const scan = await new AxeBuilder({ page })
        .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
        .analyze();
      expect(scan.violations).toEqual([]);
    }
    for (let i = 0; i < 8; i++) {
      await page.keyboard.press('Tab');
      expect(
        await page.evaluate(() => !!document.activeElement?.closest('dialog')),
      ).toBe(true);
    }
    await page.keyboard.press('Escape');
    await expect(
      page.getByRole('button', { name: 'Search documentation', exact: true }),
    ).toBeFocused();
  }
});

test('navigation fallback remains available without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    for (const route of ['/', '/docs/']) {
      await page.goto(route);
      await expect(
        page
          .locator('site-search')
          .getByRole('link', { name: 'Browse docs', exact: true }),
      ).toBeVisible();
      await page
        .locator('site-search')
        .getByRole('link', { name: 'Browse docs', exact: true })
        .click();
      await expect(page).toHaveURL('/docs/');
    }
  } finally {
    await context.close();
  }
});
