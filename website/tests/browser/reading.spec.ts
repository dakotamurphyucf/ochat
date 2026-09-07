import { test, expect } from '@playwright/test';

const routes = ['/', '/docs/start/first-agent/', '/docs/reference/chatml/'];

test('skip links move keyboard navigation into the main content', async ({
  page,
  browserName,
}) => {
  const tab =
    browserName === 'webkit' && process.platform === 'darwin'
      ? 'Alt+Tab'
      : 'Tab';
  for (const route of routes) {
    await page.goto(route);
    await page.keyboard.press(tab);
    const skip = page.getByRole('link', { name: 'Skip to content' });
    await expect(skip).toBeFocused();
    await page.keyboard.press('Enter');
    await page.keyboard.press(tab);
    expect(
      await page.evaluate(() => !!document.activeElement?.closest('main')),
    ).toBe(true);
  }
});

test('mobile menu moves focus, protects content, and closes before search', async ({
  page,
}) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto('/docs/start/first-agent/');
  const menu = page.getByRole('button', { name: 'Menu', exact: true });
  await menu.focus();
  await page.keyboard.press('Enter');
  await expect(
    page.locator('#starlight__sidebar [aria-current="page"]'),
  ).toBeFocused();
  await expect(page.locator('.main-frame')).toHaveAttribute('inert', '');
  for (let i = 0; i < 25; i++) {
    await page.keyboard.press('Tab');
    expect(
      await page.evaluate(() => !!document.activeElement?.closest('main')),
    ).toBe(false);
  }
  await page.keyboard.press('Escape');
  // Reopen and close from inside the menu to verify the return target.
  await menu.click();
  await expect(
    page.locator('#starlight__sidebar [aria-current="page"]'),
  ).toBeFocused();
  await page.keyboard.press('Escape');
  await expect(menu).toBeFocused();
  await expect(page.locator('.main-frame')).not.toHaveAttribute('inert');
  await menu.click();
  const search = page.getByRole('button', { name: /Search/ });
  await search.click();
  await expect(page.locator('#starlight__sidebar')).not.toBeVisible();
  await expect(page.getByRole('textbox')).toBeFocused();
  await page.keyboard.press('Escape');
  await expect(search).toBeFocused();
});

test('mobile contents is in flow and heading links clear the fixed header', async ({
  page,
}) => {
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto('/docs/start/first-agent/');
  const summary = page.locator('mobile-starlight-toc summary');
  const title = page.getByRole('heading', { level: 1 });
  const before = (await title.boundingBox())!.y;
  await summary.focus();
  await page.keyboard.press('Enter');
  expect((await title.boundingBox())!.y).toBeGreaterThan(before + 50);
  const link = page
    .locator('mobile-starlight-toc a')
    .filter({ hasText: '4. Quit cleanly' });
  await link.focus();
  await page.keyboard.press('Enter');
  await expect(page).toHaveURL(/#4-quit-cleanly$/);
  const heading = page.locator('[id="4-quit-cleanly"]');
  expect((await heading.boundingBox())!.y).toBeGreaterThanOrEqual(72);
});

test('200 percent text and narrow reflow retain all page content', async ({
  page,
}) => {
  for (const width of [1280, 640, 320]) {
    await page.setViewportSize({ width, height: 800 });
    for (const route of routes) {
      await page.goto(route);
      await page.evaluate(() => {
        document.documentElement.style.fontSize = '200%';
      });
      expect(
        await page.evaluate(
          () => document.documentElement.scrollWidth <= innerWidth,
        ),
        `${route} at ${width}px with enlarged text`,
      ).toBe(true);
      await expect(page.getByRole('heading', { level: 1 })).toBeVisible();
    }
  }
});

test('wide code and tables can scroll with the keyboard', async ({ page }) => {
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto('/docs/reference/chatml/');
  for (const selector of ['pre', 'table']) {
    if (selector === 'table') await page.goto('/docs/reference/tools/');
    await page.evaluate(() => document.fonts.ready);
    const target = page.locator(
      selector === 'table' ? '.table-scroll' : selector,
    );
    let found = false;
    for (let i = 0; i < (await target.count()); i++) {
      const el = target.nth(i);
      if (!(await el.evaluate((e) => e.scrollWidth > e.clientWidth + 20)))
        continue;
      await el.scrollIntoViewIfNeeded();
      await el.focus();
      await expect(el).toBeFocused();
      await page.keyboard.press('ArrowRight');
      await expect
        .poll(() => el.evaluate((e) => e.scrollLeft))
        .toBeGreaterThan(0);
      found = true;
      break;
    }
    expect(found, `Expected a wide ${selector} fixture`).toBe(true);
  }
});

test('reduced motion and forced colors keep primary controls usable', async ({
  page,
}) => {
  await page.emulateMedia({ reducedMotion: 'reduce', forcedColors: 'active' });
  await page.goto('/');
  const primary = page
    .getByRole('link', { name: 'Build your first agent' })
    .first();
  await primary.focus();
  const style = await primary.evaluate((el) => {
    const s = getComputedStyle(el);
    return {
      outline: s.outlineStyle,
      width: parseFloat(s.outlineWidth),
      animation: s.animationName,
      transition: s.transitionDuration,
    };
  });
  expect(style.outline).not.toBe('none');
  expect(style.width).toBeGreaterThanOrEqual(2);
  expect(style.animation).toBe('none');
  expect(style.transition).toBe('0s');
  await page.keyboard.press('Enter');
  await expect(page).toHaveURL(/first-agent/);
});

test('mobile navigation and contents work without JavaScript', async ({
  browser,
}) => {
  const context = await browser.newContext({
    javaScriptEnabled: false,
    viewport: { width: 320, height: 800 },
  });
  const page = await context.newPage();
  await page.goto('/docs/start/first-agent/');
  await page.getByRole('button', { name: 'Menu', exact: true }).click();
  const installation = page
    .locator('#starlight__sidebar')
    .getByRole('link', { name: 'Build and configure Ochat' });
  await installation.click();
  await expect(page).toHaveURL(/installation/);
  await page.locator('mobile-starlight-toc summary').click();
  await expect(
    page
      .locator('mobile-starlight-toc a')
      .filter({ hasText: 'Before you start' }),
  ).toBeVisible();
  await context.close();
});
