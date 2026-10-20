import type { Page } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

// Direct axe.run can project successful-node detail before selector generation.
// Its frame coverage differs from the recursive adapter, so reject any frame,
// including one attached and detached while the audit is running.
export async function analyzeFramelessPage(
  page: Page,
  options: { axeSource?: string } = {},
) {
  const mainFrame = page.mainFrame();
  let frameAttached = false;
  const recordFrame = () => {
    frameAttached = true;
  };
  const assertFrameless = async () => {
    const elements = await page.locator('iframe, frame').count();
    if (
      frameAttached ||
      elements !== 0 ||
      page.frames().length !== 1 ||
      page.frames()[0] !== mainFrame
    ) {
      throw new Error('Accessibility audit requires a frameless page');
    }
  };
  page.on('frameattached', recordFrame);
  try {
    await assertFrameless();
    const result = await new AxeBuilder({ page, axeSource: options.axeSource })
      .setLegacyMode(true)
      .options({ resultTypes: ['violations', 'incomplete'] })
      .withTags(['wcag2a', 'wcag2aa', 'wcag21aa'])
      .analyze();
    await assertFrameless();
    return result;
  } finally {
    page.off('frameattached', recordFrame);
  }
}
