import { test, expect } from '@playwright/test';
import fs from 'node:fs';
import { withNativeSourceReader } from './native-source-reader';
const report = JSON.parse(
  fs.readFileSync(
    new URL('../../.generated/examples-report.json', import.meta.url),
    'utf8',
  ),
);

// Repeated full-document captures dominate these exhaustive static-source
// checks. Retain action traces, test sources and failure diagnostics without
// screencast frames or DOM snapshots. Reader interaction tests in the other
// suites retain full traces; all assertions here still inspect the actual
// rendered content and accessible names after every native expansion.
test.use({
  trace: {
    mode: 'retain-on-failure',
    screenshots: false,
    snapshots: false,
  },
});

for (const example of report.examples.filter(
  (e: { files: unknown[] }) => e.files.length,
)) {
  test(`catalog ${example.id} preserves all inline files without JavaScript or downloads`, async ({
    browser,
  }) => {
    // This is a whole-bundle reading check, not a single interaction benchmark.
    test.setTimeout(30_000 + example.files.length * 2_000);
    await withNativeSourceReader(browser, async (page) => {
      await page.goto('/docs/examples/');
      const viewer = page.locator(`[data-example-source="${example.id}"]`);
      await viewer.locator(':scope > summary').click();
      for (const file of example.files) {
        const panel = viewer.locator(`[data-source-path="${file.path}"]`);
        if ((await panel.getAttribute('open')) === null)
          await panel.locator(':scope > summary').click();
        const code = panel.locator('pre');
        await expect(code).toBeVisible();
        // The no-JS document is static. Read its source properties together to
        // avoid repeated protocol trips and trace snapshots of the large catalog.
        const rendered = await code.evaluate((node) => ({
          text: node.querySelector('code')?.textContent,
          tabindex: node.getAttribute('tabindex'),
          language: node.dataset.language,
          highlighted: node.querySelectorAll('span[style]').length > 0,
          nestedMarkup: node.querySelectorAll('user, tool, developer, script')
            .length,
        }));
        expect(rendered.text).toBe(file.content);
        expect(rendered.tabindex).toBe('0');
        if (file.path.endsWith('.chatml')) {
          expect(rendered.language).toBe('ocaml');
          expect(rendered.highlighted).toBe(true);
        }
        await expect(code).toHaveAccessibleName(
          `${example.title}: ${file.path} source`,
        );
        expect(rendered.nestedMarkup).toBe(0);
        await panel.locator(':scope > summary').click();
      }
    });
  });
}
