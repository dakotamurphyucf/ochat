import { expect, type Browser, type Page } from '@playwright/test';

// Keep catalog and guide checks independent as the example inventory grows.
// Every case retains literal-byte, native-reading, and no-download coverage.
export async function withNativeSourceReader(
  browser: Browser,
  read: (page: Page) => Promise<void>,
) {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    const downloads: string[] = [];
    page.on('download', (download) =>
      downloads.push(download.suggestedFilename()),
    );
    await read(page);
    expect(downloads).toEqual([]);
  } finally {
    await context.close();
  }
}
