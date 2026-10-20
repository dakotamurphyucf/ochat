import { test, expect, type Page } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';
import axe, { type AxeResults, type CheckHelper } from 'axe-core';
import { analyzeFramelessPage } from './frameless-accessibility';

const tags = ['wcag2a', 'wcag2aa', 'wcag21aa'];
declare global {
  interface Window {
    axe: typeof axe;
    evaluations: Record<string, number>;
    resolveAudit?: (result: boolean) => void;
  }
}

// The installed Playwright emitter exposes this runtime inspection method.
function frameListeners(page: Page) {
  if (!('listenerCount' in page) || typeof page.listenerCount !== 'function')
    throw new Error('Expected the installed Playwright event emitter');
  const count: unknown = page.listenerCount('frameattached');
  if (typeof count !== 'number') throw new Error('Expected a listener count');
  return count;
}

function configureFixture() {
  const fixture = window;
  fixture.evaluations = {};
  fixture.axe.configure({
    checks: [
      {
        id: 'ochat-diagnostic-check',
        evaluate(this: CheckHelper, node: Element) {
          fixture.evaluations[node.id] =
            (fixture.evaluations[node.id] ?? 0) + 1;
          this.data({ case: node.id });
          this.relatedNodes([document.getElementById('related')!]);
          const outcome = node.getAttribute('data-outcome');
          return outcome === 'incomplete' ? undefined : outcome === 'pass';
        },
        metadata: {
          impact: 'moderate',
          messages: {
            pass: 'Diagnostic passed',
            fail: 'Diagnostic failed',
            incomplete: 'Diagnostic needs review',
          },
        },
      },
    ],
    rules: [
      {
        id: 'ochat-diagnostic-rule',
        selector: '[data-outcome]',
        tags: ['wcag2a'],
        any: ['ochat-diagnostic-check'],
        all: [],
        none: [],
      },
    ],
  });
}

function diagnostics(result: AxeResults) {
  return Object.fromEntries(
    ['violations', 'incomplete'].map((group) => [
      group,
      result[group as 'violations' | 'incomplete']
        .map((rule) => ({
          ...rule,
          nodes: [...rule.nodes].sort((a, b) =>
            JSON.stringify(a.target).localeCompare(JSON.stringify(b.target)),
          ),
        }))
        .sort((a, b) => a.id.localeCompare(b.id)),
    ]),
  );
}

test('frameless reporting preserves every failure and incomplete while evaluating passing nodes', async ({
  page,
}) => {
  const nodeIds = [
    'failure-a',
    'failure-b',
    'uncertain-a',
    'uncertain-b',
    'pass-a',
    'pass-b',
    'pass-c',
  ];
  const fixture = `<!doctype html><html lang="en"><head><title>Diagnostic</title></head><body><main><h1>Diagnostic</h1>
    <img id="image-a"><img id="image-b"><button id="button-a"></button><button id="button-b"></button>
    <div id="shadow-host"></div><p id="related">Related diagnostic evidence</p>
    ${nodeIds.map((id) => `<p id="${id}" data-outcome="${id.startsWith('failure') ? 'fail' : id.startsWith('uncertain') ? 'incomplete' : 'pass'}">${id}</p>`).join('')}
    </main></body></html>`;
  const axeSource = `${axe.source}\n;(${configureFixture.toString()})();`;
  const scans: AxeResults[] = [];
  for (const route of ['modern', 'direct', 'projected']) {
    await page.setContent(fixture);
    await page.evaluate(() => {
      document
        .getElementById('shadow-host')!
        .attachShadow({ mode: 'open' }).innerHTML =
        '<button id="shadow-button"></button>';
    });
    const listeners = frameListeners(page);
    const result =
      route === 'projected'
        ? await analyzeFramelessPage(page, { axeSource })
        : await new AxeBuilder({ page, axeSource })
            .setLegacyMode(route === 'direct')
            .withTags(tags)
            .analyze();
    expect(frameListeners(page)).toBe(listeners);
    expect(result.toolOptions.runOnly).toEqual({ type: 'tag', values: tags });
    expect(result.toolOptions.selectors).not.toBe(false);
    expect(await page.evaluate(() => window.evaluations)).toEqual(
      Object.fromEntries(nodeIds.map((id) => [id, 1])),
    );
    const custom = (group: 'violations' | 'incomplete' | 'passes') =>
      result[group].find((rule) => rule.id === 'ochat-diagnostic-rule')!;
    expect(
      custom('violations')
        .nodes.map((node) => node.target)
        .sort(),
    ).toEqual([['#failure-a'], ['#failure-b']]);
    expect(
      custom('incomplete')
        .nodes.map((node) => node.target)
        .sort(),
    ).toEqual([['#uncertain-a'], ['#uncertain-b']]);
    for (const node of [
      ...custom('violations').nodes,
      ...custom('incomplete').nodes,
    ]) {
      expect(node.any).toHaveLength(1);
      expect(
        node.any[0].relatedNodes!.map((related) => related.target),
      ).toEqual([['#related']]);
    }
    expect(custom('passes').nodes).toHaveLength(route === 'projected' ? 1 : 3);
    for (const [id, targets] of [
      ['image-alt', ['#image-a', '#image-b']],
      ['button-name', ['#button-a', '#button-b']],
    ] as const) {
      const nodes = result.violations.find((rule) => rule.id === id)!.nodes;
      for (const target of targets)
        expect(
          nodes.some(
            (node) => JSON.stringify(node.target) === JSON.stringify([target]),
          ),
        ).toBe(true);
    }
    expect(
      result.violations
        .find((rule) => rule.id === 'button-name')!
        .nodes.some((node) =>
          JSON.stringify(node.target).includes('shadow-button'),
        ),
    ).toBe(true);
    scans.push(result);
  }
  for (const result of scans.slice(1)) {
    expect(diagnostics(result)).toEqual(diagnostics(scans[0]));
    for (const group of [
      'violations',
      'incomplete',
      'passes',
      'inapplicable',
    ] as const)
      expect(result[group].map((rule) => rule.id).sort()).toEqual(
        scans[0][group].map((rule) => rule.id).sort(),
      );
  }
});

test('frameless auditing rejects existing frames and restores its listener', async ({
  page,
}) => {
  for (const attributes of [
    'srcdoc="<p>Same-origin child</p>"',
    `src="data:text/html,${encodeURIComponent('<p>Opaque-origin child</p>')}"`,
  ]) {
    await page.setContent(`<iframe ${attributes}></iframe>`);
    expect(page.frames()).toHaveLength(2);
    const listeners = frameListeners(page);
    await expect(analyzeFramelessPage(page)).rejects.toThrow(
      'requires a frameless page',
    );
    expect(frameListeners(page)).toBe(listeners);
  }
});

function configurePendingCheck() {
  const fixture = window;
  fixture.axe.configure({
    checks: [
      {
        id: 'ochat-pending-check',
        evaluate(this: CheckHelper) {
          fixture.resolveAudit = this.async();
        },
      },
    ],
    rules: [
      {
        id: 'ochat-pending-rule',
        selector: '#pending',
        tags: ['wcag2a'],
        any: ['ochat-pending-check'],
      },
    ],
  });
}

test('frameless auditing rejects a frame detached before completion and restores its listener', async ({
  page,
}) => {
  await page.setContent(
    '<main><h1>Diagnostic</h1><p id="pending">Pending check</p></main>',
  );
  const listeners = frameListeners(page);
  const audit = analyzeFramelessPage(page, {
    axeSource: `${axe.source}\n;(${configurePendingCheck.toString()})();`,
  });
  const rejected = expect(audit).rejects.toThrow('requires a frameless page');
  try {
    await page.waitForFunction(() => typeof window.resolveAudit === 'function');
    const attached = page.waitForEvent('frameattached');
    await page.evaluate(() =>
      document.body.append(document.createElement('iframe')),
    );
    await attached;
    await page.locator('iframe').evaluate((frame) => frame.remove());
    await expect.poll(() => page.frames().length).toBe(1);
    await page.evaluate(() => {
      const fixture = window;
      const resolve = fixture.resolveAudit;
      delete fixture.resolveAudit;
      resolve?.(true);
    });
    await rejected;
    expect(frameListeners(page)).toBe(listeners);
  } finally {
    await page.evaluate(() => {
      const fixture = window;
      const resolve = fixture.resolveAudit;
      delete fixture.resolveAudit;
      resolve?.(true);
    });
    await audit.catch(() => {});
  }
});

test('frameless auditing preserves injection errors and restores its listener', async ({
  page,
}) => {
  await page.setContent('<main><h1>Diagnostic</h1></main>');
  const listeners = frameListeners(page);
  await expect(
    analyzeFramelessPage(page, {
      axeSource: `${axe.source}\nthrow new Error('OCH-448 injection sentinel');`,
    }),
  ).rejects.toThrow('OCH-448 injection sentinel');
  expect(frameListeners(page)).toBe(listeners);
});
