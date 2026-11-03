import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { ExpressiveCode, ExpressiveCodeTheme } from 'expressive-code';
import darkTheme from '@shikijs/themes/github-dark';
import lightTheme from '@shikijs/themes/github-light';
import { visit } from 'unist-util-visit';
import {
  replLanguage,
  replLanguageName,
} from '../scripts/repl-highlighting.mjs';
import codeAccessibility from '../scripts/code-accessibility.mjs';

const hasClass = (node, name) =>
  [node.properties?.className].flat().includes(name);
const text = (node) =>
  node.type === 'text' ? node.value : (node.children || []).map(text).join('');
const elements = (tree, predicate) => {
  const result = [];
  visit(tree, 'element', (node) => {
    if (predicate(node)) result.push(node);
  });
  return result;
};

const engine = new ExpressiveCode({
  themes: [
    new ExpressiveCodeTheme(darkTheme),
    new ExpressiveCodeTheme(lightTheme),
  ],
  shiki: { langs: [replLanguage] },
  plugins: [codeAccessibility],
});

test('REPL source uses the exact shared grammar and preserves source/copy bytes', async () => {
  const shared = JSON.parse(
    await readFile(
      new URL(
        '../../lib/terminal_highlighting/grammars/chatml.json',
        import.meta.url,
      ),
      'utf8',
    ),
  );
  assert.deepEqual(replLanguage.patterns, shared.patterns);
  assert.deepEqual(replLanguage.repository, shared.repository);
  const source = '(* nested (* comment *) *)\nlet saved = [1, "café"]';
  const { renderedGroupAst } = await engine.render({
    code: source,
    language: replLanguageName,
    meta: 'title="REPL input"',
  });
  const lines = elements(renderedGroupAst, (node) => hasClass(node, 'code'));
  assert.equal(lines.map(text).join('\n'), source);
  const buttons = elements(
    renderedGroupAst,
    (node) => node.tagName === 'button',
  );
  assert.equal(buttons.length, 1);
  assert.equal(buttons[0].properties.dataCode.replaceAll('\x7f', '\n'), source);
  assert.ok(
    elements(
      renderedGroupAst,
      (node) => node.tagName === 'span' && node.properties.style,
    ).length > 3,
  );
  assert.equal(
    elements(renderedGroupAst, (node) => node.tagName === 'pre')[0].properties
      .tabIndex,
    0,
  );
});

test('display-only terminal results stay readable and scrollable without a source copy action', async () => {
  const output = '- : int = 42\n# Task.p\nsession:  pure';
  const { renderedGroupAst } = await engine.render({
    code: output,
    language: 'text',
    meta: 'title="Display only · result" frame="terminal"',
  });
  assert.equal(
    elements(renderedGroupAst, (node) => hasClass(node, 'code'))
      .map(text)
      .join('\n'),
    output,
  );
  assert.equal(
    elements(renderedGroupAst, (node) => node.tagName === 'button').length,
    0,
  );
  const pre = elements(renderedGroupAst, (node) => node.tagName === 'pre')[0];
  assert.equal(pre.properties.tabIndex, 0);
  assert.equal(pre.properties.role, 'region');
  assert.equal(
    pre.properties['aria-label'],
    'REPL output, display only 1.1: Display only · result',
  );
  assert.ok(
    elements(renderedGroupAst, (node) => hasClass(node, 'is-terminal')).length >
      0,
  );
});

test('code region names use titles and deterministic page/group positions', async () => {
  for (const title of [
    undefined,
    'Repeated input',
    'Display only · repeated',
  ]) {
    const labels = [];
    for (const groupIndex of [4, 5]) {
      const { renderedGroupAst } = await engine.render(
        [0, 1].map(() => ({
          code: '42',
          language: title?.startsWith('Display only')
            ? 'text'
            : replLanguageName,
          meta: title ? `title="${title}"` : '',
          parentDocument: { positionInDocument: { groupIndex } },
        })),
      );
      const prefix = title?.startsWith('Display only')
        ? 'REPL output, display only'
        : 'Code example';
      const groupLabels = elements(
        renderedGroupAst,
        (node) => node.tagName === 'pre',
      ).map((node) => node.properties['aria-label']);
      assert.deepEqual(
        groupLabels,
        [1, 2].map(
          (blockIndex) =>
            `${prefix} ${groupIndex + 1}.${blockIndex}${title ? `: ${title}` : ''}`,
        ),
      );
      labels.push(...groupLabels);
    }
    assert.equal(new Set(labels).size, 4);
  }
});
