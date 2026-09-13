import assert from 'node:assert/strict';
import test from 'node:test';
import fs from 'node:fs/promises';
import {
  resolveTutorialPaths,
  tutorialPaths,
} from '../config/tutorial-paths.mjs';

test('learning paths expose every registered lesson once and reject silently lost lessons', async () => {
  const tutorials = JSON.parse(
    await fs.readFile(
      new URL('../config/tutorials.json', import.meta.url),
      'utf8',
    ),
  );
  const groups = resolveTutorialPaths(tutorials);
  assert.equal(groups.flatMap((g) => g.tutorials).length, tutorials.length);
  assert.throws(
    () => resolveTutorialPaths([...tutorials, { page: 'new-lesson' }]),
    /missing from learning paths/,
  );
  assert.throws(
    () =>
      resolveTutorialPaths(tutorials, [
        ...tutorialPaths,
        { pages: ['first-agent'] },
      ]),
    /Duplicate tutorial/,
  );
  assert.throws(
    () =>
      resolveTutorialPaths(tutorials, [
        ...tutorialPaths,
        { pages: ['nonexistent'] },
      ]),
    /Unknown tutorial/,
  );
});
