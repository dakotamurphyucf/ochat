import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { watchContent } from '../scripts/watch-content.mjs';
const until = async (predicate) => {
  const deadline = Date.now() + 5000;
  while (!predicate()) {
    if (Date.now() > deadline) throw new Error('Watcher did not settle');
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
};
test('C11/P02.07: real source edits and deletions regenerate; output changes do not loop', async () => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), 'ochat-watch-'));
  const source = path.join(root, 'source');
  await fs.mkdir(source);
  const file = path.join(source, 'a.md');
  await fs.writeFile(file, 'old');
  let calls = 0;
  const watcher = await watchContent([source], {
    delay: 20,
    regenerate: async () => {
      calls++;
      await fs.writeFile(path.join(root, 'output'), String(calls));
    },
    onError: (error) => {
      throw error;
    },
  });
  try {
    await fs.writeFile(file, 'new');
    await until(() => calls === 1);
    await fs.unlink(file);
    await until(() => calls === 2);
    await new Promise((resolve) => setTimeout(resolve, 200));
    assert.equal(calls, 2);
  } finally {
    await watcher.close();
    await fs.rm(root, { recursive: true, force: true });
  }
});
test('watch failures are reported once and stop processing further events', async () => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), 'ochat-watch-'));
  let failures = 0,
    calls = 0;
  const watcher = await watchContent([root], {
    delay: 20,
    regenerate: async () => {
      calls++;
      throw new Error('broken source');
    },
    onError: async (error) => {
      assert.match(error.message, /broken source/);
      failures++;
    },
  });
  try {
    await fs.writeFile(path.join(root, 'a.md'), 'bad');
    await until(() => failures === 1);
    await fs.writeFile(path.join(root, 'a.md'), 'again');
    await new Promise((resolve) => setTimeout(resolve, 150));
    assert.equal(calls, 1);
  } finally {
    await watcher.close();
    await fs.rm(root, { recursive: true, force: true });
  }
});
