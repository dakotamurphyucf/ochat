import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import { spawn } from 'node:child_process';
import { copyPublic } from '../scripts/content.mjs';
import { publishSnapshot } from '../scripts/snapshot.mjs';
const fixture = () =>
  fs.mkdtemp(path.join(os.tmpdir(), 'ochat snapshot café '));
const write = (name) => async (stage) => {
  await fs.writeFile(path.join(stage, 'docs.txt'), name);
  await fs.writeFile(path.join(stage, 'hero.json'), name);
};
async function read(root) {
  return fs.readFile(path.join(root, '.generated/docs.txt'), 'utf8');
}
test('C28: late preparation and promotion failures leave the previous complete snapshot', async () => {
  const root = await fixture();
  try {
    await publishSnapshot(root, write('first'));
    await assert.rejects(
      publishSnapshot(root, async (stage) => {
        await write('partial')(stage);
        throw new Error('late asset failure');
      }),
      /late asset/,
    );
    assert.equal(await read(root), 'first');
    await assert.rejects(
      publishSnapshot(root, write('second'), {
        checkpoint: async (point) => {
          if (point === 'backed-up') throw new Error('rename failure');
        },
      }),
      /rename failure/,
    );
    assert.equal(await read(root), 'first');
    assert.equal(
      await fs.readFile(path.join(root, '.generated/hero.json'), 'utf8'),
      'first',
    );
  } finally {
    await fs.rm(root, { recursive: true, force: true });
  }
});
test('C11/C20: regenerated snapshots delete obsolete files and keep stable content', async () => {
  const root = await fixture();
  try {
    await publishSnapshot(root, async (stage) => {
      await write('same')(stage);
      await fs.writeFile(path.join(stage, 'deleted.md'), 'old');
    });
    await publishSnapshot(root, write('same'));
    assert.equal(await read(root), 'same');
    await assert.rejects(
      fs.stat(path.join(root, '.generated/deleted.md')),
      /ENOENT/,
    );
    await publishSnapshot(root, write('same'));
    assert.equal(await read(root), 'same');
  } finally {
    await fs.rm(root, { recursive: true, force: true });
  }
});
test('concurrent importers cannot replace one another’s output', async () => {
  const root = await fixture();
  let unblock;
  const barrier = new Promise((resolve) => (unblock = resolve));
  let ready;
  const started = new Promise((resolve) => (ready = resolve));
  try {
    const first = publishSnapshot(root, async (stage) => {
      ready();
      await barrier;
      await write('first')(stage);
    });
    await started;
    await assert.rejects(
      publishSnapshot(root, write('second')),
      /already being held/,
    );
    unblock();
    await first;
    assert.equal(await read(root), 'first');
  } finally {
    unblock();
    await fs.rm(root, { recursive: true, force: true });
  }
});
test('C28: recovery after SIGKILL restores the backup before a new failing generation', async () => {
  const root = await fixture();
  try {
    await publishSnapshot(root, write('first'));
    const module = new URL('../scripts/snapshot.mjs', import.meta.url).href;
    const script = `import {publishSnapshot} from ${JSON.stringify(module)};import fs from 'node:fs/promises';import path from 'node:path';await publishSnapshot(${JSON.stringify(root)},async s=>{await fs.writeFile(path.join(s,'docs.txt'),'second')},{checkpoint:async p=>{if(p==='backed-up')process.kill(process.pid,'SIGKILL')}});`;
    const child = spawn(
      process.execPath,
      ['--input-type=module', '-e', script],
      { stdio: 'ignore' },
    );
    const signal = await new Promise((resolve) =>
      child.on('exit', (_, signal) => resolve(signal)),
    );
    assert.equal(signal, 'SIGKILL');
    // Age the abandoned lock rather than sleeping for the stale-lock window.
    const old = new Date(Date.now() - 20000);
    await fs.utimes(path.join(root, '.content-lock'), old, old);
    await assert.rejects(
      publishSnapshot(root, async () => {
        throw new Error('invalid new source');
      }),
      /invalid new source/,
    );
    assert.equal(await read(root), 'first');
    assert.ok(
      !(await fs.readdir(root)).some((n) => n.startsWith('.generated-stage-')),
    );
  } finally {
    await fs.rm(root, { recursive: true, force: true });
  }
});
test('C25: generated destination symlinks are rejected', async () => {
  const root = await fixture(),
    outside = await fixture();
  try {
    await fs.symlink(outside, path.join(root, '.generated'));
    await assert.rejects(publishSnapshot(root, write('bad')), /symlink/);
    assert.deepEqual(await fs.readdir(outside), []);
  } finally {
    await fs.rm(root, { recursive: true, force: true });
    await fs.rm(outside, { recursive: true, force: true });
  }
});

test('the authored public root cannot copy an external directory through a symlink', async () => {
  const root = await fixture(),
    outside = await fixture();
  try {
    await fs.writeFile(path.join(outside, 'unapproved.txt'), 'external');
    await fs.symlink(outside, path.join(root, 'public'));
    await assert.rejects(
      copyPublic(path.join(root, 'public'), path.join(root, 'output')),
      /symlink/,
    );
    await assert.rejects(fs.stat(path.join(root, 'output')), /ENOENT/);
  } finally {
    await fs.rm(root, { recursive: true, force: true });
    await fs.rm(outside, { recursive: true, force: true });
  }
});
