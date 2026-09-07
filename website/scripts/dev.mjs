import { execFile, fork } from 'node:child_process';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import { watchContent } from './watch-content.mjs';
const websiteRoot = fileURLToPath(new URL('../', import.meta.url));
const repoRoot = fileURLToPath(new URL('../../', import.meta.url));
const run = promisify(execFile);
let watcher;
async function generate() {
  const { stdout, stderr } = await run(
    process.execPath,
    ['scripts/content.mjs'],
    { cwd: websiteRoot },
  );
  if (stdout) process.stdout.write(stdout);
  if (stderr) process.stderr.write(stderr);
}
async function start() {
  const child = fork(
    fileURLToPath(new URL('./dev-server.mjs', import.meta.url)),
    [],
    { cwd: websiteRoot },
  );
  let expected = false,
    ready = false;
  const done = new Promise((resolve) => child.once('exit', resolve));
  await new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('message', (message) => {
      if (message === 'ready') {
        ready = true;
        resolve();
      }
    });
    child.once('exit', (code, signal) => {
      if (!ready)
        reject(
          new Error(`Dev reader exited before startup (${signal || code})`),
        );
      else if (!expected) {
        console.error(
          `Dev reader stopped unexpectedly (${signal || code}); restart npm run dev.`,
        );
        process.exitCode = 1;
        void watcher?.close();
      }
    });
  });
  return {
    async stop() {
      expected = true;
      if (child.exitCode === null && child.signalCode === null)
        child.kill('SIGTERM');
      await done;
    },
  };
}
await generate();
let server = await start();
watcher = await watchContent(
  [
    `${repoRoot}/docs-src`,
    `${repoRoot}/Readme.md`,
    `${repoRoot}/LICENSE.txt`,
    `${repoRoot}/assets`,
    `${websiteRoot}/public`,
    `${websiteRoot}/config`,
    `${websiteRoot}/scripts`,
    `${websiteRoot}/astro.config.mjs`,
    `${websiteRoot}/package-lock.json`,
  ],
  {
    async regenerate() {
      // Both generator and reader need fresh module caches after source edits.
      await server.stop();
      await generate();
      server = await start();
    },
    async onError(error) {
      console.error(
        'Content regeneration failed. Fix the source and restart:',
        error.message,
      );
      await server.stop();
      process.exitCode = 1;
    },
  },
);
for (const signal of ['SIGINT', 'SIGTERM'])
  process.on(signal, async () => {
    await watcher.close();
    await server.stop();
  });
