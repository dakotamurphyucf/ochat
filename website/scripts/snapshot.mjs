import fs from 'node:fs/promises';
import path from 'node:path';
import lockfile from 'proper-lockfile';

async function exists(file) {
  try {
    await fs.lstat(file);
    return true;
  } catch (e) {
    if (e.code === 'ENOENT') return false;
    throw e;
  }
}
async function rejectSymlink(file) {
  if ((await exists(file)) && (await fs.lstat(file)).isSymbolicLink())
    throw new Error(`Generated path must not be a symlink: ${file}`);
}

// One owned directory contains every input consumed by Astro. The backup is
// restored on the next invocation if a process stops between the two renames.
export async function publishSnapshot(
  siteRoot,
  prepare,
  { checkpoint = async () => {} } = {},
) {
  const target = path.join(siteRoot, '.generated');
  const backup = path.join(siteRoot, '.generated-previous');
  const release = await lockfile.lock(siteRoot, {
    realpath: true,
    lockfilePath: path.join(siteRoot, '.content-lock'),
    stale: 10000,
    update: 2000,
    retries: 0,
  });
  let stage;
  try {
    await rejectSymlink(target);
    await rejectSymlink(backup);
    if (await exists(backup)) {
      if (!(await exists(target))) await fs.rename(backup, target);
      else await fs.rm(backup, { recursive: true });
    }
    for (const entry of await fs.readdir(siteRoot))
      if (entry.startsWith('.generated-stage-')) {
        const abandoned = path.join(siteRoot, entry);
        await rejectSymlink(abandoned);
        await fs.rm(abandoned, { recursive: true, force: true });
      }
    stage = await fs.mkdtemp(path.join(siteRoot, '.generated-stage-'));
    const result = await prepare(stage);
    await checkpoint('staged');
    if (await exists(target)) await fs.rename(target, backup);
    try {
      await checkpoint('backed-up');
      await fs.rename(stage, target);
    } catch (error) {
      if (await exists(backup)) await fs.rename(backup, target);
      throw error;
    }
    await fs.rm(backup, { recursive: true, force: true });
    return result;
  } finally {
    if (stage) await fs.rm(stage, { recursive: true, force: true });
    await release();
  }
}
