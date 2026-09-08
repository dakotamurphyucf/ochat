import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
export const digest = (text) => createHash('sha256').update(text).digest('hex');
export function gitFacts(root) {
  const git = (args) =>
    execFileSync('git', args, {
      cwd: root,
      encoding: 'utf8',
      maxBuffer: 16 * 1024 * 1024,
      stdio: ['ignore', 'pipe', 'pipe'],
    }).trim();
  const revision = git(['rev-parse', 'HEAD']);
  const shallow = git(['rev-parse', '--is-shallow-repository']) === 'true';
  return {
    revision,
    shallow,
    source(source, text) {
      let committed;
      try {
        committed = execFileSync('git', ['show', `${revision}:${source}`], {
          cwd: root,
          maxBuffer: 16 * 1024 * 1024,
          stdio: ['ignore', 'pipe', 'pipe'],
        });
      } catch {
        return { sourceCommit: null, sourceModified: true, lastUpdated: null };
      }
      const sourceModified = digest(committed) !== digest(text);
      const lastUpdated =
        !sourceModified && !shallow
          ? git(['log', '-1', '--format=%cI', '--', source])
          : null;
      return { sourceCommit: revision, sourceModified, lastUpdated };
    },
  };
}
