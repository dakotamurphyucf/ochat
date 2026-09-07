import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
export const repoRoot = fileURLToPath(new URL('../../', import.meta.url));
export const repository = 'https://github.com/dakotamurphyucf/ochat';
export const branch = 'main';
export const revision = execFileSync('git', ['rev-parse', 'HEAD'], {
  cwd: repoRoot,
  encoding: 'utf8',
}).trim();
export const production = process.env.SITE_ENV === 'production';
const url = new URL(process.env.SITE_URL || 'http://localhost:4321');
if (
  url.username ||
  url.password ||
  url.pathname !== '/' ||
  url.search ||
  url.hash ||
  !['http:', 'https:'].includes(url.protocol)
)
  throw new Error(
    'SITE_URL must be an HTTP(S) origin without credentials, path, query, or fragment',
  );
if (
  production &&
  (!process.env.SITE_URL ||
    url.protocol !== 'https:' ||
    /^(localhost|127\.|.*\.example$|.*\.invalid$)/.test(url.hostname))
)
  throw new Error(
    'Production requires an explicitly configured owned HTTPS SITE_URL',
  );
export const origin = url.origin;
export const sourceUrl = (source, edit = false) =>
  `${repository}/${edit ? 'edit' : 'blob'}/${edit ? branch : revision}/${source.split('/').map(encodeURIComponent).join('/')}`;
