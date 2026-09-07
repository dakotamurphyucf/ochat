// Runs real repository checks without credentials or model requests.
import { execFileSync, spawnSync } from 'node:child_process';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
const root = fileURLToPath(new URL('../../', import.meta.url));
const output = path.resolve(
  process.argv[2] || path.join(root, 'website/.release/semantic-report.json'),
);
const hash = createHash('sha256');
const tracked = execFileSync('git', ['ls-files', '-z'], {
  cwd: root,
  encoding: 'utf8',
})
  .split('\0')
  .filter(Boolean);
for (const file of tracked
  .filter((f) =>
    /^(docs-src\/|lib\/|bin\/|test\/|assets\/|Readme.md$|LICENSE.txt$|dune-project$|ochat.opam$|DEVELOPMENT.md$)/.test(
      f,
    ),
  )
  .sort()) {
  hash.update(file);
  hash.update(await fs.readFile(path.join(root, file)));
}
const run = (cmd, args) =>
  execFileSync(cmd, args, {
    cwd: root,
    encoding: 'utf8',
    maxBuffer: 32 * 1024 * 1024,
  });
const report = {
  revision: run('git', ['rev-parse', 'HEAD']).trim(),
  semanticInputsSha256: hash.digest('hex'),
  ocaml: run('ocamlc', ['-version']).trim(),
  dune: run('dune', ['--version']).trim(),
  command: 'dune build --force @agent-docs-check',
  scope:
    'Offline semantic documentation gate; no live provider quality or public hosting claim.',
};
await fs.mkdir(path.dirname(output), { recursive: true });
try {
  const checked = spawnSync('dune', ['build', '--force', '@agent-docs-check'], {
    cwd: root,
    encoding: 'utf8',
    maxBuffer: 32 * 1024 * 1024,
  });
  if (checked.error) throw checked.error;
  report.output = String(checked.stdout || '') + String(checked.stderr || '');
  report.result = checked.status === 0 ? 'pass' : 'fail';
  if (checked.status !== 0) process.exitCode = 1;
} catch (error) {
  report.result = 'fail';
  report.output = String(error.stdout || '') + String(error.stderr || '');
  process.exitCode = 1;
}
await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
console.log(
  `Semantic documentation gate: ${report.result}; evidence ${output}`,
);
