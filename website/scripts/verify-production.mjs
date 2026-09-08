import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { verifyArtifact } from './release-artifact.mjs';
import { checkApproval } from './release-approval.mjs';
import { productionOrigin } from '../config/production.mjs';

const site = fileURLToPath(new URL('../', import.meta.url));
const read = async (filename) =>
  JSON.parse(await fs.readFile(filename, 'utf8'));
const sha = (bytes) => createHash('sha256').update(bytes).digest('hex');

export function checkQualification(
  artifact,
  semantic,
  search,
  performance,
  capacity,
  browser,
  revision,
) {
  assert.equal(artifact.build.result, 'pass');
  assert.equal(artifact.build.environment, 'production');
  assert.equal(artifact.build.origin, productionOrigin);
  assert.equal(artifact.build.revision, revision);
  assert.equal(semantic.result, 'pass');
  assert.equal(semantic.revision, revision);
  for (const report of [search, performance]) {
    assert.equal(report.result, 'pass');
    assert.equal(report.artifactSha256, artifact.build.sha256);
  }
  assert.equal(search.environment, 'production');
  assert.equal(capacity.result, 'pass');
  assert.equal(browser.status, 'passed');
  assert.deepEqual(browser.failedTests, []);
}

export async function verifyProduction(directory, semanticFile) {
  const artifact = await verifyArtifact(directory);
  const revision = execFileSync('git', ['rev-parse', 'HEAD'], {
    cwd: site,
    encoding: 'utf8',
  }).trim();
  const evidence = path.join(directory, 'evidence');
  checkQualification(
    artifact,
    await read(semanticFile),
    await read(path.join(evidence, 'search-report.json')),
    await read(path.join(evidence, 'performance-report.json')),
    await read(path.join(evidence, 'capacity-report.json')),
    await read(path.join(directory, 'browser-result.json')),
    revision,
  );
  assert.equal(
    artifact.configSha256,
    sha(await fs.readFile(path.join(site, 'wrangler.jsonc'))),
  );
  assert.equal(
    artifact.packageLockSha256,
    sha(await fs.readFile(path.join(site, 'package-lock.json'))),
  );
  assert.ok(
    artifact.redirect,
    'Production requires a retained redirect Worker',
  );
  assert.deepEqual(
    artifact.redirect.files.map((f) => f.path),
    ['worker.mjs', 'wrangler.jsonc'],
  );
  for (const name of ['worker.mjs', 'wrangler.jsonc'])
    assert.deepEqual(
      await fs.readFile(path.join(directory, 'redirect', name)),
      await fs.readFile(path.join(site, 'redirect', name)),
    );
  // Check a real anonymous source URL at this exact public revision.
  const source = 'docs-src/agent-server/tutorials/local-tui.md';
  const url = `https://raw.githubusercontent.com/dakotamurphyucf/ochat/${revision}/${source}`;
  const response = await fetch(url, { signal: AbortSignal.timeout(30000) });
  assert.equal(response.status, 200, 'Public committed tutorial must resolve');
  assert.deepEqual(
    Buffer.from(await response.arrayBuffer()),
    await fs.readFile(path.join(site, '..', source)),
  );
  const approval = {
    artifactSha256: artifact.build.sha256,
    origin: artifact.build.origin,
    revision,
    reviews: {
      ...(await read(path.join(site, 'config/release-prerequisites.json'))),
      publicSourceCommit: {
        status: 'pass',
        reviewer: 'Automated anonymous source-byte check',
        reviewedAt: new Date().toISOString(),
        evidence: url,
      },
    },
  };
  assert.equal(checkApproval(artifact, approval).result, 'pass');
  await fs.writeFile(
    path.join(directory, 'approval.json'),
    JSON.stringify(approval, null, 2) + '\n',
  );
  return artifact;
}

if (
  process.argv[1] &&
  path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const [directory, semantic] = process.argv.slice(2);
  if (!directory || !semantic)
    throw new Error('Usage: verify-production.mjs ARTIFACT SEMANTIC.json');
  const artifact = await verifyProduction(
    path.resolve(directory),
    path.resolve(semantic),
  );
  console.log(
    JSON.stringify({
      result: 'pass',
      revision: artifact.build.revision,
      origin: artifact.build.origin,
      sha256: artifact.build.sha256,
    }),
  );
}
