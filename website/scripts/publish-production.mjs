// Called only by the protected main deployment job, after artifact verification.
import fs from 'node:fs/promises';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { verifyProduction } from './verify-production.mjs';
import {
  productionAccount,
  productionOrigin,
  productionWorkers,
} from '../config/production.mjs';

const [directory, semanticFile, output] = process.argv.slice(2);
if (!directory || !semanticFile || !output)
  throw new Error(
    'Usage: publish-production.mjs ARTIFACT SEMANTIC.json REPORT.json',
  );
if (
  !(
    process.env.GITHUB_EVENT_NAME === 'push' ||
    (process.env.GITHUB_EVENT_NAME === 'workflow_dispatch' &&
      process.env.CI_DEPLOY_MODE === 'redeploy')
  ) ||
  process.env.GITHUB_REF !== 'refs/heads/main' ||
  process.env.GITHUB_REPOSITORY !== 'dakotamurphyucf/ochat'
)
  throw new Error(
    'Production publishing requires an Ochat main push or explicit main redeploy',
  );
if (!process.env.CLOUDFLARE_API_TOKEN || !process.env.GITHUB_TOKEN)
  throw new Error('Deployment credentials missing');
const artifact = await verifyProduction(
  path.resolve(directory),
  path.resolve(semanticFile),
);
if (artifact.build.revision !== process.env.GITHUB_SHA)
  throw new Error('Artifact is not this workflow revision');

const report = {
  result: 'fail',
  startedAt: new Date().toISOString(),
  origin: productionOrigin,
  revision: artifact.build.revision,
  artifactSha256: artifact.build.sha256,
  run: `${process.env.GITHUB_SERVER_URL}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}`,
  before: {},
  after: {},
};
async function deployments(worker) {
  const response = await fetch(
    `https://api.cloudflare.com/client/v4/accounts/${productionAccount}/workers/scripts/${worker}/deployments`,
    {
      headers: { Authorization: `Bearer ${process.env.CLOUDFLARE_API_TOKEN}` },
      signal: AbortSignal.timeout(30000),
    },
  );
  if (response.status === 404) return { firstDeployment: true };
  const data = await response.json();
  if (!response.ok || !data.success)
    throw new Error(
      `Cannot read deployment history for ${worker}: HTTP ${response.status}`,
    );
  return data.result;
}
try {
  const domainsResponse = await fetch(
    `https://api.cloudflare.com/client/v4/accounts/${productionAccount}/workers/domains`,
    {
      headers: { Authorization: `Bearer ${process.env.CLOUDFLARE_API_TOKEN}` },
      signal: AbortSignal.timeout(30000),
    },
  );
  const domains = await domainsResponse.json();
  if (!domainsResponse.ok || !domains.success)
    throw new Error('Cannot verify existing custom-domain owners');
  for (const [hostname, service] of [
    ['ochatlabs.com', 'ochat-website'],
    ['www.ochatlabs.com', 'ochat-website-redirect'],
  ]) {
    const existing = domains.result.find(
      (domain) => domain.hostname === hostname,
    );
    if (existing && existing.service !== service)
      throw new Error(`Custom domain ${hostname} belongs to another Worker`);
  }
  report.previousDomains = domains.result.filter((domain) =>
    ['ochatlabs.com', 'www.ochatlabs.com'].includes(domain.hostname),
  );
  // Serialization prevents overlapping publishers; reject an older queued run.
  const current = await fetch(
    `https://api.github.com/repos/${process.env.GITHUB_REPOSITORY}/git/ref/heads/main`,
    {
      headers: {
        Authorization: `Bearer ${process.env.GITHUB_TOKEN}`,
        Accept: 'application/vnd.github+json',
      },
      signal: AbortSignal.timeout(30000),
    },
  );
  if (
    !current.ok ||
    (await current.json()).object.sha !== process.env.GITHUB_SHA
  )
    throw new Error('Superseded main revision; refusing deployment');
  for (const worker of productionWorkers)
    report.before[worker] = await deployments(worker);
  for (const args of [
    [
      'deploy',
      '--config',
      path.resolve(directory, 'wrangler.jsonc'),
      '--env',
      'production',
    ],
    ['deploy', '--config', path.resolve(directory, 'redirect/wrangler.jsonc')],
  ]) {
    const result = spawnSync(
      process.execPath,
      ['node_modules/wrangler/bin/wrangler.js', ...args],
      {
        stdio: 'inherit',
        env: {
          ...process.env,
          CLOUDFLARE_ACCOUNT_ID: productionAccount,
          WRANGLER_SEND_METRICS: 'false',
        },
      },
    );
    if (result.error) throw result.error;
    if (result.status !== 0)
      throw new Error(`Wrangler deployment failed (${result.status})`);
  }
  for (const worker of productionWorkers)
    report.after[worker] = await deployments(worker);
  report.result = 'pass';
} catch (error) {
  report.error = error.message;
  throw error;
} finally {
  report.finishedAt = new Date().toISOString();
  await fs.mkdir(path.dirname(path.resolve(output)), { recursive: true });
  await fs.writeFile(output, JSON.stringify(report, null, 2) + '\n');
}
