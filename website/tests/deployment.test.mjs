import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import { spawnSync } from 'node:child_process';
import { parse } from 'yaml';
import { createHash } from 'node:crypto';
import {
  deploymentHeaders,
  inspectCapacity,
  limits,
} from '../scripts/deployment-policy.mjs';
import { inventory, verifyArtifact } from '../scripts/release-artifact.mjs';
import {
  checkApproval,
  requiredReviews,
} from '../scripts/release-approval.mjs';
import { checkQualification } from '../scripts/verify-production.mjs';
import redirectWorker from '../redirect/worker.mjs';

test('public promotion honors the manual-review deferral and rejects incomplete hosted checks or stale evidence', () => {
  const artifact = {
    build: {
      environment: 'production',
      origin: 'https://docs.ochat.org',
      revision: 'a'.repeat(40),
      sha256: 'b'.repeat(64),
    },
  };
  const approval = {
    artifactSha256: artifact.build.sha256,
    origin: artifact.build.origin,
    revision: artifact.build.revision,
    reviews: Object.fromEntries(
      requiredReviews.map((name) => [
        name,
        {
          status: 'pass',
          reviewer: 'Test fixture',
          reviewedAt: '2026-09-07T00:00:00Z',
          evidence: 'Synthetic unit test only',
        },
      ]),
    ),
  };
  assert.equal(requiredReviews.includes('manualAccessibility'), false);
  assert.equal(checkApproval(artifact, approval).result, 'pass');
  approval.reviews.manualAccessibility = { status: 'deferred' };
  assert.equal(checkApproval(artifact, approval).result, 'pass');
  for (const name of requiredReviews) {
    const pending = structuredClone(approval);
    pending.reviews[name].status = 'pending';
    assert.equal(checkApproval(artifact, pending).result, 'blocked');
  }
  assert.equal(
    checkApproval(artifact, { ...approval, artifactSha256: 'c'.repeat(64) })
      .result,
    'blocked',
  );
  assert.equal(
    checkApproval(
      { build: { ...artifact.build, origin: 'https://release.ochat.test' } },
      { ...approval, origin: 'https://release.ochat.test' },
    ).result,
    'blocked',
  );
  assert.equal(
    checkApproval(
      { build: { ...artifact.build, environment: 'preview' } },
      approval,
    ).result,
    'blocked',
  );
});

test('workflow always detects changes, runs selected checks concurrently, and requires their results', async () => {
  const workflow = parse(
    await fs.readFile(
      new URL('../../.github/workflows/website.yml', import.meta.url),
      'utf8',
    ),
  );
  for (const trigger of Object.values(workflow.on))
    assert.ok(!trigger || (!trigger.paths && !trigger['paths-ignore']));
  for (const name of ['framework', 'semantics', 'website']) {
    assert.equal(workflow.jobs[name].needs, 'changes');
    assert.equal(
      workflow.jobs[name].if,
      `needs.changes.outputs.${name} == 'true'`,
    );
  }
  assert.deepEqual(workflow.jobs.website.strategy.matrix.environment, [
    'preview',
    'production',
  ]);
  assert.deepEqual(workflow.jobs.framework.strategy.matrix.tier, [
    'normal',
    'e2e',
  ]);
  const gate = workflow.jobs['release-gate'];
  assert.deepEqual(gate.needs, [
    'changes',
    'semantics',
    'framework',
    'website',
  ]);
  assert.equal(gate.if, 'always()');
  const run = gate.steps.find((step) => step.env?.NEEDS_JSON);
  assert.equal(run.env.NEEDS_JSON, '${{ toJSON(needs) }}');
  assert.equal(run.run, 'node .github/scripts/release-gate.mjs');
  assert.ok(
    workflow.jobs.changes.steps.some(
      (step) => step.run === 'node --test .github/tests/*.test.mjs',
    ),
  );
});

test('deployment capacity rejects excess rules, oversized assets and redirect loops', () => {
  assert.equal(
    inspectCapacity([{ bytes: 1 }], deploymentHeaders(false)).result,
    'pass',
  );
  assert.equal(
    inspectCapacity([{ bytes: limits.assetBytes + 1 }], '').result,
    'fail',
  );
  assert.equal(inspectCapacity([], '/' + 'x'.repeat(2000)).result, 'fail');
  assert.equal(
    inspectCapacity(
      [],
      Array.from({ length: 101 }, (_, i) => `/p${i}\n  X-Test: value`).join(
        '\n',
      ),
    ).result,
    'fail',
  );
  assert.equal(inspectCapacity([], '', '/a /b 301\n/b /a 301').result, 'fail');
  assert.equal(
    inspectCapacity([], '', '/a /b 301\n/b /c 301\n/c /d 301').result,
    'fail',
  );
  assert.equal(inspectCapacity([], '', '/old /docs/ 301').result, 'pass');
  assert.match(deploymentHeaders(false), /X-Robots-Tag: noindex/);
  assert.doesNotMatch(deploymentHeaders(true), /X-Robots-Tag/);
});

test('retained artifact verification rejects changed, additional and symlinked output', async () => {
  const directory = await fs.mkdtemp(
    path.join(os.tmpdir(), 'ochat-artifact-test-'),
  );
  try {
    await fs.mkdir(path.join(directory, 'dist'));
    await fs.writeFile(path.join(directory, 'dist/index.html'), 'known good');
    await fs.writeFile(path.join(directory, 'wrangler.jsonc'), '{}');
    await fs.mkdir(path.join(directory, 'redirect'));
    await fs.writeFile(
      path.join(directory, 'redirect/worker.mjs'),
      'original redirect',
    );
    const actual = await inventory(path.join(directory, 'dist'));
    const manifest = {
      build: { sha256: actual.sha256 },
      files: actual.files,
      configSha256: createHash('sha256').update('{}').digest('hex'),
      redirect: await inventory(path.join(directory, 'redirect')),
    };
    await fs.writeFile(
      path.join(directory, 'artifact.json'),
      JSON.stringify(manifest),
    );
    await verifyArtifact(directory);
    await fs.writeFile(path.join(directory, 'dist/index.html'), 'changed');
    await assert.rejects(verifyArtifact(directory), /differ/);
    await fs.writeFile(path.join(directory, 'dist/index.html'), 'known good');
    await fs.writeFile(path.join(directory, 'dist/extra.html'), 'unapproved');
    await assert.rejects(verifyArtifact(directory), /differ/);
    await fs.unlink(path.join(directory, 'dist/extra.html'));
    await fs.symlink('index.html', path.join(directory, 'dist/link.html'));
    await assert.rejects(verifyArtifact(directory), /symlink/);
    await fs.unlink(path.join(directory, 'dist/link.html'));
    await fs.writeFile(
      path.join(directory, 'redirect/worker.mjs'),
      'changed redirect',
    );
    await assert.rejects(verifyArtifact(directory), /redirect Worker changed/);
  } finally {
    await fs.rm(directory, { recursive: true, force: true });
  }
});

test('production publishing is main-only, serialized, gated, and uses the tested artifact', async () => {
  const workflow = parse(
    await fs.readFile(
      new URL('../../.github/workflows/website.yml', import.meta.url),
      'utf8',
    ),
  );
  const publish = workflow.jobs['deploy-production'];
  assert.deepEqual(publish.needs, ['changes', 'release-gate']);
  assert.equal(
    publish.if,
    "always() && github.ref == 'refs/heads/main' && needs.changes.outputs.deploy == 'true' && needs.release-gate.result == 'success'",
  );
  assert.equal(publish.environment.name, 'production');
  assert.equal(publish.concurrency['cancel-in-progress'], false);
  assert.equal(workflow.permissions.contents, 'read');
  assert.deepEqual(Object.keys(workflow.on).sort(), [
    'pull_request',
    'push',
    'schedule',
    'workflow_dispatch',
  ]);
  assert.deepEqual(workflow.on.workflow_dispatch.inputs.mode.options, [
    'validate',
    'redeploy',
    'cold',
  ]);
  assert.ok(
    publish.steps.some(
      (step) =>
        step.uses === 'actions/download-artifact@v4' &&
        step.with.name === 'website-production-release',
    ),
  );
  assert.ok(
    !publish.steps.some((step) => /npm run build/.test(step.run || '')),
  );
  const secrets = Object.values(workflow.jobs)
    .flatMap((job) => job.steps || [])
    .filter((step) =>
      JSON.stringify(step).includes('secrets.CLOUDFLARE_API_TOKEN'),
    );
  assert.equal(secrets.length, 1);
  assert.equal(secrets[0].name, 'Publish qualified release');
});

test('production qualification rejects stale, preview, failed, or mismatched evidence', () => {
  const revision = 'a'.repeat(40);
  const build = {
    result: 'pass',
    environment: 'production',
    origin: 'https://ochatlabs.com',
    revision,
    sha256: 'b'.repeat(64),
  };
  const good = [
    { build },
    { result: 'pass', revision },
    { result: 'pass', environment: 'production', artifactSha256: build.sha256 },
    { result: 'pass', artifactSha256: build.sha256 },
    { result: 'pass' },
    { status: 'passed', failedTests: [] },
    revision,
  ];
  checkQualification(...good);
  for (const mutate of [
    (args) => {
      args[0].build.environment = 'preview';
    },
    (args) => {
      args[0].build.origin = 'https://release.ochat.test';
    },
    (args) => {
      args[0].build.revision = 'c'.repeat(40);
    },
    (args) => {
      args[1].result = 'fail';
    },
    (args) => {
      args[1].revision = 'c'.repeat(40);
    },
    (args) => {
      args[2].artifactSha256 = 'c'.repeat(64);
    },
    (args) => {
      args[3].result = 'fail';
    },
    (args) => {
      args[4].result = 'fail';
    },
    (args) => {
      args[5].failedTests = ['broken-test'];
    },
    (args) => {
      args[5].status = 'failed';
    },
  ]) {
    const args = structuredClone(good);
    mutate(args);
    assert.throws(() => checkQualification(...args));
  }
});

test('www redirect preserves encoded paths and query strings and rejects unrelated hosts', () => {
  for (const scheme of ['http:', 'https:'])
    for (const route of [
      '/',
      '/docs/start/first-agent/?q=a%2Fb&x=two+words',
      '/downloads/a%20b.chatmd',
    ]) {
      const response = redirectWorker.fetch(
        new Request(`${scheme}//www.ochatlabs.com${route}`),
      );
      assert.equal(response.status, 308);
      assert.equal(
        response.headers.get('location'),
        `https://ochatlabs.com${route}`,
      );
    }
  assert.equal(
    redirectWorker.fetch(new Request('https://unrelated.example/')).status,
    404,
  );
});
