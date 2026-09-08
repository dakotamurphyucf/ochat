import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { verifyArtifact } from './release-artifact.mjs';
export const requiredReviews = [
  // Manual accessibility review is deferred from launch by the user (2026-09-07).
  'hostedRehearsal',
  'hostedRollback',
  'remoteEnforcement',
  'publicSourceCommit',
];
export function checkApproval(artifact, approval) {
  const failures = [];
  const build = artifact.build;
  const url = new URL(build.origin);
  if (
    build.environment !== 'production' ||
    url.protocol !== 'https:' ||
    /(^localhost$|^127\.|\.(test|invalid|example|localhost)$)/.test(
      url.hostname,
    )
  )
    failures.push(
      'An owned public production origin is required; fixtures cannot be promoted',
    );
  if (
    approval.artifactSha256 !== build.sha256 ||
    approval.origin !== build.origin ||
    approval.revision !== build.revision
  )
    failures.push(
      'Approval does not identify this exact artifact, origin and source revision',
    );
  for (const name of requiredReviews) {
    const review = approval.reviews?.[name];
    if (
      review?.status !== 'pass' ||
      !review.reviewer?.trim() ||
      !review.evidence?.trim() ||
      !Number.isFinite(Date.parse(review.reviewedAt))
    )
      failures.push(`Required review incomplete: ${name}`);
  }
  return { result: failures.length ? 'blocked' : 'pass', failures };
}
if (
  process.argv[1] &&
  path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const [directory, record] = process.argv.slice(2);
  if (!directory || !record)
    throw new Error(
      'Usage: node scripts/release-approval.mjs ARTIFACT APPROVAL.json',
    );
  const artifact = await verifyArtifact(path.resolve(directory));
  const approval = JSON.parse(await fs.readFile(record, 'utf8'));
  const result = checkApproval(artifact, approval);
  console.log(JSON.stringify(result, null, 2));
  if (result.result !== 'pass') process.exitCode = 1;
}
