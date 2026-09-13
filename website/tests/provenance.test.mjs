import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { digest, gitFacts } from '../scripts/provenance.mjs';
test('C27: source dates come from Git; edited, new and shallow sources are not misdated', async () => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), 'ochat-git-'));
  const git = (args) => execFileSync('git', args, { cwd: root, stdio: 'pipe' });
  try {
    git(['init', '-q']);
    await fs.writeFile(path.join(root, 'a.md'), 'original');
    git(['add', '.']);
    git([
      '-c',
      'user.name=Test',
      '-c',
      'user.email=test@example.invalid',
      'commit',
      '-qm',
      'fixture',
    ]);
    let facts = gitFacts(root);
    const recordedRevision = facts.revision;
    await fs.writeFile(path.join(root, 'a.md'), 'changed worktree');
    assert.equal(
      facts.sourceDigestAt(recordedRevision, 'a.md'),
      digest('original'),
    );
    assert.throws(
      () => facts.sourceDigestAt('HEAD', 'a.md'),
      /Invalid recorded/,
    );
    assert.throws(
      () => facts.sourceDigestAt(recordedRevision, '../a.md'),
      /Invalid recorded/,
    );
    assert.throws(
      () => facts.sourceDigestAt(recordedRevision, 'absent.md'),
      /unavailable/,
    );
    assert.equal(facts.source('a.md', 'original').sourceModified, false);
    assert.match(facts.source('a.md', 'original').lastUpdated, /^\d{4}-/);
    assert.equal(facts.source('a.md', 'changed').lastUpdated, null);
    assert.equal(facts.source('new.md', 'new').sourceCommit, null);
    await fs.writeFile(path.join(root, '.git/shallow'), facts.revision + '\n');
    facts = gitFacts(root);
    assert.equal(facts.shallow, true);
    assert.equal(facts.source('a.md', 'original').lastUpdated, null);
  } finally {
    await fs.rm(root, { recursive: true, force: true });
  }
});
