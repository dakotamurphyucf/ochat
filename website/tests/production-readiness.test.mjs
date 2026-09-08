import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { waitForProduction } from '../scripts/production-readiness.mjs';

const homepageSha256 = createHash('sha256').update('current').digest('hex');
const redirect = () => Response.redirect('https://ochatlabs.com/', 308);

test('readiness retries www certificate failures even when the apex already matches', async () => {
  const attempts = [];
  let alternateCalls = 0;
  let pauses = 0;
  const ready = await waitForProduction({
    homepageSha256,
    attempts,
    pause: async () => pauses++,
    get: async (route) => {
      if (route === '/') return new Response('current');
      if (++alternateCalls < 3) throw new Error('certificate not ready');
      return redirect();
    },
  });
  assert.equal(ready, true);
  assert.equal(alternateCalls, 3);
  assert.equal(pauses, 2);
  assert.equal(attempts.length, 3);
  assert.equal(attempts[0].error, 'certificate not ready');
  assert.equal(attempts[1].error, 'certificate not ready');
  assert.equal(attempts[2].ready, true);
});

test('readiness exhausts its bound when www never becomes reachable', async () => {
  const attempts = [];
  let pauses = 0;
  assert.equal(
    await waitForProduction({
      homepageSha256,
      attempts,
      maxAttempts: 3,
      pause: async () => pauses++,
      get: async (route) => {
        if (route === '/') return new Response('current');
        throw new Error('www unavailable');
      },
    }),
    false,
  );
  assert.equal(attempts.length, 3);
  assert.equal(pauses, 2);
  assert.ok(attempts.every((attempt) => attempt.error === 'www unavailable'));
});

test('readiness rejects stale apex bytes and incorrect alternate redirects', async () => {
  for (const scenario of ['stale', 'wrong-host', 'wrong-status']) {
    const attempts = [];
    assert.equal(
      await waitForProduction({
        homepageSha256,
        attempts,
        maxAttempts: 1,
        get: async (route) => {
          if (route === '/')
            return new Response(scenario === 'stale' ? 'old' : 'current');
          if (scenario === 'wrong-host')
            return Response.redirect('https://other.example/', 308);
          if (scenario === 'wrong-status') return new Response('no redirect');
          return redirect();
        },
      }),
      false,
      scenario,
    );
    assert.equal(attempts[0].ready, false);
  }
});
