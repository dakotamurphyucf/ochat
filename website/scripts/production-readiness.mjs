import { createHash } from 'node:crypto';
import { productionOrigin } from '../config/production.mjs';

// A successful apex response alone must not end readiness retries: the www
// certificate and route must also be ready in the same completed observation.
export async function waitForProduction({
  get,
  homepageSha256,
  attempts,
  maxAttempts = 12,
  pause = () => new Promise((resolve) => setTimeout(resolve, 5000)),
}) {
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      const response = await get('/');
      const matchingBytes =
        response.status === 200 &&
        createHash('sha256')
          .update(Buffer.from(await response.arrayBuffer()))
          .digest('hex') === homepageSha256;
      const alternate = await get('https://www.ochatlabs.com/');
      const ready =
        matchingBytes &&
        alternate.status === 308 &&
        alternate.headers.get('location') === productionOrigin + '/';
      attempts.push({
        attempt,
        status: response.status,
        matchingBytes,
        alternateStatus: alternate.status,
        ready,
      });
      if (ready) return true;
    } catch (error) {
      attempts.push({ attempt, error: error.message });
    }
    if (attempt < maxAttempts) await pause();
  }
  return false;
}
