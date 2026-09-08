// Reviewed against Cloudflare Workers Static Assets documentation, 2026-09-07.
export const limits = Object.freeze({
  files: 20000,
  assetBytes: 25 * 1024 * 1024,
  headerRules: 100,
  headerLineCharacters: 2000,
  staticRedirects: 2000,
  dynamicRedirects: 100,
  redirects: 2100,
  redirectLineCharacters: 1000,
});

export function deploymentHeaders(production) {
  return `/*
  X-Content-Type-Options: nosniff
  Referrer-Policy: strict-origin-when-cross-origin
  Cache-Control: public, max-age=0, must-revalidate
${production ? '' : '  X-Robots-Tag: noindex, nofollow\n'}
/_astro/*
  ! Cache-Control
  Cache-Control: public, max-age=31536000, immutable
`;
}

export function inspectCapacity(files, headers, redirects = '') {
  const failures = [];
  const headerLines = headers
    .split(/\r?\n/)
    .filter((s) => s.trim() && !s.trim().startsWith('#'));
  const headerRules = headerLines.filter((s) => !/^\s/.test(s)).length;
  const rules = redirects
    .split(/\r?\n/)
    .filter((s) => s.trim() && !s.trim().startsWith('#'));
  const dynamicRedirects = rules.filter((s) =>
    /[*:]/.test(s.trim().split(/\s+/)[0]),
  ).length;
  const metrics = {
    files: files.length,
    assetBytes: Math.max(0, ...files.map((f) => f.bytes)),
    headerRules,
    headerLineCharacters: Math.max(0, ...headerLines.map((s) => s.length)),
    staticRedirects: rules.length - dynamicRedirects,
    dynamicRedirects,
    redirects: rules.length,
    redirectLineCharacters: Math.max(0, ...rules.map((s) => s.length)),
  };
  for (const [name, actual] of Object.entries(metrics))
    if (actual > limits[name])
      failures.push(
        `${name}: ${actual} exceeds Free-plan limit ${limits[name]}`,
      );
  const destinations = new Map();
  for (const rule of rules) {
    const [from, to, code = '302', ...extra] = rule.trim().split(/\s+/);
    if (
      !from?.startsWith('/') ||
      !to ||
      extra.length ||
      !['200', '301', '302', '303', '307', '308'].includes(code)
    )
      failures.push(`Malformed redirect: ${rule}`);
    if (destinations.has(from))
      failures.push(`Duplicate redirect source: ${from}`);
    destinations.set(from, to);
  }
  for (const start of destinations.keys()) {
    const seen = new Set();
    let target = start;
    while (destinations.has(target)) {
      if (seen.has(target)) {
        failures.push(`Redirect loop from ${start}`);
        break;
      }
      seen.add(target);
      target = destinations.get(target);
    }
    if (seen.size > 2)
      failures.push(`Redirect chain exceeds two hops from ${start}`);
  }
  return {
    plan: 'Workers Free',
    reviewedAt: '2026-09-07',
    source:
      'https://developers.cloudflare.com/workers/platform/limits/#static-assets',
    limits,
    metrics,
    failures,
    result: failures.length ? 'fail' : 'pass',
  };
}
