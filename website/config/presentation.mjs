import { createHash } from 'node:crypto';
export const homeTitle = 'Ochat — Build your own AI agents in text files';
export const homeDescription =
  'Understand projects, review documentation, research source notes, and automate reports. Build agents in text files, reuse specialists, and add workflow control when you need it.';
export const socialPath = (route) =>
  '/social/' +
  (route === '/'
    ? 'home'
    : createHash('sha256').update(route).digest('hex').slice(0, 16)) +
  '.png';
export const socialAlt = (title) =>
  `Ochat — ${title}. Agent definitions, tools, and workflows.`;
export const sitemapIncluded = (route, entries, production) =>
  production &&
  (route === '/' ||
    entries.some((e) => e.route === route && e.sitemap && !e.noindex));

// Publishing surfaces match the selected Graphite + blue UI tokens.
export const brandColors = Object.freeze({
  background: '#12151b',
  foreground: '#edf0f7',
  accent: '#91b3ff',
  muted: '#b2bac8',
  line: '#303744',
  lightBackground: '#ffffff',
});
