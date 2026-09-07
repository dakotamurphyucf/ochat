import { production, origin } from '../../config/site.mjs';
export function GET() {
  return new Response(
    production
      ? `User-agent: *\nAllow: /\nSitemap: ${origin}/sitemap-index.xml\n`
      : 'User-agent: *\nDisallow: /\n',
    { headers: { 'Content-Type': 'text/plain' } },
  );
}
