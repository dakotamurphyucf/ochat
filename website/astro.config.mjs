import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import sitemap from '@astrojs/sitemap';
import searchIndex from './scripts/search-index.mjs';
import readingAccessibility from './scripts/rehype-reading.mjs';
import codeAccessibility from './scripts/code-accessibility.mjs';
import { unified } from '@astrojs/markdown-remark';
import { readFileSync } from 'node:fs';
import { sitemapIncluded } from './config/presentation.mjs';
import { origin, repository, production } from './config/site.mjs';
import { buildSidebar } from './config/navigation.mjs';
const entries = JSON.parse(
  readFileSync(new URL('./config/docs-manifest.json', import.meta.url), 'utf8'),
);
const report = JSON.parse(
  readFileSync(
    new URL('./.generated/content-report.json', import.meta.url),
    'utf8',
  ),
);
export default defineConfig({
  site: origin,
  markdown: { processor: unified({ rehypePlugins: [readingAccessibility] }) },
  output: 'static',
  trailingSlash: 'always',
  publicDir: './.generated/public',
  integrations: [
    sitemap({
      filter: (url) =>
        sitemapIncluded(new URL(url).pathname, entries, production),
    }),
    starlight({
      disable404Route: true,
      pagefind: false,
      markdown: { processedDirs: ['.generated/docs'] },
      title: 'Ochat',
      description: 'Build your own AI agents in text files.',
      favicon: '/favicon.svg',
      social: [{ icon: 'github', label: 'GitHub', href: repository }],
      customCss: ['./src/styles/docs.css'],
      components: {
        Header: './src/components/DocsHeader.astro',
        Head: './src/components/Head.astro',
        Footer: './src/components/DocsFooter.astro',
        SiteTitle: './src/components/DocsTitle.astro',
        PageTitle: './src/components/ArticleTitle.astro',
        ThemeProvider: './src/components/ThemeProvider.astro',
        ThemeSelect: './src/components/ThemeSelect.astro',
        Search: './src/components/Search.astro',
        MobileMenuToggle: './src/components/MobileMenuToggle.astro',
        MarkdownContent: './src/components/MarkdownContent.astro',
      },
      sidebar: buildSidebar(entries),
      pagination: false,
      lastUpdated: false,
      expressiveCode: {
        plugins: [codeAccessibility],
        themes: ['github-dark', 'github-light'],
        shiki: {
          langAlias: {
            chatml: 'ocaml',
            console: 'shellsession',
            mermaid: 'text',
            ...Object.fromEntries(
              report.languageFallbacks.map((f) => [f.language, f.highlightAs]),
            ),
          },
        },
      },
    }),
    searchIndex(),
  ],
});
