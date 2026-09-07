import path from 'node:path';
import fs from 'node:fs/promises';
import { unified } from 'unified';
import remarkParse from 'remark-parse';
import remarkGfm from 'remark-gfm';
import { visit, SKIP } from 'unist-util-visit';
import { toString } from 'mdast-util-to-string';
import { toMarkdown } from 'mdast-util-to-markdown';
import GithubSlugger from 'github-slugger';
import { parseFragment } from 'parse5';
import { parse as parseYaml } from 'yaml';
import { manifestSchema } from './manifest-schema.mjs';

export const parser = unified().use(remarkParse).use(remarkGfm);
export const rendered = (e) =>
  ['publish', 'compatibility', 'bridge'].includes(e.disposition);
export const encodePath = (s) => s.split('/').map(encodeURIComponent).join('/');
export function bridgeMappings(text, entry, context) {
  if (entry.disposition !== 'bridge') return [];
  const slugger = new GithubSlugger();
  const mappings = [];
  let current;
  visit(parser.parse(text), (node) => {
    if (node.type === 'heading') {
      current = {
        fragment: slugger.slug(
          toString(node, { includeHtml: false, includeImageAlt: false }),
        ),
        target: null,
      };
      mappings.push(current);
    } else if (node.type === 'link' && current && !current.target) {
      current.target = resolveLink(node.url, entry.source, context);
    }
  });
  for (const mapping of mappings) {
    if (!mapping.target)
      throw new Error(
        `Bridge heading has no forwarding link: ${entry.source}#${mapping.fragment}`,
      );
  }
  return mappings;
}
export function validateManifest(entries, tracked) {
  const parsed = manifestSchema.safeParse(entries);
  if (!parsed.success)
    throw new Error(`Invalid documentation manifest: ${parsed.error.message}`);
  const sources = new Set(),
    ids = new Set(),
    routes = new Map();
  for (const e of entries) {
    if (!e.id || ids.has(e.id))
      throw new Error(`Duplicate or empty ID: ${e.id}`);
    ids.add(e.id);
    if (!tracked.has(e.source) || sources.has(e.source))
      throw new Error(`Missing, untracked, or duplicate source: ${e.source}`);
    sources.add(e.source);
    if (
      !e.source.startsWith('docs-src/') ||
      !e.source.endsWith('.md') ||
      path.posix.normalize(e.source) !== e.source
    )
      throw new Error(`Invalid documentation source: ${e.source}`);
    if (e.limitationSource && !tracked.has(e.limitationSource))
      throw new Error(`Missing limitation source: ${e.source}`);
    if (
      ['offline-checked', 'live-checked'].includes(e.verification) &&
      (!e.verifiedAt || !e.verifiedCommit)
    )
      throw new Error(`Verification needs date and revision: ${e.source}`);
    if (
      ![
        'publish',
        'compatibility',
        'bridge',
        'repository-only',
        'deferred',
      ].includes(e.disposition)
    )
      throw new Error(`Invalid disposition: ${e.source}`);
    if (!e.title || !['authored', 'generated-from-code'].includes(e.provenance))
      throw new Error(`Missing title/provenance: ${e.source}`);
    if (e.provenance === 'generated-from-code' && !tracked.has(e.generatedBy))
      throw new Error(`Missing generator: ${e.source}`);
    if (rendered(e)) {
      if (!e.description || !/^\/docs\/(?:[a-z0-9-]+\/)*$/.test(e.route || ''))
        throw new Error(
          `Invalid documentation route or missing description: ${e.source}`,
        );
      if (routes.has(e.route))
        throw new Error(
          `Route collision: ${e.source} and ${routes.get(e.route)}`,
        );
      routes.set(e.route, e.source);
      for (const field of ['navigation', 'search', 'sitemap', 'noindex'])
        if (typeof e[field] !== 'boolean')
          throw new Error(`Missing ${field}: ${e.source}`);
      if (
        e.disposition === 'bridge' &&
        (e.search || e.sitemap || e.navigation || !e.noindex)
      )
        throw new Error(`Bridge policy invalid: ${e.source}`);
    } else if (e.route || e.navigation || e.search || e.sitemap)
      throw new Error(`Unpublished page has publication settings: ${e.source}`);
    if (
      ['deferred', 'repository-only'].includes(e.disposition) &&
      !e.reviewNote
    )
      throw new Error(`Missing disposition reason: ${e.source}`);
    // These features require route/alias adapters; never silently accept unused declarations.
    if (e.aliases?.length || Object.keys(e.fragmentAliases || {}).length)
      throw new Error(
        `Use a bridge until alias mapping is implemented: ${e.source}`,
      );
  }
  for (const e of entries)
    for (const id of e.related || [])
      if (!ids.has(id))
        throw new Error(`Unknown related ID ${id} in ${e.source}`);
  for (const source of tracked)
    if (
      source.startsWith('docs-src/') &&
      source.endsWith('.md') &&
      !sources.has(source)
    )
      throw new Error(`Missing disposition: ${source}`);
  return routes;
}
export async function containedFile(root, relative) {
  const full = await fs.realpath(path.resolve(root, relative));
  const base = await fs.realpath(root);
  if (!full.startsWith(base + path.sep))
    throw new Error(`Source escapes repository: ${relative}`);
  if (!(await fs.stat(full)).isFile())
    throw new Error(`Not a file: ${relative}`);
  return full;
}
export function resolveLink(url, source, context, image = false) {
  if (/^(https?:|mailto:)/i.test(url)) return url;
  if (
    /^[a-z][\w+.-]*:/i.test(url) ||
    url.startsWith('//') ||
    url.includes('\\')
  )
    throw new Error(`Unsupported link ${url} in ${source}`);
  if (url.startsWith('#') || url.startsWith('?') || url === '') return url;
  const match = /^([^?#]*)(\?[^#]*)?(#.*)?$/.exec(url);
  if (!match) throw new Error(`Invalid URL ${url} in ${source}`);
  const [, raw, query = '', fragment = ''] = match;
  let decoded;
  try {
    decoded = decodeURIComponent(raw);
  } catch {
    throw new Error(`Invalid URL encoding ${url} in ${source}`);
  }
  const target = path.posix.normalize(
    path.posix.join(path.posix.dirname(source), decoded),
  );
  if (
    decoded.startsWith('/') ||
    target.startsWith('../') ||
    !context.tracked.has(target)
  )
    throw new Error(
      `Unknown or escaping local destination ${url} in ${source}`,
    );
  const asset = context.assets[target];
  if (
    context.supplemental &&
    !context.bySource.has(target) &&
    !context.supplemental.has(target)
  )
    throw new Error(
      `Missing supplemental link disposition: ${target} in ${source}`,
    );
  if (image) {
    if (!asset)
      throw new Error(
        `Image needs asset allowlist entry: ${target} in ${source}`,
      );
    return asset + query + fragment;
  }
  const entry = context.bySource.get(target);
  const destination =
    entry && rendered(entry)
      ? entry.route
      : asset || context.downloads?.get(target) || context.sourceUrl(target);
  return destination + query + fragment;
}
const allowedTags = new Set([
  'a',
  'img',
  'div',
  'span',
  'br',
  'hr',
  'kbd',
  'code',
  'pre',
  'details',
  'summary',
  'p',
  'strong',
  'em',
  'b',
  'i',
  'sub',
  'sup',
  'table',
  'thead',
  'tbody',
  'tr',
  'th',
  'td',
  'ul',
  'ol',
  'li',
  'blockquote',
  'h1',
  'h2',
  'h3',
  'h4',
  'h5',
  'h6',
]);
const allowedAttrs = new Set([
  'href',
  'src',
  'alt',
  'title',
  'id',
  'name',
  'width',
  'height',
  'loading',
  'decoding',
  'class',
  'open',
  'align',
  'colspan',
  'rowspan',
  'target',
  'rel',
]);
export function transformHtml(html, source, context) {
  const tree = parseFragment(html, { sourceCodeLocationInfo: true });
  const edits = [];
  function walk(node) {
    if (node.tagName && !allowedTags.has(node.tagName))
      throw new Error(
        `Unsupported raw HTML <${node.tagName}> in ${source}; put language examples in code fences`,
      );
    for (const attr of node.attrs || []) {
      if (
        ['loading', 'decoding'].includes(attr.name) &&
        (node.tagName !== 'img' ||
          !(
            attr.name === 'loading'
              ? ['lazy', 'eager']
              : ['async', 'sync', 'auto']
          ).includes(attr.value))
      )
        throw new Error(
          `Invalid image loading attribute ${attr.name} in ${source}`,
        );
      if (!allowedAttrs.has(attr.name))
        throw new Error(`Unsupported HTML attribute ${attr.name} in ${source}`);
      if (
        node.tagName === 'a' &&
        ['id', 'name'].includes(attr.name) &&
        context.headingIds?.has(attr.value)
      ) {
        const loc = node.sourceCodeLocation?.attrs?.[attr.name];
        if (loc)
          edits.push({ start: loc.startOffset, end: loc.endOffset, text: '' });
      }
      if (attr.name === 'href' || attr.name === 'src') {
        const value = resolveLink(
          attr.value,
          source,
          context,
          attr.name === 'src',
        );
        const loc = node.sourceCodeLocation?.attrs?.[attr.name];
        if (loc)
          edits.push({
            start: loc.startOffset,
            end: loc.endOffset,
            text: `${attr.name}="${value.replaceAll('&', '&amp;').replaceAll('"', '&quot;')}"`,
          });
      }
    }
    for (const child of node.childNodes || []) walk(child);
  }
  walk(tree);
  return applyEdits(html, edits);
}
export function applyEdits(text, edits) {
  let boundary = text.length;
  for (const e of edits.sort((a, b) => b.start - a.start)) {
    if (e.end > boundary)
      throw new Error('Overlapping Markdown transformations');
    text = text.slice(0, e.start) + e.text + text.slice(e.end);
    boundary = e.start;
  }
  return text;
}
export function transformMarkdown(text, entry, context) {
  if (/^---\r?\n/.test(text)) {
    const match = /^---\r?\n([\s\S]*?)\r?\n---(?:\r?\n|$)/.exec(text);
    if (!match) throw new Error(`Unclosed source frontmatter: ${entry.source}`);
    const metadata = parseYaml(match[1]);
    if (!metadata || typeof metadata !== 'object' || Array.isArray(metadata))
      throw new Error(`Invalid source frontmatter: ${entry.source}`);
    for (const [key, value] of Object.entries(metadata)) {
      if (
        ![
          'title',
          'description',
          'status',
          'audience',
          'kind',
          'verification',
          'verifiedAt',
          'verifiedCommit',
        ].includes(key) ||
        JSON.stringify(value) !== JSON.stringify(entry[key])
      )
        throw new Error(
          `Source frontmatter conflicts with manifest field ${key}: ${entry.source}`,
        );
    }
    text = text.slice(match[0].length);
  }
  const tree = parser.parse(text),
    edits = [],
    slugger = new GithubSlugger();
  const headings = [],
    fences = [];
  let removedTitle = false;
  // Redundant explicit anchors must not duplicate the renderer's heading IDs.
  // Keep distinct historical aliases; the matching heading retains the URL.
  const imageDefinitions = new Set();
  visit(tree, 'imageReference', (node) => {
    imageDefinitions.add(node.identifier);
  });
  const headingIds = new Set();
  const inventorySlugger = new GithubSlugger();
  visit(tree, 'heading', (node) => {
    headingIds.add(
      inventorySlugger.slug(
        toString(node, { includeHtml: false, includeImageAlt: false }),
      ),
    );
  });
  visit(tree, (node) => {
    if (node.type === 'heading') {
      const id = slugger.slug(
        toString(node, { includeHtml: false, includeImageAlt: false }),
      );
      headings.push(id);
      if (node.depth === 1 && !removedTitle) {
        edits.push({
          start: node.position.start.offset,
          end: node.position.end.offset,
          text: `<a id="${id}"></a>`,
        });
        removedTitle = true;
        return SKIP;
      }
    }
    if (node.type === 'code')
      fences.push({ lang: node.lang || 'text', value: node.value });
    if (['link', 'image', 'definition'].includes(node.type)) {
      // Rewrite only parsed link nodes. The rest of the source, including fenced
      // examples and their line endings, is copied without reserialization.
      visit(node, (nested) => {
        if (['link', 'image', 'definition'].includes(nested.type))
          nested.url = resolveLink(
            nested.url,
            entry.source,
            context,
            nested.type === 'image' ||
              (nested.type === 'definition' &&
                imageDefinitions.has(nested.identifier)),
          );
      });
      edits.push({
        start: node.position.start.offset,
        end: node.position.end.offset,
        text: toMarkdown(node).trimEnd(),
      });
      return SKIP;
    }
    if (node.type === 'html')
      edits.push({
        start: node.position.start.offset,
        end: node.position.end.offset,
        text: transformHtml(node.value, entry.source, {
          ...context,
          headingIds,
        }),
      });
  });
  return { body: applyEdits(text, edits), headings, fences };
}
