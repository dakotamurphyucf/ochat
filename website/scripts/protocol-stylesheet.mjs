import fs from 'node:fs/promises';
import { parse } from 'parse5';

const codeStylesheet = /^\/_astro\/ec\.[^/.]+\.css$/;

function placeStylesheet(html, route) {
  const document = parse(html, { sourceCodeLocationInfo: true });
  const root = document.childNodes.find((node) => node.tagName === 'html');
  const head = root?.childNodes.find((node) => node.tagName === 'head');
  const links = [];

  function collect(node) {
    if (node.tagName === 'link') {
      const { rel, href } = Object.fromEntries(
        node.attrs.map(({ name, value }) => [name, value]),
      );
      if (rel === 'stylesheet' && codeStylesheet.test(href)) links.push(node);
    }
    for (const child of node.childNodes || []) collect(child);
  }
  collect(document);

  const [link] = links;
  const location = link?.sourceCodeLocation;
  const headEnd = head?.sourceCodeLocation?.endTag?.startOffset;
  if (links.length !== 1 || !location || headEnd === undefined)
    throw new Error(
      `${route}: expected one generated code stylesheet and head`,
    );
  if (link.parentNode === head) return html;

  const { startOffset: start, endOffset: end } = location;
  if (start < headEnd)
    throw new Error(
      `${route}: generated code stylesheet has no expected location`,
    );
  return (
    html.slice(0, headEnd) +
    html.slice(start, end) +
    html.slice(headEnd, start) +
    html.slice(end)
  );
}

// Expressive Code first links its stylesheet in the article body. On this
// dense page, a late stylesheet can move a native fragment out of view as
// thousands of source lines change height. Fetch it with the head styles.
export default function protocolStylesheet(route) {
  return {
    name: 'ochat-protocol-stylesheet',
    hooks: {
      'astro:build:generated': async ({ dir }) => {
        const file = new URL(`.${route}index.html`, dir);
        const html = await fs.readFile(file, 'utf8');
        const updated = placeStylesheet(html, route);
        if (updated !== html) await fs.writeFile(file, updated);
      },
    },
  };
}
