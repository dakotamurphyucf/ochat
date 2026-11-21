import fs from 'node:fs/promises';
import { ident, parse as parseCss, walk as walkCss } from 'css-tree';
import { parse } from 'parse5';

const codeStylesheet = /^\/_astro\/ec\.[^/.]+\.css$/;
const resourceFunctions = new Set([
  'url',
  'src',
  'image',
  'image-set',
  '-webkit-image-set',
]);

async function loadInlineCss(dir, href) {
  const css = await fs.readFile(new URL(`.${href}`, dir), 'utf8');
  const reject = () => {
    throw new Error(`${href}: expected self-contained generated CSS`);
  };
  if (/<\/style/i.test(css)) reject();
  walkCss(
    parseCss(css, { parseCustomProperty: true, onParseError: reject }),
    (node) => {
      if (node.type === 'Raw') reject();
      if (node.type === 'Url' && !/^data:/i.test(node.value)) reject();
      if (
        node.type === 'Atrule' &&
        ident.decode(node.name).toLowerCase() === 'import'
      )
        reject();
      if (node.type === 'Function') {
        const name = ident.decode(node.name).toLowerCase();
        if (resourceFunctions.has(name)) reject();
      }
    },
  );
  return css;
}

async function placeStylesheet(html, dir, route) {
  const document = parse(html, { sourceCodeLocationInfo: true });
  const root = document.childNodes.find((node) => node.tagName === 'html');
  const head = root?.childNodes.find((node) => node.tagName === 'head');
  const links = [];

  function collect(node) {
    if (node.tagName === 'link') {
      const { rel, href } = Object.fromEntries(
        node.attrs.map(({ name, value }) => [name, value]),
      );
      if (rel === 'stylesheet' && codeStylesheet.test(href))
        links.push({ href, location: node.sourceCodeLocation });
    }
    for (const child of node.childNodes || []) collect(child);
  }
  collect(document);

  const [link] = links;
  const headEnd = head?.sourceCodeLocation?.endTag?.startOffset;
  if (links.length !== 1 || !link?.location || headEnd === undefined)
    throw new Error(
      `${route}: expected one generated code stylesheet and head`,
    );
  const { startOffset: start, endOffset: end } = link.location;
  const css = await loadInlineCss(dir, link.href);
  const style = `<style data-ochat-protocol-code>${css}</style>`;
  if (end <= headEnd) return html.slice(0, start) + style + html.slice(end);
  return (
    html.slice(0, headEnd) +
    style +
    html.slice(headEnd, start) +
    html.slice(end)
  );
}

// Expressive Code first links its stylesheet in the article body. On this
// dense page, a late stylesheet can move a native fragment out of view as
// thousands of source lines change height. Put the self-contained rules in
// the head so the first layout does not depend on a separate request.
export default function protocolStylesheet(route) {
  return {
    name: 'ochat-protocol-stylesheet',
    hooks: {
      'astro:build:generated': async ({ dir }) => {
        const file = new URL(`.${route}index.html`, dir);
        const html = await fs.readFile(file, 'utf8');
        await fs.writeFile(file, await placeStylesheet(html, dir, route));
      },
    },
  };
}
