import fs from 'node:fs/promises';
import path from 'node:path';
import satori from 'satori';
import sharp from 'sharp';
import { digest } from './provenance.mjs';
import {
  socialPath,
  homeDescription,
  brandColors,
} from '../config/presentation.mjs';
const site = new URL('../', import.meta.url).pathname;
const el = (type, style, children) => ({ type, props: { style, children } });
export async function publishingAssets(stage, pages) {
  const fontPath =
    'node_modules/@fontsource/manrope/files/manrope-latin-700-normal.woff';
  const font = await fs.readFile(path.join(site, fontPath));
  const icon = await fs.readFile(path.join(site, 'public/favicon.svg'));
  const assets = [];
  const policy = JSON.parse(
    await fs.readFile(path.join(site, 'config/media.json')),
  );
  const publicFiles = async (dir, prefix = '') => {
    const names = [];
    for (const item of await fs.readdir(dir, { withFileTypes: true })) {
      const name = prefix + '/' + item.name;
      if (item.isDirectory())
        names.push(...(await publicFiles(path.join(dir, item.name), name)));
      else names.push(name);
    }
    return names;
  };
  const actualPublic = (await publicFiles(path.join(site, 'public'))).sort();
  if (
    JSON.stringify(actualPublic) !==
    JSON.stringify(policy.public.map((a) => a.url).sort())
  )
    throw new Error(
      'Every authored public asset requires a media manifest owner',
    );
  for (const record of policy.public) {
    const data = await fs.readFile(path.join(site, 'public', record.url));
    await fs.access(path.join(site, 'public', record.license));
    assets.push({
      ...record,
      kind: 'authored',
      bytes: data.length,
      sha256: digest(data),
    });
  }
  const emit = async (url, buffer, record) => {
    const file = path.join(stage, 'public', url);
    await fs.mkdir(path.dirname(file), { recursive: true });
    await fs.writeFile(file, buffer);
    const { width, height } = await sharp(buffer).metadata();
    assets.push({
      url,
      ...record,
      width,
      height,
      bytes: buffer.length,
      sha256: digest(buffer),
    });
  };
  for (const [url, size] of [
    ['/favicon-32.png', 32],
    ['/apple-touch-icon.png', 180],
    ['/icon-192.png', 192],
    ['/icon-512.png', 512],
  ]) {
    await emit(url, await sharp(icon).resize(size, size).png().toBuffer(), {
      kind: 'icon',
      source: 'website/public/favicon.svg',
      license: '/licenses/ochat.txt',
    });
  }
  for (const page of [
    {
      route: '/',
      title: 'Build agents.\nKeep control.',
      section: 'JUST TEXT FILES',
      description: homeDescription,
    },
    ...pages,
  ]) {
    const label =
      page.disposition === 'compatibility'
        ? 'COMPATIBILITY'
        : page.disposition === 'bridge'
          ? 'PREVIOUS LOCATION'
          : page.status === 'experimental'
            ? `${page.section} · EXPERIMENTAL`
            : page.section;
    const titleSize =
      page.title.length > 65 ? 48 : page.title.length > 40 ? 58 : 72;
    const svg = await satori(
      el(
        'div',
        {
          width: 1200,
          height: 630,
          display: 'flex',
          flexDirection: 'column',
          padding: '52px 64px',
          background: brandColors.background,
          color: brandColors.foreground,
          fontFamily: 'Manrope',
          fontWeight: 700,
          position: 'relative',
        },
        [
          el('div', { display: 'flex', alignItems: 'center', gap: 18 }, [
            {
              type: 'img',
              props: {
                src: `data:image/svg+xml;base64,${icon.toString('base64')}`,
                width: 52,
                height: 52,
              },
            },
            el('div', { fontSize: 42, letterSpacing: '-2px' }, 'ochat'),
          ]),
          el(
            'div',
            {
              fontSize: 18,
              color: brandColors.accent,
              marginTop: 52,
              letterSpacing: '2px',
            },
            label.toUpperCase(),
          ),
          el(
            'div',
            {
              fontSize: titleSize,
              lineHeight: 1.12,
              letterSpacing: '-2px',
              marginTop: 22,
              maxWidth: 1000,
              whiteSpace: 'pre-wrap',
            },
            page.title,
          ),
          el(
            'div',
            {
              display: 'flex',
              marginTop: 'auto',
              paddingTop: 24,
              borderTop: `1px solid ${brandColors.line}`,
              fontSize: 20,
              color: brandColors.muted,
              justifyContent: 'space-between',
            },
            [
              el('span', {}, 'Instructions. Tools. Workflows.'),
              el(
                'span',
                { color: brandColors.accent },
                page.route === '/' ? 'Open-source · OCaml' : 'Documentation',
              ),
            ],
          ),
        ],
      ),
      {
        width: 1200,
        height: 630,
        fonts: [{ name: 'Manrope', data: font, weight: 700, style: 'normal' }],
      },
    );
    await emit(
      socialPath(page.route),
      await sharp(Buffer.from(svg)).png().toBuffer(),
      {
        kind: 'social',
        route: page.route,
        title: page.title,
        source: 'website/scripts/publishing-assets.mjs + docs manifest',
        license: '/licenses/ochat.txt',
        font: {
          source: fontPath,
          sha256: digest(font),
          license: '/licenses/manrope.txt',
        },
      },
    );
  }
  await fs.writeFile(
    path.join(stage, 'publishing-assets.json'),
    JSON.stringify(
      {
        version: 1,
        policy,
        generator:
          'Satori + Sharp; deterministic local templates; no runtime provider calls',
        assets,
      },
      null,
      2,
    ) + '\n',
  );
}
