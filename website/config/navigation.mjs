import { assertApiReferenceExcluded } from './api-reference.mjs';

const groups = [
  'Start here',
  'Applications',
  'Concepts',
  'Tutorials',
  'Guides',
  'Shell access',
  'ChatML runtime',
  'MCP integration',
  'Search and indexing',
  'Agent hosting',
  'Agent protocol',
  'Reference',
  'Commands',
  'Learn more',
  'Library',
  'Agent libraries',
  'TUI internals',
  'Compatibility',
];
const published = (entry) =>
  ['publish', 'compatibility', 'bridge'].includes(entry.disposition);

export function buildSidebar(entries) {
  assertApiReferenceExcluded({
    urls: entries.filter((e) => published(e)).map((e) => e.route),
    origin: 'https://docs.ochat.test',
  });
  for (const entry of entries.filter((e) => e.navigation)) {
    if (!published(entry) || !groups.includes(entry.section))
      throw new Error(
        `Unmapped navigation entry: ${entry.id} (${entry.section})`,
      );
  }
  const sections = groups
    .map((label) => ({
      label,
      collapsed: label !== 'Start here',
      items: entries
        .filter((e) => e.navigation && e.section === label)
        .sort(
          (a, b) =>
            (a.order ?? 999) - (b.order ?? 999) ||
            a.id.localeCompare(b.id, 'en'),
        )
        .map((e) => ({ label: e.title, link: e.route })),
    }))
    .filter((group) => group.items.length);
  const families = [
    ['Getting started', ['Start here', 'Applications', 'Concepts']],
    ['Tutorials', ['Tutorials']],
    [
      'Guides',
      [
        'Guides',
        'Shell access',
        'MCP integration',
        'Search and indexing',
        'Agent hosting',
        'Learn more',
      ],
    ],
    [
      'Reference',
      ['Reference', 'Commands', 'ChatML runtime', 'Agent protocol'],
    ],
    [
      'Internals',
      ['Library', 'Agent libraries', 'TUI internals', 'Compatibility'],
    ],
  ];
  return families
    .map(([label, members]) => ({
      label,
      collapsed: label !== 'Getting started',
      items: sections
        .filter((section) => members.includes(section.label))
        .flatMap((section) =>
          members.length === 1 ? section.items : [section],
        ),
    }))
    .filter((group) => group.items.length);
}

const paths = [
  {
    id: 'tutorials',
    title: 'Learn from the beginning',
    description:
      'Follow a connected curriculum, with an outcome and complete source for every lesson.',
    label: 'Tutorials',
  },
  {
    id: 'applications',
    title: 'Build something useful',
    description:
      'Choose a code, documentation, research, or automation workflow and adapt its files.',
    label: 'Applications & guides',
  },
  {
    id: 'chatmd',
    title: 'Understand the model',
    description:
      'Learn how instructions, tools, agent composition, and optional scripting fit together.',
    label: 'Explanations',
  },
  {
    id: 'chatmd-reference',
    title: 'Look up the details',
    description:
      'Find exact ChatMD declarations, then follow links to tool, language, and host contracts.',
    label: 'Reference',
  },
];

export function documentationPaths(entries) {
  return paths.map((item) => {
    const entry = entries.find((e) => e.id === item.id);
    if (!entry || !published(entry) || !entry.route)
      throw new Error(
        `Documentation path requires a published page: ${item.id}`,
      );
    return { ...item, href: entry.route };
  });
}
