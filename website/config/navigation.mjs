import { assertApiReferenceExcluded } from './api-reference.mjs';

const groups = [
  'Start here',
  'Tools and shell access',
  'Subagents and agent teams',
  'ChatML workflows',
  'Complete applications',
  'Run and operate',
  'Reference',
  'Commands',
  'ChatML reference',
  'Agent protocol',
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
    ['Start here', ['Start here']],
    ['Tools and shell access', ['Tools and shell access']],
    ['Subagents and agent teams', ['Subagents and agent teams']],
    ['ChatML workflows', ['ChatML workflows']],
    ['Complete applications', ['Complete applications']],
    ['Run and operate', ['Run and operate']],
    [
      'Reference',
      ['Reference', 'Commands', 'ChatML reference', 'Agent protocol'],
    ],
    [
      'Contributor documentation',
      ['Library', 'Agent libraries', 'TUI internals', 'Compatibility'],
    ],
  ];
  return families
    .map(([label, members]) => ({
      label,
      collapsed: label !== 'Start here',
      items: members.flatMap((member) => {
        const section = sections.find((section) => section.label === member);
        return section
          ? members.length === 1
            ? section.items
            : [section]
          : [];
      }),
    }))
    .filter((group) => group.items.length);
}

const paths = [
  {
    id: 'shell/README',
    title: 'Build tools with guardrails',
    description:
      'Give an agent useful command-line capabilities, then customize access with shell runtimes, ChatML rules, and reviewer agents.',
    label: 'Tools and shell access',
  },
  {
    id: 'guide/subagents',
    title: 'Work with a team of specialists',
    description:
      'Choose one-off reviews, continuing conversations, or agents created for the task. Keep their tools and responsibilities explicit.',
    label: 'Subagents and agent teams',
  },
  {
    id: 'chatml',
    title: 'Program the workflow',
    description:
      'Sequence tools, retain workflow state, coordinate sessions, and deliver background results with ChatML.',
    label: 'ChatML workflows',
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
