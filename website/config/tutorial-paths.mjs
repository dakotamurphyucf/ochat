// Explicit membership keeps new lessons visible and avoids a single chain through
// unrelated features. Page IDs and tutorial IDs remain stable as paths grow.
export const tutorialPaths = [
  {
    id: 'foundation',
    title: 'Create and compose',
    description:
      'Start with a local conversation, project evidence and a specialist.',
    pages: ['first-agent', 'tutorials/file-tool', 'tutorials/specialist'],
  },
  {
    id: 'shell',
    title: 'Tools and shell access',
    description:
      'Give an agent command-line capabilities with explicit guardrails.',
    pages: ['agent-server/tutorials/shell-agent', 'tutorials/shell-guardrails'],
  },
  {
    id: 'chatml',
    title: 'ChatML workflows',
    description:
      'Sequence file tools, package reusable logic, then control a conversation.',
    pages: [
      'tutorials/chatml-program',
      'tutorials/chatml-tool',
      'tutorials/workflow',
    ],
  },
  {
    id: 'operating',
    title: 'Run and host',
    description:
      'Optional paths: batch requests, daemon hosting and background scheduling.',
    pages: [
      'cli/chat-completion',
      'agent-server/tutorials/unix-daemon',
      'background',
    ],
  },
  {
    id: 'clients',
    title: 'Connect external clients',
    description:
      'Optional integration path for stdio and authenticated HTTP clients.',
    pages: ['stdio', 'agent-server/tutorials/http-client'],
  },
];

export function resolveTutorialPaths(tutorials, paths = tutorialPaths) {
  const byPage = new Map(
    tutorials.map((tutorial) => [tutorial.page, tutorial]),
  );
  const seen = new Set();
  const groups = paths.map(({ pages, ...group }) => ({
    ...group,
    tutorials: pages.map((page) => {
      if (!byPage.has(page))
        throw new Error(`Unknown tutorial in learning path: ${page}`);
      if (seen.has(page))
        throw new Error(`Duplicate tutorial in learning paths: ${page}`);
      seen.add(page);
      return byPage.get(page);
    }),
  }));
  for (const page of byPage.keys()) {
    if (!seen.has(page))
      throw new Error(`Tutorial missing from learning paths: ${page}`);
  }
  return groups;
}

export function tutorialNeighbors(page) {
  const path = tutorialPaths.find((path) => path.pages.includes(page));
  if (!path) throw new Error(`Tutorial missing from learning paths: ${page}`);
  const index = path.pages.indexOf(page);
  return {
    previous: path.pages[index - 1] ?? null,
    next: path.pages[index + 1] ?? 'tutorials',
  };
}
