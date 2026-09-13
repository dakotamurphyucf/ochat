// Website links for the stable topics named by the installed authoring primer.
// Targets follow authoring_corpus.ml's source excerpts; this does not alter the
// installed corpus or claim that dynamic reference.tools/signatures are static.
const guide = (name, fragment = '') =>
  `docs-src/guide/${name}.md${fragment ? `#${fragment}` : ''}`;

export const authoringTopicLinks = {
  'chatml.types': guide(
    'chatml-ocaml-differences',
    'records-are-structural-with-conservative-joins',
  ),
  'chatml.programs': guide('chatml-authoring-language'),
  'chatml.task-effects': guide('chatml-task-effects'),
  'runtime.invocations.one-off': guide(
    'chatml-authoring-runtime',
    'one-off-tool-using-computations',
  ),
  'runtime.invocations.standalone': guide(
    'chatml-authoring-runtime',
    'standalone-tools-and-explicit-outcomes',
  ),
  'runtime.invocations.moderator': guide(
    'chatml-authoring-runtime',
    'moderator-tools-and-session-owned-state',
  ),
  'runtime.jobs.acknowledgement': guide(
    'chatml-authoring-background',
    'acknowledge-before-publishing-a-result',
  ),
  'runtime.jobs.timers': guide(
    'chatml-authoring-background',
    'schedule-checks-and-choose-recovery-behavior',
  ),
  'runtime.delivery.notifications': guide(
    'chatml-authoring-background',
    'publish-data-and-request-a-model-turn',
  ),
  'runtime.delivery.ingress': guide(
    'chatml-authoring-background',
    'receive-external-completion-data',
  ),
  'runtime.delegation.stop-helper': guide(
    'chatml-authoring-children',
    'stop-and-use-the-shared-helper-path',
  ),
  'runtime.recovery.background': guide(
    'chatml-authoring-background',
    'keep-transaction-and-restart-guarantees-precise',
  ),
  'reference.tools': guide(
    'authoring-context-tool',
    'interpret-availability-and-completeness',
  ),
  'reference.signatures': guide(
    'authoring-context-tool',
    'interpret-availability-and-completeness',
  ),
};
