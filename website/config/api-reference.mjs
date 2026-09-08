// P09 release decision. Inclusion requires a new artifact review and pipeline;
// copying docs/ or _build/ into the prose site is never an opt-in mechanism.
export const apiReference = Object.freeze({
  status: 'deferred',
  landing: '/docs/integrations/ocaml/',
  reason:
    'Fresh odoc output has unresolved references and broken mounted links.',
  review: 'planning/p09-completion-review.md',
});

export function assertApiReferenceExcluded({ files = [], urls = [], origin }) {
  for (const file of files) {
    if (/^api(?:\/|$)/.test(file.replaceAll('\\', '/')))
      throw new Error(`API reference is deferred: unexpected output ${file}`);
  }
  for (const href of urls) {
    const url = new URL(href, origin);
    if (
      url.origin === new URL(origin).origin &&
      /^\/api(?:\/|$)/.test(decodeURIComponent(url.pathname))
    )
      throw new Error(`API reference is deferred: unexpected URL ${href}`);
  }
}
