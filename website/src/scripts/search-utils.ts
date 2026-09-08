export function safeResultUrl(value: string, origin: string): string | null {
  try {
    const url = new URL(value, origin);
    if (
      url.origin !== origin ||
      !url.pathname.startsWith('/docs/') ||
      !url.pathname.endsWith('/') ||
      url.search
    )
      return null;
    return url.pathname + url.hash;
  } catch {
    return null;
  }
}

// Pagefind plain_excerpt removes highlight tags but retains HTML entities.
// Decode only entities, then render through textContent; source tags stay text.
export function decodeExcerpt(value: string): string {
  const names: Record<string, string> = {
    amp: '&',
    lt: '<',
    gt: '>',
    quot: '"',
    apos: "'",
    nbsp: '\u00a0',
  };
  return value.replace(
    /&(#x[0-9a-f]+|#\d+|amp|lt|gt|quot|apos|nbsp);/gi,
    (entity, name: string) => {
      if (!name.startsWith('#')) return names[name.toLowerCase()] || entity;
      const code =
        name[1].toLowerCase() === 'x'
          ? parseInt(name.slice(2), 16)
          : Number(name.slice(1));
      return code > 0 && code <= 0x10ffff && !(code >= 0xd800 && code <= 0xdfff)
        ? String.fromCodePoint(code)
        : entity;
    },
  );
}
