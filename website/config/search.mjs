// Measured P07 failures: compatibility pages led MCP and save-session searches.
// Keep them searchable for explicit legacy identifiers, with lower content weight.
export const searchWeight = (entry) =>
  entry?.disposition === 'compatibility' ? 0.1 : undefined;
export const searchStatus = (entry) =>
  entry?.disposition === 'compatibility'
    ? 'Compatibility'
    : entry?.status === 'experimental'
      ? 'Experimental'
      : 'Current';

export const searchKind = (entry) =>
  ({
    tutorial: 'Tutorial',
    guide: 'Guide',
    reference: 'Reference',
    concept: 'Explanation',
    index: 'Overview',
  })[entry?.kind] ||
  (/reference|library|commands|protocol|internals/i.test(entry?.section || '')
    ? 'Reference'
    : /concept/i.test(entry?.section || '')
      ? 'Explanation'
      : 'Guide');
