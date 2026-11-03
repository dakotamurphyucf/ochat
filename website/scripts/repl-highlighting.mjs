import { readFileSync } from 'node:fs';

// The REPL walkthrough shares the terminal grammar without changing the
// established highlighting of other documentation or full source readers.
export const replLanguageName = 'chatml-repl';
export const replLanguage = {
  ...JSON.parse(
    readFileSync(
      new URL(
        '../../lib/terminal_highlighting/grammars/chatml.json',
        import.meta.url,
      ),
      'utf8',
    ),
  ),
  name: replLanguageName,
  displayName: 'ChatML',
};
