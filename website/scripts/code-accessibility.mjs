import { visit } from 'unist-util-visit';
// Expressive Code emits its own HTML after Markdown processing, so apply
// keyboard access in its supported render hook, before serialization.
export default {
  name: 'ochat-keyboard-code',
  hooks: {
    postprocessRenderedBlock({ codeBlock, groupContents, renderData }) {
      const displayOnly =
        codeBlock.language === 'text' &&
        codeBlock.props.title?.startsWith('Display only');
      // The Markdown integration supplies page-local group positions. Include
      // the block position too, so repeated or missing titles stay distinct
      // without sharing mutable counters between server-rendered pages.
      const groupIndex =
        codeBlock.parentDocument?.positionInDocument?.groupIndex ?? 0;
      const blockIndex = groupContents.findIndex(
        (entry) => entry.codeBlock === codeBlock,
      );
      const position = `${groupIndex + 1}.${blockIndex + 1}`;
      const label = `${displayOnly ? 'REPL output, display only' : 'Code example'} ${position}${codeBlock.props.title ? `: ${codeBlock.props.title}` : ''}`;
      visit(renderData.blockAst, 'element', (node) => {
        if (node.tagName === 'pre')
          Object.assign(node.properties, {
            tabIndex: 0,
            role: 'region',
            'aria-label': label,
          });
        // Output remains readable/selectable, but cannot be mistaken for a
        // source snippet by its copy action. Source fences keep exact copying.
        if (displayOnly && node.children)
          node.children = node.children.filter(
            (child) =>
              child.type !== 'element' ||
              ![child.properties?.className].flat().includes('copy'),
          );
      });
    },
  },
};
