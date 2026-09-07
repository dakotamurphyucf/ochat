import { visit } from 'unist-util-visit';
// Expressive Code emits its own HTML after Markdown processing, so apply
// keyboard access in its supported render hook, before serialization.
export default {
  name: 'ochat-keyboard-code',
  hooks: {
    postprocessRenderedBlock({ renderData }) {
      visit(renderData.blockAst, 'element', (node) => {
        if (node.tagName === 'pre')
          Object.assign(node.properties, {
            tabIndex: 0,
            role: 'region',
            'aria-label': 'Code example',
          });
      });
    },
  },
};
