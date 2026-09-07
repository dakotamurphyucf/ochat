import { visit, SKIP } from 'unist-util-visit';
export default function readingAccessibility() {
  return (tree) => {
    visit(tree, 'element', (node, index, parent) => {
      if (node.tagName === 'pre') {
        node.properties ||= {};
        Object.assign(node.properties, {
          tabIndex: 0,
          role: 'region',
          ariaLabel: 'Code example',
        });
      }
      if (node.tagName === 'table' && parent && typeof index === 'number') {
        // Keep the native table structure inside a separately focusable scroller.
        // Touch/scrollbars work without JS; the shell supplements WebKit keys.
        parent.children[index] = {
          type: 'element',
          tagName: 'div',
          properties: {
            className: ['table-scroll'],
            tabIndex: 0,
            role: 'region',
            ariaLabel: 'Scrollable data table',
          },
          children: [node],
        };
        return SKIP;
      }
    });
  };
}
