export const sourceFileId = (example, file) =>
  `source-${example}-${file.replace(/[^a-zA-Z0-9-]/g, (c) => '_' + c.charCodeAt(0).toString(16) + '_')}`;
export const sourceFileHref = (route, example, file) =>
  `${route}#${sourceFileId(example, file)}`;
