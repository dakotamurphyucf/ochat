// Only the alternate hostname invokes this Worker. The main site stays static.
export default {
  fetch(request) {
    const url = new URL(request.url);
    if (url.hostname !== 'www.ochatlabs.com')
      return new Response('Not found', { status: 404 });
    url.protocol = 'https:';
    url.hostname = 'ochatlabs.com';
    url.port = '';
    return Response.redirect(url.href, 308);
  },
};
