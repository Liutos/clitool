'use strict';

function parse(content) {
  const img = 'http://n.1whour.com/' + content.match(/newkuku.*?\.jpg/)[0];
  const nextPattern = /(comiclist.*?\.htm)/g;
  const matched = content.match(nextPattern);
  let next = matched[matched.length - 1] || null;
  if (next) {
    next = {
      entry: 'http://comic.kukudm.com/' + next
    };
  }
  return {
    img,
    next: [next]
  };
}

exports.encoding = 'GBK';
exports.parse = parse;
