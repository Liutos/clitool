'use strict';

function parse(content) {
  const img = 'http://n.1whour.com/' + content.match(/newkuku.*?\.jpg/)[0];
  const nextPattern = /(comiclist.*?\.htm)/g;
  let next = content.match(nextPattern)[1] || null;
  if (next) {
    next = 'http://comic.kukudm.com/' + next;
  }
  return {
    img,
    next
  };
}

exports.encoding = 'GBK';
exports.parse = parse;
