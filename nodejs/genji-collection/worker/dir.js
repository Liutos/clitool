'use strict';

const cheerio = require('cheerio');

function parse(content) {
  const $ = cheerio.load(content);
  const as = $('#comiclistn > dd').find('a');
  let next = [];
  for (let i = 0; i < as.length; i += 1) {
    const $a = $(as[i]);
    if ($a.text().indexOf('源君物语') !== -1) {
      const href = $a.attr('href');
      if (href) {
        next.push('http://comic.kukudm.com' + href);
      }
    }
  }
  return {
    img: null,
    next
  };
}

exports.encoding = 'GBK';
exports.parse = parse;
