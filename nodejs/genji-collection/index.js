'use strict';

const co = require('co');
const iconv = require('iconv-lite');
const request = require('co-request');

/**
 * @param {string} uri - 可获得漫画图片的页面地址，例如http://comic.kukudm.com/comiclist/2036/43239/100.htm
 */
function* fetchAndParse(uri) {
  const response = yield request({
    encoding: null,
    uri
  });
  const statusCode = response.statusCode;
  if (statusCode === 200) {
    const octets = response.body;
    const content = iconv.decode(octets, 'GBK');
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
  } else {
    throw new Error(statusCode + ':' + uri);
  }
}

co(function* () {
  // const uri = 'http://comic.kukudm.com/comiclist/2036/43239/5.htm';
  const uri = 'http://comic.kukudm.com/comiclist/2036/43239/100.htm';
  const { img, next } = yield fetchAndParse(uri);
  console.log(img);
  console.log(next);
}).catch(function (err) {
  console.error(err);
});
