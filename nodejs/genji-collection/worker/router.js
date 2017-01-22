'use strict';

const comicDir = require('./dir');
const comicPage = require('./page');

const iconv = require('iconv-lite');
const request = require('co-request');

function dispatch(uri) {
  if (uri === 'http://comic.kukudm.com/comiclist/2036/') {
    return comicDir;
  } else {
    return comicPage;
  }
}

function* invoke(mod, uri) {
  const response = yield request({
    encoding: null,
    uri
  });
  const {
    body,
    statusCode
  } = response;
  if (statusCode !== 200) {
    throw new Error(statusCode + ':' + uri);
  }
  const content = iconv.decode(body, mod.encoding);
  return mod.parse(content);
}

exports.dispatch = dispatch;
exports.invoke = invoke;
