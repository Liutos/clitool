'use strict';

const request = require('co-request');

function* push(img, name) {
  const host = '127.0.0.1';
  const port = 1219;
  const text = img;
  const url = `http://${host}:${port}/`;

  yield request({
    qs: {
      data: text,
      name,
      opt: 'put'
    },
    url
  });
}

exports.push = push;
