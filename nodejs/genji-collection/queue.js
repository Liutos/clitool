'use strict';

const request = require('co-request');

const QUEUE_HOST = '127.0.0.1';
const QUEUE_PORT = 1219;

function* pull(name) {
  const host = QUEUE_HOST;
  const port = QUEUE_PORT;
  const url = `http://${host}:${port}/`;

  const response = yield request({
    qs: {
      name,
      opt: 'get'
    },
    url
  });
  const body = response.body;

  if (body === 'HTTPSQS_GET_END') {
    return null;
  }
  return body;
}

function* push(img, name) {
  const host = QUEUE_HOST;
  const port = QUEUE_PORT;
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

exports.pull = pull;
exports.push = push;
