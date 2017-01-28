'use strict';

const request = require('co-request');

function* create(task) {
  const uri = task.entry;
  delete task.uri;
  const response = yield request({
    json: {
      extras: task,
      uri
    },
    method: 'POST',
    uri: 'http://127.0.0.1:8086/task'
  });
  return response.body;
}

exports.create = create;
