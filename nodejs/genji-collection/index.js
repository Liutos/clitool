'use strict';

const queue = require('./queue');

const router = require('./worker/router');

const _ = require('lodash');
const co = require('co');
const sleep = require('sleep');

const TASK_QUEUE = 'genji-task';

function* start(uri) {
  const mod = router.dispatch(uri);
  if (mod) {
    console.log('Fetch and parse URI: ' + uri);
    const {
      img,
      next
    } = yield router.invoke(mod, uri);
    if (img) {
      yield queue.push(img, 'genji-page');
    }
    if (typeof next === 'string') {
      yield queue.push(next, TASK_QUEUE);
    } else if (_.isArray(next)) {
      for (const uri of next) {
        yield queue.push(uri, TASK_QUEUE);
      }
      console.log(next.length + ' task created');
    }
  }
}

co(function* () {
  let uri;
  do {
    const uri = yield queue.pull(TASK_QUEUE);
    if (uri) {
      yield start(uri);
      sleep.sleep(1);
    }
  } while (!uri);
}).catch(function (err) {
  console.error(err);
});
