'use strict';

const queue = require('./queue');

const router = require('./worker/router');

const _ = require('lodash');
const co = require('co');

const TASK_QUEUE = 'genji-task';

function* start(uri) {
  const mod = router.dispatch(uri);
  if (mod) {
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
    }
  }
}

co(function* () {
  const uri = yield queue.pull(TASK_QUEUE);
  if (uri) {
    yield start(uri);
  } else {
    console.info('Nothing to do');
  }
}).catch(function (err) {
  console.error(err);
});
