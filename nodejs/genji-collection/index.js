'use strict';

const queue = require('./queue');

const router = require('./worker/router');

const _ = require('lodash');
const co = require('co');
const sleep = require('sleep');

const TASK_QUEUE = 'genji-task';

function* start(task) {
  const uri = task.entry;
  const mod = router.dispatch(uri);
  if (mod) {
    console.log('Fetch and parse URI: ' + uri);
    const {
      img,
      next
    } = yield router.invoke(mod, uri);
    if (img) {
      yield queue.push(JSON.stringify(_.extend({}, task, {
        source: img
      })), 'genji-page');
    }
    for (const n of next) {
      yield queue.push(JSON.stringify(_.extend({}, task, n)), TASK_QUEUE);
    }
    console.log(next.length + ' task created');
  }
}

co(function* () {
  let raw;
  do {
    raw = yield queue.pull(TASK_QUEUE);
    if (raw) {
      const task = JSON.parse(raw);
      yield start(task);
      sleep.sleep(1);
    }
  } while (raw);
}).catch(function (err) {
  console.error(err);
});
