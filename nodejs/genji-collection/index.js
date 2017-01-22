'use strict';

const queue = require('./queue');

const router = require('./worker/router');

const co = require('co');

co(function* () {
  const uri = 'http://comic.kukudm.com/comiclist/2036/';
  const mod = router.dispatch(uri);
  const {
    img,
    next
  } = yield router.invoke(mod, uri);
  yield queue.push(img, 'genji-page');
  if (typeof next === 'string') {
    yield queue.push(next, 'genji-task');
  } else {
    for (const uri of next) {
      yield queue.push(uri, 'genji-task');
    }
  }
}).catch(function (err) {
  console.error(err);
});
