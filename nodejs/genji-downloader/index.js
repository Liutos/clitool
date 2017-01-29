'use strict';

const queue = require('./queue');

const co = require('co');
const mkdirp = require('mkdirp');
const request = require('co-request');
const sleep = require('sleep');

const fs = require('fs');

function* start(task) {
  // 下载图片
  const response = yield request({
    encoding: null,
    uri: encodeURI(task.source)
  });
  const body = response.body;
  // 创建目录
  const dir = '/home/liutos/data/genji-story/' + task.dir;
  if (!fs.existsSync(dir)) {
    mkdirp.sync(dir);
  }
  // 从entry中分析出文件名
  const fileno = task.entry.match(/\/([0-9]+)\.htm$/)[1];
  const suffix = task.source.match(/\.([^\/\.]+)$/)[1];
  const path = '/home/liutos/data/genji-story/' + task.dir + '/' + fileno + '.' + suffix;
  // 写入文件
  fs.writeFileSync(path, body);
  console.log(path + ':文件已写入');
}

co(function* () {
  let raw;
  do {
    raw = yield queue.pull('genji-page');
    if (raw) {
      const task = JSON.parse(raw);
      yield start(task);
      sleep.sleep(1);
    }
  } while (raw);
  // yield start({
  //   dir: '源君物语 Vol_1',
  //   entry: 'http://comic.kukudm.com/comiclist/2036/43239/1.htm',
  //   source: 'http://n.1whour.com/newkuku/2014/201412/1205b/源君物语/YjwyVol_01/Yjwy01-001AFJ.jpg'
  // });
}).catch(function (err) {
  console.error(err);
});
