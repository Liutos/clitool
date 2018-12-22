const request = require('co-request');

const fs = require('fs');

function parseFaqOrg(path) {
  const content = fs.readFileSync(path).toString('utf-8');
  const lines = content.split('\n');
  const qas = [];
  let answer = [];
  let lineNum = 0;
  let mode;
  let question;
  let questionLineNum;
  for (const line of lines) {
    lineNum += 1;
    if (line.startsWith('*')) {
      if (mode === 'answer') {
        // 在遇到星号的时候模式已经处于answer中，说明在此之前还有未处理的QA
        qas.push({
          answer: answer.join('\n'),
          path,
          question,
          questionLineNum
        });
        answer = [];
        question = null;
      }
      mode = 'question';
    } else {
      mode = 'answer';
    }
    if (mode === 'answer') {
      answer.push(line);
    } else {
      question = line;
      questionLineNum = lineNum;
    }
  }
  if (question) {
    qas.push({
      answer: answer.join('\n'),
      question
    });
  }
  // console.log(JSON.stringify(qas, null, 2));
  return qas;
}

async function dropFaq() {
  await request({
    method: 'delete',
    url: 'http://localhost:9200/faq'
  });
}

/**
 * 重建faq索引并写入全量的笔记数据
 */
async function main() {
  await dropFaq();
  const dir = '/Users/liutos/Documents/Projects/my_note/faq/';
  const basenames = fs.readdirSync(dir);
  for (const basename of basenames) {
    const path = dir + basename;
    const type = basename.match(/(.*)\.org/)[1];
    const qas = parseFaqOrg(path);
    for (const qa of qas) {
      await request({
        body: qa,
        json: true,
        method: 'post',
        url: 'http://localhost:9200/faq/_doc'
      });
    }
    console.log(`文件${path}处理完毕`);
  }
}

main();