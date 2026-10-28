// etaf-diff-highlight.js --- diff2html JSON with word highlights -*- js -*-

// Usage: NODE_PATH=<dir containing diff2html> node etaf-diff-highlight.js
//
// Reads a unified diff on stdin and prints the same JSON as
// `diff2html -f json`, with one extra field per line: `runs`, a list of
// [text, changed] pairs.  The word highlight comes from diff2html's own
// `diffHighlight`, the function its HTML renderers use, so the changed
// words match the diff2html page.

'use strict';

const { parse } = require('diff2html');
const { diffHighlight } = require('diff2html/lib/render-utils');

const highlightConfig = { diffStyle: 'word', matching: 'none' };

const unescapeHtml = (text) => text
  .replace(/&lt;/g, '<')
  .replace(/&gt;/g, '>')
  .replace(/&quot;/g, '"')
  .replace(/&#x27;/g, "'")
  .replace(/&#x2F;/g, '/')
  .replace(/&amp;/g, '&');

// Turn diffHighlight's HTML (<ins>/<del> around changed words) into runs.
function htmlToRuns(html) {
  const runs = [];
  const pattern = /<(ins|del)[^>]*>([\s\S]*?)<\/\1>|([^<]+)/g;
  let match;
  while ((match = pattern.exec(html)) !== null) {
    if (match[1] !== undefined) {
      runs.push([unescapeHtml(match[2]), true]);
    } else {
      runs.push([unescapeHtml(match[3]), false]);
    }
  }
  return runs;
}

// Pair each run of deleted lines with the following run of inserted lines,
// line by line, as diff2html does, and give every line its runs.
function annotateLines(lines) {
  let i = 0;
  while (i < lines.length) {
    if (lines[i].type === 'context') {
      lines[i].runs = [[lines[i].content, false]];
      i += 1;
      continue;
    }
    const dels = [];
    const ins = [];
    while (i < lines.length && lines[i].type === 'delete') {
      dels.push(lines[i]);
      i += 1;
    }
    while (i < lines.length && lines[i].type === 'insert') {
      ins.push(lines[i]);
      i += 1;
    }
    for (const line of dels.concat(ins)) {
      line.runs = [[line.content, false]];
    }
    const pairs = Math.min(dels.length, ins.length);
    for (let k = 0; k < pairs; k += 1) {
      const highlight = diffHighlight(dels[k].content, ins[k].content,
                                      false, highlightConfig);
      dels[k].runs = [[highlight.oldLine.prefix, false]]
        .concat(htmlToRuns(highlight.oldLine.content));
      ins[k].runs = [[highlight.newLine.prefix, false]]
        .concat(htmlToRuns(highlight.newLine.content));
    }
  }
}

let input = '';
process.stdin.setEncoding('utf8');
process.stdin.on('data', (chunk) => { input += chunk; });
process.stdin.on('end', () => {
  const files = parse(input);
  for (const file of files) {
    for (const block of file.blocks) {
      annotateLines(block.lines);
    }
  }
  process.stdout.write(JSON.stringify(files));
});
