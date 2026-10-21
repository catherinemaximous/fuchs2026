// Tiny local server for the experiment (no npm packages needed).
// Run:  node server.js     then open  http://localhost:3000/?p=1
const http = require('http');
const fs = require('fs');
const path = require('path');

const PORT = process.env.PORT || 3000;
const AUDIO_DIR = process.env.AUDIO_DIR || 'C:\\Users\\cathe\\Documents\\fuchs2026-replication-jspsych\\Audio files';
const IMG_DIR   = process.env.IMG_DIR   || 'C:\\Users\\cathe\\Documents\\fuchs2026-replication-jspsych\\Images';
const DATA_DIR  = path.join(__dirname, 'data');   // finished participants are saved here
const COUNTER_FILE = path.join(DATA_DIR, 'counter.json');  // remembers the last list number handed out

const MIME = { '.html': 'text/html; charset=utf-8', '.js': 'text/javascript', '.css': 'text/css',
               '.wav': 'audio/wav', '.mp3': 'audio/mpeg', '.ogg': 'audio/ogg',
               '.jpg': 'image/jpeg', '.jpeg': 'image/jpeg', '.png': 'image/png', '.json': 'application/json' };

function send(res, code, type, body) {
  res.writeHead(code, { 'Content-Type': type, 'Cache-Control': 'no-store' });
  res.end(body);
}
function sendFile(res, filePath) {
  fs.readFile(filePath, (err, data) => {
    if (err) return send(res, 404, 'text/plain', 'Not found: ' + path.basename(filePath));
    send(res, 200, MIME[path.extname(filePath).toLowerCase()] || 'application/octet-stream', data);
  });
}
function listDir(dir, exts) {
  try { return fs.readdirSync(dir).filter(f => exts.includes(path.extname(f).toLowerCase())); }
  catch (e) { console.error('Cannot read folder: ' + dir + '\n  ' + e.message); return []; }
}

const server = http.createServer((req, res) => {
  const url = new URL(req.url, 'http://localhost');
  const p = decodeURIComponent(url.pathname);

  if (p === '/' || p === '/index.html') return sendFile(res, path.join(__dirname, 'index.html'));

  if (p === '/api/files') {
    return send(res, 200, MIME['.json'], JSON.stringify({
      audio: listDir(AUDIO_DIR, ['.wav', '.mp3', '.ogg']),
      images: listDir(IMG_DIR, ['.jpg', '.jpeg', '.png'])
    }));
  }
  if (p.startsWith('/audio/')) return sendFile(res, path.join(AUDIO_DIR, path.basename(p)));
  if (p.startsWith('/img/'))   return sendFile(res, path.join(IMG_DIR, path.basename(p)));

  if (p === '/api/next') {                       // hands out list 1, 2, 3, ... one per page load
    fs.mkdirSync(DATA_DIR, { recursive: true });
    let n = 0;
    try { n = Number(JSON.parse(fs.readFileSync(COUNTER_FILE, 'utf8')).last) || 0; } catch (e) {}
    n += 1;
    fs.writeFileSync(COUNTER_FILE, JSON.stringify({ last: n }));
    console.log('assigned list ' + n);
    return send(res, 200, MIME['.json'], JSON.stringify({ p: n }));
  }

  if (p === '/api/save' && req.method === 'POST') {
    let body = '';
    req.on('data', c => { body += c; });
    req.on('end', () => {
      const pid = String(url.searchParams.get('p') || 'x').replace(/[^0-9a-zA-Z_-]/g, '');
      fs.mkdirSync(DATA_DIR, { recursive: true });
      const name = `picmatch_p${pid}_${new Date().toISOString().replace(/[:.]/g, '-')}.csv`;
      fs.writeFileSync(path.join(DATA_DIR, name), body, 'utf8');
      console.log('saved ' + name);
      send(res, 200, 'text/plain', 'ok');
    });
    return;
  }
  send(res, 404, 'text/plain', 'Not found');
});

server.listen(PORT, () => {
  console.log('Audio folder : ' + AUDIO_DIR + '  (' + listDir(AUDIO_DIR, ['.wav', '.mp3', '.ogg']).length + ' audio files)');
  console.log('Image folder : ' + IMG_DIR + '  (' + listDir(IMG_DIR, ['.jpg', '.jpeg', '.png']).length + ' images)');
  console.log('Participants: open  http://localhost:' + PORT + '/   (list numbers are assigned automatically)');
  console.log('Testing:      open  http://localhost:' + PORT + '/?p=1&debug=1   (fixed list, does not use up a number)');
});
