// Reads the audio/ and img/ folders and writes stimuli.json (the list of file names the page uses).
// Run from the repo folder:   node make_stimuli_json.js      (uses ./stimuli/Audio files and ./stimuli/Images)
//                       or:   node make_stimuli_json.js "C:\path\Audio files" "C:\path\Images"
const fs = require('fs'), path = require('path');
const audioDir = process.argv[2] || path.join(__dirname, 'stimuli', 'Audio files');
const imgDir = process.argv[3] || path.join(__dirname, 'stimuli', 'Images');
const list = (d, exts) => fs.readdirSync(d).filter(f => exts.includes(path.extname(f).toLowerCase())).sort();
const out = { audio: list(audioDir, ['.wav', '.mp3', '.ogg']), images: list(imgDir, ['.jpg', '.jpeg', '.png']) };
fs.writeFileSync(path.join(__dirname, 'stimuli.json'), JSON.stringify(out, null, 1));
console.log(out.audio.length + ' audio files, ' + out.images.length + ' images -> stimuli.json');
