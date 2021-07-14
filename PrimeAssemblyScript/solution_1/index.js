import fs from 'node:fs/promises';
import loader from "@assemblyscript/loader";
import { WASI } from 'wasi';

const wasi = new WASI();

loader.instantiate(fs.readFile("./dist/release.wasm"), {
    wasi_snapshot_preview1: wasi.wasiImport
}).then(result => {
    wasi.start(result);
    result.exports.bench();
}).catch(console.error);
