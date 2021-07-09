import loader from "@assemblyscript/loader";
import fs from 'fs';
import { WASI } from 'wasi';
const w = new WASI();
loader.instantiate(
    fs.promises.readFile("./dist/release.wasm"), {
    wasi_snapshot_preview1: w.wasiImport
}).then((result) => {
    w.start(result);
    result.exports.bench()
}).catch((err) => {
    console.error(err);
})
