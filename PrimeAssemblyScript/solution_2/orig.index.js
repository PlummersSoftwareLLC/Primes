'use strict';

import fs from 'node:fs/promises';
import loader from "@assemblyscript/loader";
import { WASI } from 'wasi';

const wasi = new WASI({ version: 'preview1' });
const binary = fs.readFile("./dist/orig.wasm");
const result = await loader.instantiate(binary, { wasi_snapshot_preview1: wasi.wasiImport });

wasi.start(result);
result.exports.bench();
