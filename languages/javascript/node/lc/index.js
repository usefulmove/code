#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const files = process.argv.slice(2);

if (files.length === 0) {
  console.error('Usage: lc <file> [file2 ...]');
  process.exit(1);
}

let hasError = false;

for (const file of files) {
  try {
    const content = fs.readFileSync(file);
    const lines = content.toString().split('\n').length - 1;
    console.log(`${lines}\t${file}`);
  } catch (err) {
    console.error(`lc: ${file}: ${err.message}`);
    hasError = true;
  }
}

if (hasError) process.exit(1);
