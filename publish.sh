#!/bin/bash -e

npm run build
cd js-dist
cp ../package.json .
npm publish
