#!/bin/bash -e

npm build
cd js-dist
cp ../package.json .
npm publish
