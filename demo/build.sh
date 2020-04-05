#!/bin/bash -e

elm-app build
sed -i '.bak' 's+/static+static+g' build/index.html
