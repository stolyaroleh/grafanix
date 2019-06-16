#!/bin/sh

watch() {
  inotifywait --recursive ./src --include ".*\.elm" \
    --event modify
  elm make ./src/Main.elm --output ../static/main.js
}

while true
do
  watch
done
