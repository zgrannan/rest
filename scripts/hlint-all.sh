#!/usr/bin/env bash

find src -name \*.hs | while read SRC_FILE ; do
    hlint "$SRC_FILE" --refactor --refactor-options=-i
done
