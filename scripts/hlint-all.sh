#!/usr/bin/env bash

find src -name \*.hs | while read SRC_FILE ; do
    hlint "$SRC_FILE" --refactor --refactor-options=-i
done

find test -name \*.hs | while read SRC_FILE ; do
    hlint "$SRC_FILE" --refactor --refactor-options=-i
done

find testlib -name \*.hs | while read SRC_FILE ; do
    hlint "$SRC_FILE" --refactor --refactor-options=-i
done
