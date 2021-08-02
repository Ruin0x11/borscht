#!/usr/bin/env bash

cargo run --release -- analyze -s -o ../custom-gx/1.90-borscht ../elonaplus1.90/start.ax
cargo run --release -- analyze -s -o ../custom-gx/2.05-borscht ../elonaplus2.05/elonaplus.ax
cargo run --release -- print-vars database/plus1.90.ron > ../custom-gx/1.90.hsp
cargo run --release -- print-vars database/plus2.05.ron > ../custom-gx/2.05.hsp

unix2dos ../custom-gx/1.90-borscht/*
unix2dos ../custom-gx/2.05-borscht/*

pushd ../custom-gx
diff -U5 --recursive 1.90-borscht/ 2.05-borscht/ > 1.90-to-2.05.diff
unix2dos 1.90-to-2.05.diff
git status
popd
