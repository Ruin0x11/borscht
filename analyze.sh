#!/usr/bin/env bash

set -e

cargo run --release -- analyze -s -o ../custom-gx/1.90-borscht ../elonaplus1.90/start.ax
cargo run --release -- analyze -s -o ../custom-gx/2.05-borscht ../elonaplus2.05/elonaplus.ax
cargo run --release -- analyze -s -o ../custom-gx/2.06-borscht ../elonaplus2.06/start.ax
cargo run --release -- print-vars database/plus1.90.ron > ../custom-gx/1.90.hsp
cargo run --release -- print-vars database/plus2.05.ron > ../custom-gx/2.05.hsp
cargo run --release -- print-vars database/plus2.06.ron > ../custom-gx/2.06.hsp

set +e

unix2dos ../custom-gx/1.90-borscht/*.hsp
unix2dos ../custom-gx/2.05-borscht/*.hsp
unix2dos ../custom-gx/2.06-borscht/*.hsp

pushd ../custom-gx/diff
diff -U5 --recursive ../1.90-borscht/ ../2.05-borscht/ | unix2dos > 1.90-to-2.05.diff
diff -U5 --recursive ../2.05-borscht/ ../2.06-borscht/ | unix2dos > 2.05-to-2.06.diff
git status
git add ../1.90-borscht ../2.05-borscht ../2.06-borscht
git add 1.90-to-2.05.diff 2.05-to-2.06.diff
popd
