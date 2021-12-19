#!/usr/bin/env bash

set -e

# cargo run --release -- analyze -s -o ../custom-gx/1.90-borscht ../elonaplus1.90/start.ax
# cargo run --release -- analyze -s -o ../custom-gx/2.05-borscht ../elonaplus2.05/elonaplus.ax
# cargo run --release -- analyze -s -o ../custom-gx/2.06-borscht ../elonaplus2.06/start.ax
# cargo run --release -- analyze -s -o ../custom-gx/2.06fix-borscht ../elonaplus2.06fix/start.ax
# cargo run --release -- analyze -s -o ../custom-gx/2.07-borscht ../elonaplus2.07/start.ax
cargo run --release -- analyze -s -o ../custom-gx/2.08-borscht ../elonaplus2.08/start.ax
# cargo run --release -- print-vars database/plus1.90.ron > ../custom-gx/1.90.hsp
# cargo run --release -- print-vars database/plus2.05.ron > ../custom-gx/2.05.hsp
# cargo run --release -- print-vars database/plus2.06.ron > ../custom-gx/2.06.hsp
# cargo run --release -- print-vars database/plus2.06fix.ron > ../custom-gx/2.06fix.hsp
# cargo run --release -- print-vars database/plus2.07.ron > ../custom-gx/2.07.hsp
cargo run --release -- print-vars database/plus2.08.ron > ../custom-gx/2.08.hsp

set +e

# unix2dos ../custom-gx/1.90-borscht/*.hsp
# unix2dos ../custom-gx/2.05-borscht/*.hsp
# unix2dos ../custom-gx/2.06-borscht/*.hsp
# unix2dos ../custom-gx/2.06fix-borscht/*.hsp
# unix2dos ../custom-gx/2.07-borscht/*.hsp
# unix2dos ../custom-gx/2.07-borscht/*.hsp
unix2dos ../custom-gx/2.08-borscht/*.hsp

pushd ../custom-gx/diff
# diff -U5 --recursive ../1.90-borscht/ ../2.05-borscht/ | unix2dos > 1.90-to-2.05.diff
# diff -U5 --recursive ../2.05-borscht/ ../2.06-borscht/ | unix2dos > 2.05-to-2.06.diff
# diff -U5 --recursive ../2.06-borscht/ ../2.06fix-borscht/ | unix2dos > 2.06-to-2.06fix.diff
# diff -U5 --recursive ../2.06fix-borscht/ ../2.07-borscht/ | unix2dos > 2.06fix-to-2.07.diff
diff -U5 --recursive ../2.07-borscht/ ../2.08-borscht/ | unix2dos > 2.07-to-2.08.diff
git status
# git add ../1.90-borscht ../2.05-borscht ../2.06-borscht ../2.06fix-borscht
# git add ../2.07-borscht
git add ../2.08-borscht
# git add 1.90-to-2.05.diff 2.05-to-2.06.diff 2.06-to-2.06fix.diff
# git add 2.06fix-to-2.07.diff
git add 2.07-to-2.08.diff
popd
