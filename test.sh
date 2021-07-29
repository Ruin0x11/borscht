#!/usr/bin/env bash

cargo run --release -- analyze "../elonaplus1.90/start.ax"

dos2unix '../elonaplus1.90/start.hsp'

diff '../elonaplus1.90/start.hsp' '../ElonaCustom 1.90.4/source.hsp' > 1.diff

# diff 1.diff 2.diff

wc 2.diff
wc 1.diff

cp 1.diff 2.diff
