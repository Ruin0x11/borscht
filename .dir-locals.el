((nil (projectile-project-compilation-cmd . "RUST_BACKTRACE=1 cargo run --release -- analyze ../elonaplus1.90/start.ax && dos2unix ../elonaplus1.90/start.hsp; wc diff.diff; diff ../elonaplus1.90/start.hsp '../ElonaCustom 1.90.4/source.hsp' > diff.diff; wc diff.diff")
