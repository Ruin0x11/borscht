extern crate anyhow;
extern crate clap;
extern crate log;
extern crate env_logger;
extern crate hexyl;

extern crate exe2ax;
extern crate erystia;

use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path};
use std::time::Instant;
use anyhow::Result;
use clap::{Arg, App, SubCommand, ArgMatches, crate_version, crate_authors};
use exe2ax::as_::DecodeOptions;
use exe2ax::as_::ax3::Hsp3As;

fn get_app<'a, 'b>() -> App<'a, 'b> {
    App::new("borscht")
        .version(crate_version!())
        .author(crate_authors!())
        .about("Cross-platform decompiler for Hot Soup Processor binaries")
        .subcommand(SubCommand::with_name("unpack")
                    .about("Unpack an .exe compiled with HSP")
                    .arg(Arg::with_name("output-dir")
                         .short("o")
                         .long("output-dir")
                         .help("output directory")
                         .takes_value(true)
                         .value_name("DIR"))
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help(".exe file")
                         .index(1))
        )
        .subcommand(SubCommand::with_name("decode")
                    .about("Decode an HSP .ax")
                    .arg(Arg::with_name("output-dir")
                         .short("o")
                         .long("output-dir")
                         .help("output directory")
                         .takes_value(true)
                         .value_name("DIR"))
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help(".ax file")
                         .index(1))
        )
        .subcommand(SubCommand::with_name("analyze")
                    .about("Analyze an HSP .ax")
                    .arg(Arg::with_name("output-dir")
                         .short("o")
                         .long("output-dir")
                         .help("output directory")
                         .takes_value(true)
                         .value_name("DIR"))
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help(".ax file")
                         .index(1))
        )
}

fn print_bytes<R: AsRef<[u8]>>(input: R) {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    let mut printer = hexyl::Printer::new(&mut handle, true, hexyl::BorderStyle::Unicode, false);
    printer.print_all(&input.as_ref()[..]).unwrap()
}

fn cmd_unpack(sub_matches: &ArgMatches) -> Result<()> {
    let input_path = Path::new(sub_matches.value_of("FILE").unwrap());
    let output_dir = match sub_matches.value_of("output-dir") {
        Some(dir) => Path::new(dir),
        None => input_path.parent().unwrap()
    };

    fs::create_dir_all(output_dir)?;
    let mut input_file = File::open(input_path)?;

    let dpm = exe2ax::dpm::exe_to_dpm(&mut input_file)?;

    for dpm_file in dpm.iter_files() {
        let output_file = output_dir.join(input_path.with_file_name(&dpm_file.name).file_name().unwrap());
        let dpm_file_ref = dpm.get_file_data(&dpm_file.name).unwrap();
        let mut file = File::create(&output_file)?;

        if dpm_file.name.ends_with(".ax") {
            let ax = exe2ax::ax::dpm_to_ax(&dpm_file_ref)?;
            file.write_all(ax.as_ref())?;
        } else {
            file.write_all(dpm_file_ref.data)?;
        }

        println!("Wrote {:?}.", output_file);
    }

    Ok(())
}

fn cmd_decode(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());
    let output_dir = match sub_matches.value_of("output-dir") {
        Some(dir) => Path::new(dir),
        None => input_file.parent().unwrap()
    };

    let output_file = output_dir.join(input_file.with_extension("hsp").file_name().unwrap());
    let opts = DecodeOptions {};

    let buffer = fs::read(input_file)?;
    let ax = exe2ax::ax::bytes_to_ax(buffer)?;

    let now = Instant::now();
    let mut as_ = exe2ax::as_::ax_to_as(ax, &opts)?;

    println!("Decompiled bytecode in {:.2?}", now.elapsed());

    let mut file = File::create(&output_file)?;
    as_.write_code(&mut file)?;

    // println!("Wrote {:?}.", output_file);

    Ok(())
}

fn cmd_analyze(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());
    let output_dir = match sub_matches.value_of("output-dir") {
        Some(dir) => Path::new(dir),
        None => input_file.parent().unwrap()
    };

    let output_file = output_dir.join(input_file.with_extension("hsp").file_name().unwrap());

    let opts = DecodeOptions {};

    let buffer = fs::read(input_file)?;
    let ax = exe2ax::ax::bytes_to_ax(buffer)?;

    let now = Instant::now();
    let mut as_ = exe2ax::as_::ax_to_as(ax, &opts)?;

    println!("Decompiled bytecode in {:.2?}.", now.elapsed());

    let now = Instant::now();
    let result = erystia::analyze(&as_)?;
    as_.program = result.node;

    println!("Analyzed in {:.2?}.", now.elapsed());

    let now = Instant::now();
    let mut file = File::create(&output_file)?;
    as_.write_code(&mut file)?;

    println!("Wrote {:?} in {:.2?}.", output_file, now.elapsed());

    Ok(())
}

fn main() -> Result<()> {
    env_logger::init();

    let matches = get_app().get_matches();

    match matches.subcommand() {
        ("unpack", Some(sub_matches)) => cmd_unpack(&sub_matches)?,
        ("decode", Some(sub_matches)) => cmd_decode(&sub_matches)?,
        ("analyze", Some(sub_matches)) => cmd_analyze(&sub_matches)?,
        _ => get_app().print_long_help()?
    }

    Ok(())
}
