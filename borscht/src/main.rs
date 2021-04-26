extern crate anyhow;
extern crate clap;
extern crate log;
extern crate env_logger;
extern crate hexyl;

extern crate exe2ax;

use std::fs::{self, File};
use std::io::{self};
use std::path::{Path};
use anyhow::Result;
use clap::{Arg, App, SubCommand, ArgMatches, crate_version, crate_authors};

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
    let ax = exe2ax::ax::dpm_to_ax(&dpm)?;
    let as_ = exe2ax::as_::ax_to_as(&ax)?;

    // print_bytes(&dpm);

    Ok(())
}

fn cmd_decode(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());
    let output_dir = match sub_matches.value_of("output-dir") {
        Some(dir) => Path::new(dir),
        None => input_file.parent().unwrap()
    };

    // let scene = avg32::load(&input_file.to_str().unwrap())?;
    // let sexp = disasm::disassemble(&scene)?;

    let output_file = output_dir.join(input_file.with_extension("hsp").file_name().unwrap());
    // let mut file = File::create(&output_file)?;
    // file.write_all(&sexp.as_bytes())?;

    println!("Decompiled bytecode to {:?}.", output_file);
    Ok(())
}

fn main() -> Result<()> {
    env_logger::init();

    let matches = get_app().get_matches();

    match matches.subcommand() {
        ("unpack", Some(sub_matches)) => cmd_unpack(&sub_matches)?,
        ("decode", Some(sub_matches)) => cmd_decode(&sub_matches)?,
        _ => get_app().print_long_help()?
    }

    Ok(())
}
