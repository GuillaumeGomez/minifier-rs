use std::convert::From;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

use clap::builder::PossibleValue;
use clap::{command, value_parser, Arg, ArgAction, ValueEnum};

extern crate minifier;
use minifier::{css, js, json};

pub struct Cli;

impl Cli {
    pub fn init() {
        let matches = command!()
            .arg(
                Arg::new("FileType")
                    .short('t')
                    .long("type")
                    .help(
                        "File Extention without dot. This option is optional.
If you don't provide this option, all input files
type will detect via extension of input file.
",
                    )
                    .required(false)
                    .value_parser(value_parser!(FileType)),
            )
            .arg(
                Arg::new("output")
                    .short('o')
                    .long("out")
                    .help("Output file or directory (Default is parent dir of input files)")
                    .required(false)
                    .value_parser(value_parser!(PathBuf)),
            )
            .arg(
                Arg::new("FILE")
                    .help("Input Files...")
                    .num_args(1..)
                    .value_parser(value_parser!(PathBuf))
                    .action(ArgAction::Append),
            )
            .get_matches();
        let args: Vec<&PathBuf> = matches
            .get_many::<PathBuf>("FILE")
            .unwrap_or_default()
            .collect::<Vec<_>>();
        let ext: Option<&FileType> = matches.get_one::<FileType>("FileType");
        let out: Option<&PathBuf> = matches.get_one::<PathBuf>("output");
        for path in args.into_iter() {
            write_out_file(path, out, ext);
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum FileType {
    // Html,
    Css,
    Js,
    Json,
    Unknown,
}

impl FileType {
    fn as_str(&self) -> &str {
        match self {
            Self::Css => "css",
            Self::Js => "js",
            Self::Json => "json",
            Self::Unknown => "unknown",
        }
    }
}

impl ValueEnum for FileType {
    fn value_variants<'a>() -> &'a [Self] {
        &[FileType::Css, FileType::Js, FileType::Json]
    }
    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(match *self {
            FileType::Css => PossibleValue::new("css")
                .help("All the files will be consider as CSS, regardless of their extension."),
            FileType::Js => PossibleValue::new("js").help(
                "All the files will be consider as JavaScript, regardless of their extension.",
            ),
            FileType::Json => PossibleValue::new("json")
                .help("All the files will be consider as JSON, regardless of their extension."),
            FileType::Unknown => panic!("unknow file"),
        })
    }
}
impl std::str::FromStr for FileType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for variant in Self::value_variants() {
            if variant.to_possible_value().unwrap().matches(s, false) {
                return Ok(*variant);
            };
        }
        Err(format!("Invalid variant: {s}"))
    }
}

impl From<&PathBuf> for FileType {
    fn from(value: &PathBuf) -> Self {
        let ext = value.extension();
        if ext.is_none() {
            return Self::Unknown;
        };
        match ext.unwrap().to_ascii_lowercase().to_str().unwrap() {
            "css" => Self::Css,
            "js" => Self::Js,
            "json" => Self::Json,
            _ => Self::Unknown,
        }
    }
}

pub fn get_all_data<T: AsRef<Path>>(file_path: T) -> io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut data = String::new();
    file.read_to_string(&mut data).unwrap();
    Ok(data)
}

fn write_out_file(file_path: &PathBuf, out_path: Option<&PathBuf>, ext: Option<&FileType>) {
    let file_ext = if let Some(v) = ext {
        v
    } else {
        &FileType::from(file_path)
    };
    if file_ext == &FileType::Unknown {
        eprintln!("{:?}: unknow file extension...", file_path);
        return;
    };
    match get_all_data(file_path) {
        Ok(content) => {
            let out = if out_path.is_some() {
                let mut op = out_path.unwrap().clone();
                if op.is_dir() {
                    op.push(file_path);
                    op.set_extension(format!("min.{}", file_ext.as_str()));
                };
                if op.parent().is_some() && !op.parent().unwrap().is_dir() {
                    std::fs::create_dir_all(op.parent().unwrap()).unwrap();
                };
                op
            } else {
                let mut p = file_path.clone();
                p.set_extension(format!("min.{}", file_ext.as_str()));
                p
            };
            if let Ok(mut file) = OpenOptions::new()
                .truncate(true)
                .write(true)
                .create(true)
                .open(&out)
            {
                let func = |s: &str| -> String {
                    match file_ext {
                        FileType::Css => {
                            css::minify(s).expect("css minification failed").to_string()
                        }
                        FileType::Js => js::minify(s).to_string(),
                        FileType::Json => json::minify(s).to_string(),
                        FileType::Unknown => panic!("{:?}: unknow file extension...", file_path),
                    }
                };
                if let Err(e) = write!(file, "{}", func(&content)) {
                    eprintln!("Impossible to write into {:?}: {}", out, e);
                } else {
                    println!("{:?}: done -> generated into {:?}", file_path, out);
                }
            } else {
                eprintln!("Impossible to create new file: {:?}", out);
            }
        }
        Err(e) => eprintln!("{:?}: {}", file_path, e),
    }
}
