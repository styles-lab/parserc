use divan::bench;
use parserc::{ControlFlow, Kind, Parser, ParserExt, Result, ensure_keyword, ensure_next};

fn main() {
    divan::main();
}

fn mock_recovable(_: &str) -> Result<&str, &str, Kind> {
    Err(ControlFlow::Recovable(Kind::None))
}

#[bench]
fn bench_opt() {
    mock_recovable.ok().parse("hello world").unwrap();
}

#[bench]
fn bench_ensure_keyword() {
    ensure_keyword("hello").parse("hello world").unwrap();
}

#[bench]
fn bench_ensure_keyword_bytes() {
    ensure_keyword("hello")
        .parse(b"hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_ensure_keyword1() {
    ensure_keyword("hello").parse("").expect_err("short input");
}

#[bench]
fn bench_ensure_char() {
    ensure_next('你').parse("你 hello world").unwrap();
}

#[bench]
fn bench_ensure_char1() {
    ensure_next('你')
        .parse("我 hello world")
        .expect_err("no match");
}

#[bench]
fn bench_ensure_byte() {
    ensure_next(b'<')
        .parse(b"< hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_ensure_byte1() {
    ensure_next(b'<')
        .parse(b"> hello world".as_slice())
        .expect_err("mismatch.");
}
