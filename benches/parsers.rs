use divan::{Bencher, bench};
use parserc::{Parser, ParserExt, ensure_keyword, ensure_next, take_until};

fn main() {
    divan::main();
}

#[bench]
fn bench_opt() {
    ensure_keyword("hello")
        .ok()
        .parse(b"hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_take_until() {
    take_until("<!--")
        .parse("world !!!!!!!  dfdfdfdfdfd <!--")
        .unwrap();
}

#[bench]
fn bench_take_until_bytes() {
    take_until("<!--")
        .parse(b"world !!!!!!!  dfdfdfdfdfd <!--".as_slice())
        .unwrap();
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

#[bench]
fn bench_or_true(bencher: Bencher) {
    let mut mock = ensure_keyword("true")
        .map(|_| true)
        .or(ensure_keyword("false").map(|_| false));

    bencher.bench_local(move || mock.parse("true").unwrap());
}

#[bench]
fn bench_or_true_bytes(bencher: Bencher) {
    let mut mock = ensure_keyword("true")
        .map(|_| true)
        .or(ensure_keyword("false").map(|_| false));

    let input = b"true".as_slice();

    bencher.bench_local(move || mock.parse(input).unwrap());
}

#[bench]
fn bench_or_false(bencher: Bencher) {
    let mut mock = ensure_keyword("true")
        .map(|_| true)
        .or(ensure_keyword("false").map(|_| false));

    bencher.bench_local(move || mock.parse("false").unwrap());
}

#[bench]
fn bench_or_false_bytes(bencher: Bencher) {
    let mut mock = ensure_keyword("true")
        .map(|_| true)
        .or(ensure_keyword("false").map(|_| false));

    bencher.bench_local(move || mock.parse("false".as_bytes()).unwrap());
}
