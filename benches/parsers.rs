use divan::bench;
use parserc::{Kind, Parser, ParserExt, ensure_keyword, ensure_next, take_until};

fn main() {
    divan::main();
}

#[bench]
fn bench_opt() {
    ensure_keyword::<&str, &[u8], Kind>("hello")
        .ok()
        .parse(b"hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_take_until() {
    take_until::<&str, &str, Kind>("<!--")
        .parse("world !!!!!!!  dfdfdfdfdfd <!--")
        .unwrap();
}

#[bench]
fn bench_take_until_bytes() {
    take_until::<&str, &[u8], Kind>("<!--")
        .parse(b"world !!!!!!!  dfdfdfdfdfd <!--".as_slice())
        .unwrap();
}

#[bench]
fn bench_ensure_keyword() {
    ensure_keyword::<&str, &str, Kind>("hello")
        .parse("hello world")
        .unwrap();
}

#[bench]
fn bench_ensure_keyword_bytes() {
    ensure_keyword::<&str, &[u8], Kind>("hello")
        .parse(b"hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_ensure_keyword1() {
    ensure_keyword::<&str, &str, Kind>("hello")
        .parse("")
        .expect_err("short input");
}

#[bench]
fn bench_ensure_char() {
    ensure_next::<char, &str, Kind>('你')
        .parse("你 hello world")
        .unwrap();
}

#[bench]
fn bench_ensure_char1() {
    ensure_next::<char, &str, Kind>('你')
        .parse("我 hello world")
        .expect_err("no match");
}

#[bench]
fn bench_ensure_byte() {
    ensure_next::<u8, &[u8], Kind>(b'<')
        .parse(b"< hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_ensure_byte1() {
    ensure_next::<u8, &[u8], Kind>(b'<')
        .parse(b"> hello world".as_slice())
        .expect_err("mismatch.");
}

#[divan::bench_group]
mod bench_or {
    use divan::Bencher;
    use parserc::{Kind, Parser, ParserExt, ensure_keyword};

    #[divan::bench]
    fn bench_or_true(bencher: Bencher) {
        let mut mock = ensure_keyword::<&str, &str, Kind>("true")
            .map(|_| true)
            .or(ensure_keyword("false").map(|_| false));

        bencher.bench_local(move || mock.parse("true").unwrap());
    }

    #[divan::bench]
    fn bench_or_true_bytes(bencher: Bencher) {
        let mut mock = ensure_keyword::<&str, &[u8], Kind>("true")
            .map(|_| true)
            .or(ensure_keyword("false").map(|_| false));

        let input = b"true".as_slice();

        bencher.bench_local(move || mock.parse(input).unwrap());
    }

    #[divan::bench]
    fn bench_or_false(bencher: Bencher) {
        let mut mock = ensure_keyword::<&str, &str, Kind>("true")
            .map(|_| true)
            .or(ensure_keyword("false").map(|_| false));

        bencher.bench_local(move || mock.parse("false").unwrap());
    }

    #[divan::bench]
    fn bench_or_false_bytes(bencher: Bencher) {
        let mut mock = ensure_keyword::<&str, &[u8], Kind>("true")
            .map(|_| true)
            .or(ensure_keyword("false").map(|_| false));

        bencher.bench_local(move || mock.parse("false".as_bytes()).unwrap());
    }
}

#[divan::bench_group]

mod parse {
    use parserc::Parser;

    #[divan::bench]
    fn bench_parse() {
        use parserc::Parse;

        bool::parse("true").unwrap();
    }

    #[divan::bench]
    fn bench_into_parse() {
        use parserc::Parse;
        bool::into_parser().parse("true").unwrap();
    }
}
