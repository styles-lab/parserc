use divan::bench;
use parserc::{Kind, Parser, ParserExt, keyword, next, take_until};

fn main() {
    divan::main();
}

#[bench]
fn bench_opt() {
    keyword::<&str, &[u8], Kind>("hello")
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
    keyword::<&str, &str, Kind>("hello")
        .parse("hello world")
        .unwrap();
}

#[bench]
fn bench_ensure_keyword_bytes() {
    keyword::<&str, &[u8], Kind>("hello")
        .parse(b"hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_ensure_keyword1() {
    keyword::<&str, &str, Kind>("hello")
        .parse("")
        .expect_err("short input");
}

#[bench]
fn bench_ensure_char() {
    next::<char, &str, Kind>('你')
        .parse("你 hello world")
        .unwrap();
}

#[bench]
fn bench_ensure_char1() {
    next::<char, &str, Kind>('你')
        .parse("我 hello world")
        .expect_err("no match");
}

#[bench]
fn bench_ensure_byte() {
    next::<u8, &[u8], Kind>(b'<')
        .parse(b"< hello world".as_slice())
        .unwrap();
}

#[bench]
fn bench_ensure_byte1() {
    next::<u8, &[u8], Kind>(b'<')
        .parse(b"> hello world".as_slice())
        .expect_err("mismatch.");
}

#[divan::bench_group]
mod bench_or {
    use divan::Bencher;
    use parserc::{Kind, Parser, ParserExt, keyword};

    #[divan::bench]
    fn bench_or_true(bencher: Bencher) {
        let mut mock = keyword::<&str, &str, Kind>("true")
            .map(|_| true)
            .or(keyword("false").map(|_| false));

        bencher.bench_local(move || mock.parse("true").unwrap());
    }

    #[divan::bench]
    fn bench_or_true_bytes(bencher: Bencher) {
        let mut mock = keyword::<&str, &[u8], Kind>("true")
            .map(|_| true)
            .or(keyword("false").map(|_| false));

        let input = b"true".as_slice();

        bencher.bench_local(move || mock.parse(input).unwrap());
    }

    #[divan::bench]
    fn bench_or_false(bencher: Bencher) {
        let mut mock = keyword::<&str, &str, Kind>("true")
            .map(|_| true)
            .or(keyword("false").map(|_| false));

        bencher.bench_local(move || mock.parse("false").unwrap());
    }

    #[divan::bench]
    fn bench_or_false_bytes(bencher: Bencher) {
        let mut mock = keyword::<&str, &[u8], Kind>("true")
            .map(|_| true)
            .or(keyword("false").map(|_| false));

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

#[divan::bench_group]
mod bench_take_while {
    use parserc::{Kind, Parser, Result, take_till, take_while};

    fn digit(input: &str) -> Result<&str, &str, Kind> {
        take_while(|c: char| c.is_ascii_digit()).parse(input)
    }

    fn digit_bytes(input: &[u8]) -> Result<&[u8], &[u8], Kind> {
        take_while(|c: u8| c.is_ascii_digit()).parse(input)
    }

    #[divan::bench]
    fn bench_digit() {
        digit("hello123").unwrap();
    }

    #[divan::bench]
    fn bench_digit2() {
        digit("12345566900goodluck").unwrap();
    }

    #[divan::bench]
    fn bench_digit_bytes() {
        digit_bytes(b"hello123").unwrap();
    }

    #[divan::bench]
    fn bench_digit2_bytes() {
        digit_bytes(b"12345566900goodluck").unwrap();
    }

    fn non_digit_bytes(input: &[u8]) -> Result<&[u8], &[u8], Kind> {
        take_till(|c: u8| c.is_ascii_digit()).parse(input)
    }

    #[divan::bench]
    fn bench_non_digit_bytes() {
        non_digit_bytes(b"goodluck12345566900").unwrap();
    }
}
