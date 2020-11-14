/// 適当なエラー型
#[derive(Debug)]
enum Error {
    Failed,
}

/// impl FindToken<char> for &strの簡易実装
fn find_char<'a>(arr: &'a str, c: char) -> bool {
    arr.chars().any(|i| i == c)
}

/// impl InputTokenAtPosition for &str の簡易実装
fn split_at_position1_complete<'a, P>(s: &'a str, predicate: P) -> Result<(&'a str, &'a str), Error>
where
    P: Fn(char) -> bool,
{
    match s.find(predicate) {
        Some(0) => Err(Error::Failed),
        Some(i) => Ok((&s[i..], &s[..i])),
        None => {
            if s.is_empty() {
                Err(Error::Failed)
            } else {
                Ok((&s[s.len()..], &s[..s.len()]))
            }
        }
    }
}

fn specialized_is_a<'a>(arr: &'a str) -> impl Fn(&'a str) -> Result<(&'a str, &'a str), Error> {
    move |i: &str| split_at_position1_complete(i, |c| !find_char(arr, c))
}

fn main() {
    let digit = specialized_is_a("1234567890");
    eprintln!("digit(\"1234567890\")(\"123abc\") => {:?}", digit("123abc"));

    eprintln!(
        "digit(\"1234567890\")(\"Drumato\") => {:?}",
        digit("Drumato")
    );

    eprintln!("digit(\"1234567890\")(\"\") => {:?}", digit(""));
}
