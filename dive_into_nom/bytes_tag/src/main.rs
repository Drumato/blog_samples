#[allow(dead_code)]
fn elf_magic_number(i: &[u8]) -> nom::IResult<&[u8], &[u8]> {
    nom::bytes::complete::tag(&[0x7f, 0x45, 0x4c, 0x46])(i)
}

#[allow(dead_code)]
fn drumato(i: &str) -> nom::IResult<&str, &str> {
    nom::bytes::complete::tag("drumato")(i)
}

fn main() {}

#[test]
fn drumato_test() {
    assert_eq!(Ok((";", "drumato")), drumato("drumato;"));
    assert!(drumato("not_drumato;").is_err());
}

#[test]
fn elf_magic_number_test() {
    assert_eq!(
        Ok((&[0x00][..], &[0x7f, 0x45, 0x4c, 0x46][..])),
        elf_magic_number(&[0x7f, 0x45, 0x4c, 0x46, 0x00])
    );

    assert!(elf_magic_number(&[0x01, 0x02, 0x03, 0x04, 0x05]).is_err());
}
