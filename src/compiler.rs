use crate::scanner::Scanner;

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);

    while let Some(token) = scanner.next() {
        println!("{token}");
    }
}
