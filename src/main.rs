fn main() -> std::io::Result<()> {
    let args: Vec<_> = std::env::args().skip(1).collect();

    if args.len() != 2 {
        println!("Expected exactly 2 arguments (input and output paths)");
    }

    Ok(())
}
