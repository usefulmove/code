fn main() {
    let clos = |x: u32| x + 32;

    fn fun(x: u32) -> u32 { x + 32 }

    println!(
        "  ( {} : {} : {} )",
        clos(1),
        fun(1),
        (|x| x + 32)(1),
    );
}
