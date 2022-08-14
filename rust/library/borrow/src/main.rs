fn main() {
    let mut a: String = "desafortunadamente".to_string();

    {
        let b: &mut String = &mut a;
        *b = "yourstringwasstolen".to_string();
    }

    println!("a: {}", a);
}