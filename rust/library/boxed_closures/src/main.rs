struct B {
    f: Box<dyn Fn(u8) -> u8>,
}

impl B {
    fn new<F>(f: F) -> Self
    where 
        F: Fn(u8) -> u8 + 'static,
    {
        B { f: Box::new(f) }
    }
}


fn main() {
    static n: u8 = 3;

    let mut my_closure = B::new(|x| n + x + 1);

    if true {
        my_closure = B::new(|x| n + x + 2);
    }
    else {
        my_closure = B::new(|x| n + x + 5);
    }

    println!("{}", (my_closure.f)(2));
}

