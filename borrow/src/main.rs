struct Object<'a> {
    name: &'a str,
    value: u32,
}

fn modify_mutable_reference<'a>(mut_ref: &'a mut Object) {
    mut_ref.value = 212;
}

fn print_obj(obj: &Object) {
    println!(
        "Object {{ name: {}, value: {} }}",
        obj.name,
        obj.value,
    )
}

fn main() {
    let mut o = Object { name: "George", value: 5 };

    print_obj(&o);
    modify_mutable_reference(&mut o);
    print_obj(&o);
}
