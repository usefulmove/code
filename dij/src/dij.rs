use std::collections::HashMap;

fn main() {
    // create graph
    let mut graph: HashMap<&str, HashMap<&str, u32>> = HashMap::new();

    // add node data
    graph.insert("book", HashMap::new());
    graph["book"].insert("lp", 5);
    graph["book"].insert("poster", 0);
    
    println!("{:#?}", graph);
}

#[test]
fn test_test() {
    assert!(graph.contains_key("book"));
}
