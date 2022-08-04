use std::collections::HashMap;

fn main() {
    // build relation graph
    let mut relation_graph: HashMap<&str, HashMap<&str, u32>> = HashMap::new();

    relation_graph.insert("book", HashMap::new());
    relation_graph.insert("lp", HashMap::new());
    relation_graph.insert("poster", HashMap::new());
    relation_graph.insert("bass", HashMap::new());
    relation_graph.insert("drums", HashMap::new());
    for (&node, obj) in relation_graph.iter_mut() {
        match node {
            "book" => {
                obj.insert("lp", 5);
                obj.insert("poster", 0);
            },
            "lp" => {
                obj.insert("bass", 15);
                obj.insert("drums", 20);
            },
            "poster" => {
                obj.insert("bass", 30);
                obj.insert("drums", 35);
            },
            "bass" => {
                obj.insert("piano", 20);
            },
            "drums" => {
                obj.insert("piano", 10);
            },
            _ => {
            },
        }
    }
    
    println!("{:#?}", relation_graph);
}

#[test]
fn test_test() {
    // TODO
}
