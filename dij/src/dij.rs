use std::collections::HashMap;
use std::collections::HashSet;

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


    /*

      execute Dijkstra's algorithm on graph
        to find shortest weighted path from
        book to drums
      
      we have a weighted graph that represents ("holds") the transaction
      (node - position, connections, distance) information for a connection
      diagram that we will use Dijkstra's algorithm on to find the shortest
      (lowest cost) path from start (book) to end (piano) node. the
      transactions are the edges where the costs are the weights.

    */

    let start_node: &str = "book";
    let end_node: &str = "piano";

    //    all nodes
    let mut all_nodes: HashSet<&str> = HashSet::new();
    all_nodes.insert(end_node);
    for o in relation_graph.keys() {
        all_nodes.insert(o);
    }

    println!("all nodes: {:#?}", all_nodes);

    //    processed status
    let mut processed: HashSet<&str> = HashSet::new();
    processed.insert(end_node);

    println!("processed nodes: {:#?}", processed);
}

#[test]
fn test_test() {
    // TODO
}
