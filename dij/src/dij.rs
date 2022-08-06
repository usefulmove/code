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
            _ => {},
        }
    }
    println!("relation graph: {:#?}", relation_graph);


    /*
      execute Dijkstra's algorithm on graph to find shortest weighted
      path from book to drums

      we have a weighted graph that represents ("holds") the transaction
      (node - position, connections, distance) information for a connection
      diagram that we will use Dijkstra's algorithm on to find the shortest
      (lowest cost) path from start (book) to end (piano) node. the
      transactions are the edges where the costs are the weights.

    */

    let start_node: &str = "book";
    let end_node: &str = "piano";

    // all nodes
    let mut all_nodes: HashSet<&str> = HashSet::new();
    all_nodes.insert(end_node);
    for o in relation_graph.keys() {
        all_nodes.insert(o);
    }
    println!("all nodes: {:#?}", all_nodes);

    // processed status
    let mut processed: HashSet<&str> = HashSet::new();
    processed.insert(end_node);
    println!("processed nodes: {:#?}", processed);

    // cost
    let mut cost: HashMap<&str, u32> = HashMap::new();
    for o in relation_graph.keys() {
        cost.insert(o, u32::MAX);
    }
    cost.insert(end_node, u32::MAX);
    cost.insert(start_node, 0);
    println!("costs: {:#?}", cost);

    // path structure
    let mut path_data: HashMap<&str, &str> = HashMap::new();

    process_node("book", &relation_graph, &mut cost);

    //println!(" costs: {:#?}", cost); // debug temp remove

}

fn process_node<'a>(pnode: &str, rel_graph: &'a HashMap<&str, HashMap<&str, u32>>, cost_map: &'a mut HashMap<&'a str, u32>) {
    /* calculate the cost of reaching adjacent nodes (anode) by adding the cost
     * of getting to this node (pnode) to the cost of reaching them (edge weight)
     * and update if better
     */
    for anode in rel_graph[pnode].keys() {
        let adj_cost: u32 = cost_map[pnode.clone()] + rel_graph[pnode][anode];
        println!("cost of reaching \"{}\" is {}", anode.clone(), adj_cost);
        if adj_cost < cost_map[anode.clone()] {
            println!("updating \"{}\" node cost", anode.clone());
            cost_map.insert(anode.clone(), adj_cost);
        }

    }
    //TODO
}

fn get_lowest_unprocessed<'a>(cost_map: &'a mut HashMap<&str, u32>) -> &'a str {
    "book"
}

#[test]
fn test_test() {
    // TODO
}
