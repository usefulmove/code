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
    relation_graph.insert("piano", HashMap::new());
    for (&node, obj) in relation_graph.iter_mut() {
        match node {
            "book" => {
                obj.insert("lp", 5);
                obj.insert("poster", 0);
            }
            "lp" => {
                obj.insert("bass", 15);
                obj.insert("drums", 20);
            }
            "poster" => {
                obj.insert("bass", 30);
                obj.insert("drums", 35);
            }
            "bass" => {
                obj.insert("piano", 20);
            }
            "drums" => {
                obj.insert("piano", 10);
            }
            _ => {}
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

    // processed status
    let mut process_map: HashSet<&str> = HashSet::new();
    process_map.insert(end_node);

    // cost
    let mut cost: HashMap<&str, u32> = HashMap::new();
    for o in relation_graph.keys() {
        cost.insert(o, u32::MAX);
    }
    cost.insert(start_node, 0);

    // path structure
    let mut path_data: HashMap<&str, &str> = HashMap::new();

    while !get_unprocessed_low(&relation_graph, &mut cost, &process_map).is_empty() {
        let next_node = get_unprocessed_low(&relation_graph, &mut cost, &process_map);
        process_node(
            next_node,
            &relation_graph,
            &mut cost,
            &mut process_map,
            &mut path_data,
        );
    }

    let shortest_path: HashMap<&str, &str> = short_path(&path_data, start_node, end_node);

    println!("cost map: {:#?}", cost);
    println!("shortest path: {:#?}", shortest_path);
}

fn process_node<'a, 'b, 'c>(
    pnode: &'c str,
    rel_graph: &'a HashMap<&str, HashMap<&'b str, u32>>,
    cost_map: &'a mut HashMap<&'b str, u32>,
    proc_map: &mut HashSet<&'c str>,
    path_map: &mut HashMap<&'b str, &'c str>,
) {
    // calculate the cost of reaching adjacent nodes (anode) by adding the cost
    // of getting to this node (pnode) to the cost of reaching them (edge weight)
    // and update if better
    for anode in rel_graph[pnode].keys() {
        let adj_cost: u32 = cost_map[pnode] + rel_graph[pnode][anode];
        if adj_cost < cost_map[anode] {
            // better cost - update path data and adjacent node cost
            path_map.insert(anode, pnode);
            cost_map.insert(anode, adj_cost);
        }
    }

    // mark processed
    proc_map.insert(pnode);
}

// find lowest unprocessed node on relation graph
fn get_unprocessed_low<'d, 'e>(
    rel_graph: &'d HashMap<&str, HashMap<&'d str, u32>>,
    cost_map: &'e mut HashMap<&'d str, u32>,
    proc_map: &'e HashSet<&'d str>,
) -> &'d str {
    let nodes: HashSet<&str> = rel_graph.keys().cloned().collect();

    let mut lowest_unprocessed_node: &str = "";
    let mut lowest: u32 = u32::MAX;
    for unprocessed_node in nodes.difference(proc_map) {
        if cost_map[unprocessed_node] < lowest {
            lowest_unprocessed_node = unprocessed_node;
            lowest = cost_map[unprocessed_node];
        }
    }

    lowest_unprocessed_node
}

fn short_path<'a>(
    data: &'a HashMap<&str, &str>,
    start: &'a str,
    end: &'a str,
) -> HashMap<&'a str, &'a str> {
    let mut short_path: HashMap<&str, &str> = HashMap::new();
    let mut current_node = end;
    while current_node != start {
        short_path.insert(current_node, data[current_node]);
        current_node = data[current_node];
    }

    short_path
}
