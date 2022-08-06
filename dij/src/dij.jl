#! julia

# build relational graph
relation_graph = Dict{String, Dict{String, UInt32}}()

# add node data
relation_graph["book"] = Dict{String, UInt32}()
relation_graph["book"]["lp"] = 5
relation_graph["book"]["poster"] = 0

relation_graph["lp"] = Dict{String, UInt32}()
relation_graph["lp"]["bass"] = 15
relation_graph["lp"]["drums"] = 20

relation_graph["poster"] = Dict{String, UInt32}()
relation_graph["poster"]["bass"] = 30
relation_graph["poster"]["drums"] = 35

relation_graph["bass"] = Dict{String, UInt32}()
relation_graph["bass"]["piano"] = 20

relation_graph["drums"] = Dict{String, UInt32}()
relation_graph["drums"]["piano"] = 10

# print current graph state
function print_relation_graph()
    for key in keys(relation_graph)
        println(key, " = ", relation_graph[key])
    end
end

print_relation_graph()

# execute Dijkstra's algorithm on graph
#   to find shortest weighted path from
#   book to drums
#
# we have a weighted graph that represents ("holds") the transaction
# (node - position, connections, distance) information for a connection
# diagram that we will use Dijkstra's algorithm on to find the shortest
# (lowest cost) path from start (book) to end (piano) node. the
# transactions are the edges where the costs are the weights.

# define start and end nodes
start_node = "book"
end_node = "piano"

all_nodes = Set(keys(relation_graph))
push!(all_nodes, end_node)

# build process state structure
processed = Set{String}([end_node]) # end node does not need to be processed

# build cost structure
cost = Dict{String, UInt32}()
for key in keys(relation_graph)
    cost[key] = typemax(UInt32) # initialize cost structure
end
cost[start_node] = 0
cost[end_node] = typemax(UInt32)

# build path structure
path_data = Dict{String, String}()

function process_node(pnode)
    # calculate the cost of reaching adjacent nodes (anode) by adding the cost
    # of getting to this node (pnode) to the cost of reaching them (edge weight)
    # and update if better
    for adjacent_node in keys(relation_graph[pnode])
        if (relation_graph[pnode][adjacent_node] + cost[pnode]) < cost[adjacent_node]
            # update cost and record path
            cost[adjacent_node] = relation_graph[pnode][adjacent_node] + cost[pnode]
            path_data[adjacent_node] = pnode
        end
    end

    # mark processed
    push!(processed, pnode)
end

function get_lowest_unprocessed()
    lowest_unprocessed_node = ""
    lowest_value = typemax(UInt32)
    for node in get_unprocessed()
        if cost[node] < lowest_value
            lowest_unprocessed_node = node
            lowest_value = cost[node]
        end
    end
    lowest_unprocessed_node
end

function get_unprocessed()
    setdiff(all_nodes, processed)
end

# process lowest cost unprocessed node
while !isempty(get_lowest_unprocessed())
    process_node(get_lowest_unprocessed())
end

function simplify_path(data)
    path = Dict{String, String}()

    current_node = end_node
    while current_node != start_node
        path[current_node] = data[current_node]
        current_node = data[current_node]
    end

    path
end

fastest_path = simplify_path(path_data)

println("\nfastest path:  ", fastest_path)
