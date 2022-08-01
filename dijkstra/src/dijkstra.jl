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
function print_graph()
    for key in keys(relation_graph)
        println(key, " = ", relation_graph[key])
    end
end

print_graph()

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

# build process state and cost structures
processed = Set{String}([end_node]) # end node does not need to be processed
cost = Dict{String, UInt32}()
for key in keys(relation_graph)
    cost[key] = typemax(UInt32) # initialize costs
end
cost[start_node] = 0
cost[end_node] = typemax(UInt32)

function process_node(node)
    # add cost of getting to this node to the costs of reaching adjacent nodes
    for adjacent_node in keys(relation_graph[node])
        # store edge weight
        cost[adjacent_node] = relation_graph[node][adjacent_node] + cost[node]
    end

    # mark processed
    push!(processed, node)
end

function find_lowest_unprocessed()
    lowest_unp = ""
    lowest_value = typemax(UInt32)
    for unp in get_unprocessed()
        if cost[unp] < lowest_value
            lowest_unp = unp
            lowest_value = cost[unp]
        end
    end
    lowest_unp
end

function get_unprocessed()
    setdiff(all_nodes, processed)
end

# process lowest cost unprocessed node
while !isempty(find_lowest_unprocessed())
    process_node(find_lowest_unprocessed())
end

println("processed = ", processed)
println("cost = ", cost)

println("all nodes:  ", all_nodes)
println("unprocessed nodes:  ", get_unprocessed())
