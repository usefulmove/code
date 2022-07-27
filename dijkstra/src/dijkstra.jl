#! julia

# create directional graph
graph = Dict{String, Dict{String, UInt32}}()

# add node data
graph["book"] = Dict{String, UInt32}()
graph["book"]["lp"] = 5
graph["book"]["poster"] = 0

graph["lp"] = Dict{String, UInt32}()
graph["lp"]["bass"] = 15
graph["lp"]["drums"] = 20

graph["poster"] = Dict{String, UInt32}()
graph["poster"]["bass"] = 30
graph["poster"]["drums"] = 35

graph["bass"] = Dict{String, UInt32}()
graph["bass"]["piano"] = 20

graph["drums"] = Dict{String, UInt32}()
graph["drums"]["piano"] = 10

println(graph)

for node in graph
    for nd in node
      println(graph[node][nd])
    end
end


# execute Dijkstra's algorithm on graph
#   to find shortest weighted path from
#   book to drums
#
# we have a weighted graph that represents ("holds") the transaction
# (node - position, connections, distance) information for a connection
# diagram that we will use Dijkstra's algorithm on to find the shortest
# (lowest cost) path from start (book) to end (piano) node. the
# transactions are the edges where the costs are the weights.

# from start node
println(graph["book"]["lp"])

# find lowest cost node
lowest_node = ""
#for node in graph.keys()
#    if lowest_node == ""
#        lowest_node = node
#    else
#        if graph[node]["lp"] < graph[lowest_node]["lp"]
#            lowest_node = node
#    graph[node]["distance"] = 0
#    graph[node]["previous"] = nil
#end