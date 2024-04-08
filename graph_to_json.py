import networkx as nx
import json

# Construct graph
G = nx.Graph()
G.add_edges_from([(1, 2), (2, 3), (3, 1)])

# Convert graph data to JSON format
graph_data = {"nodes": [], "links": []}

for node in G.nodes():
    graph_data["nodes"].append({"id": node})

for edge in G.edges():
    graph_data["links"].append({"source": edge[0], "target": edge[1]})

# Write JSON data to a file
with open("graph_data.json", "w") as json_file:
    json.dump(graph_data, json_file)
