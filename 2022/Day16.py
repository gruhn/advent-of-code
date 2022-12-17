from pyscipopt import Model, quicksum
import networkx as nx
import re

def solve(input, minutes, actors):
  graph = nx.DiGraph()
  start_node = "AA"

  for (node, rate, neighbors) in input:
    graph.add_node(node, rate=rate)

    # Add self-edges, which model the action of opening a valve at that node.
    graph.add_edge(node, node)

    for neigbor in neighbors:
      graph.add_edge(node, neigbor)

  model = Model()
  model.setMaximize()

  time_steps = range(1, minutes+1)

  x = {}
  for t in time_steps:
    for (source,target) in graph.edges:
      if source == target:
        rate = graph.nodes[source]["rate"]
        obj = rate * (minutes - t) 
        x[t,source,target] = model.addVar(vtype="BINARY", obj=obj)
        if obj == 0:
          # cutting plane: ignore borken valves
          model.addCons(x[t,source,target] == 0)
      else:
        x[t,source,target] = model.addVar(vtype="BINARY", obj=0)

  # All paths start at `start_node`
  model.addCons(actors == quicksum( x[1,start_node,target] for target in graph.successors(start_node) ))

  # Self-edges can be used at most once, i.e. a valve can only be opened once.
  for node in graph.nodes:
    used_edge_count = quicksum( x[t,node,node] for t in time_steps )
    model.addCons(used_edge_count <= 1)

  for t in time_steps:
    # Pick at most one edge for each time step. This implies that we spend exactly 30min.
    used_edge_count = quicksum( x[t,source,target] for (source,target) in graph.edges )
    model.addCons(used_edge_count <= actors)

    # Flow-conservation constraint to ensure the choosen edges form paths
    if t > 1:
      for node in graph.nodes:
          outgoing_edges = quicksum( x[t,node,target] for target in graph.successors(node) )
          ingoing_edges = quicksum( x[t-1,source,node] for source in graph.predecessors(node) )

          model.addCons(outgoing_edges - ingoing_edges == 0)

          # curring planes: (technically redundant)
          # model.addCons(outgoing_edges <= 1)
          # model.addCons(ingoing_edges <= 1)

  model.optimize()
  return model.getObjVal()

input = []
for line in open("input/16.txt", "r").read().splitlines():
  node, rate, *neighbors = re.findall(r'([A-Z][A-Z]|[0-9]+)', line)
  input.append((node, int(rate), neighbors))

print("Part 1 :", solve(input, 30, 1))
print("Part 2 :", solve(input, 26, 2))