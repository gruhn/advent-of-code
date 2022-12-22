# docker run --rm -it -v $(pwd):/home/user/scripts gruhn/scipsuite:8.0.3 python Day19.py

from pyscipopt import Model, quicksum
import re
import time
from math import prod

def solve(last_minute, costs):
  [ 
    ore_robot_ore_cost, 
    clay_robot_ore_cost, 
    obsidian_robot_ore_cost, 
    obsidian_robot_clay_cost, 
    geode_robot_ore_cost, 
    geode_robot_obsidian_cost
  ] = costs

  model = Model()

  time_steps = range(0, last_minute+1)

  make_ore_robot = {}
  make_clay_robot = {}
  make_obsidian_robot = {}
  make_geode_robot = {}

  ore_budget = {}
  clay_budget = {}
  obsidian_budget = {}

  for t in time_steps:
    make_ore_robot[t] = model.addVar(vtype="BINARY")
    make_clay_robot[t] = model.addVar(vtype="BINARY")
    make_obsidian_robot[t] = model.addVar(vtype="BINARY")
    make_geode_robot[t] = model.addVar(vtype="BINARY")

    ore_budget[t] = model.addVar(vtype="INTEGER", name=f"ore_budget_at_{t}")
    clay_budget[t] = model.addVar(vtype="INTEGER", name=f"clay_budget_at_{t}")
    obsidian_budget[t] = model.addVar(vtype="INTEGER", name=f"obsidian_budget_at_{t}")

    # make at most one robot in each time step
    model.addCons(make_ore_robot[t] + make_clay_robot[t] + make_obsidian_robot[t] + make_geode_robot[t] <= 1)

    # we have one ore collecting robot from the beginning; zero from the other types
    model.addCons(make_ore_robot[0] == 1)
    model.addCons(make_clay_robot[0] == 0)
    model.addCons(make_obsidian_robot[0] == 0)
    model.addCons(make_geode_robot[0] == 0)

    # no resources at the beginning
    model.addCons(ore_budget[0] == 0)
    model.addCons(clay_budget[0] == 0)
    model.addCons(obsidian_budget[0] == 0)

    if t > 0:
      ore_robot_count = quicksum(make_ore_robot[tt] for tt in time_steps if tt < t)
      spent_ores = make_ore_robot[t]*ore_robot_ore_cost + make_clay_robot[t]*clay_robot_ore_cost + make_obsidian_robot[t]*obsidian_robot_ore_cost + make_geode_robot[t]*geode_robot_ore_cost
      model.addCons(spent_ores <= ore_budget[t-1])
      model.addCons(ore_budget[t] == ore_robot_count + ore_budget[t-1] - spent_ores)

      clay_robot_count = quicksum(make_clay_robot[tt] for tt in time_steps if tt < t)
      spent_clay = make_obsidian_robot[t]*obsidian_robot_clay_cost
      model.addCons(spent_clay <= clay_budget[t-1])
      model.addCons(clay_budget[t] == clay_robot_count + clay_budget[t-1] - spent_clay)

      obsidian_robot_count = quicksum(make_obsidian_robot[tt] for tt in time_steps if tt < t)
      spent_obsidian = make_geode_robot[t]*geode_robot_obsidian_cost
      model.addCons(spent_obsidian <= obsidian_budget[t-1])
      model.addCons(obsidian_budget[t] == obsidian_robot_count + obsidian_budget[t-1] - spent_obsidian)

  total_geodes = quicksum( make_geode_robot[t] * (last_minute-t) for t in time_steps )
  model.setObjective(total_geodes, "maximize")
  model.optimize()

  # for t in time_steps:
  #   print("")
  #   print(f"== Minute {t} ==")
  #   ore_robot_count = round(sum(model.getVal(make_ore_robot[tt]) for tt in time_steps if tt < t))
  #   clay_robot_count = round(sum(model.getVal(make_clay_robot[tt]) for tt in time_steps if tt < t))
  #   obsidian_robot_count = round(sum(model.getVal(make_obsidian_robot[tt]) for tt in time_steps if tt < t))
  #   geode_robot_count = round(sum(model.getVal(make_geode_robot[tt]) for tt in time_steps if tt < t))
  #   print(f"{ore_robot_count} ore-collecting robots; you now have {round(model.getVal(ore_budget[t]))} ore")
  #   print(f"{clay_robot_count} clay-collecting robots; use now have {round(model.getVal(clay_budget[t]))} clay.")
  #   print(f"{obsidian_robot_count} obsidian-collecting robots; you now have {round(model.getVal(obsidian_budget[t]))} obsidian.")
  #   print(f"{geode_robot_count} geode-cracking robots; you now have {round(model.getVal(geode_budget[t]))} geodes.")

  return model

time_start = time.time()

input = []
for line in open("input/19.txt", "r").read().splitlines():
  blueprint_id, *costs = re.findall(r'([0-9]+)', line)
  input.append((int(blueprint_id), list(map(int, costs))))

part1 = []
for (blueprint_id, costs) in input:
  model = solve(24, costs)
  part1.append((blueprint_id, model))

part2 = []
for (blueprint_id, costs) in input[0:3]:
  model = solve(32, costs)
  part2.append((blueprint_id, model))

print("=== Part 1 ===")
for (blueprint_id, model) in part1:
  print(f"Blueprint {blueprint_id}: ObjVal = {round(model.getObjVal())}, Time = {model.getSolvingTime()}")
print("Answer: ", sum( id*round(m.getObjVal()) for (id,m) in part1 ))

print("=== Part 2 ===")
for (blueprint_id, model) in part2:
  print(f"Blueprint {blueprint_id}: ObjVal = {round(model.getObjVal())}, Time = {model.getSolvingTime()}")
print("Answer: ", prod( round(m.getObjVal()) for (_,m) in part2 ))

print("Total Time: ", time.time() - time_start)
