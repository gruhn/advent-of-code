# docker run --rm -it -v $(pwd):/home/user/scripts gruhn/scipsuite:8.0.3 python Day23.py

from pyscipopt import Model, quicksum
import re
import time

def manhattan(p1, p2):
  x1, x2, x3 = p1
  y1, y2, y3 = p2
  return abs(x1 - y1) + abs(x2 - y2) + abs(x3 - y3)

def solve(nanobots):
  model = Model()
  model.setMaximize()

  # The 3 coordinate components of a point that is in the range of the
  # maximal number of nanobots.
  x = {}
  x[1] = model.addVar(vtype="INTEGER")
  x[2] = model.addVar(vtype="INTEGER")
  x[3] = model.addVar(vtype="INTEGER")

  # One binary choice for each nanobot. Do we pick a point that is in  
  # range of that nanobot yes or no?
  y = {}
  for i in range(len(nanobots)):
    y[i] = model.addVar(vtype="BINARY", obj=1)

  # Choose sufficiently large big M to eliminate constraints where  y[i] == 0.
  M = max( manhattan((0,0,0), (x1,x2,x3)) for x1, x2, x3, _ in nanobots )

  for i in range(len(nanobots)):
    x1, x2, x3, r = nanobots[i]

    for sign1 in [1, -1]:
      for sign2 in [1, -1]:
        for sign3 in [1, -1]:
          model.addCons(
              sign1 * (x[1] - x1)
            + sign2 * (x[2] - x2)
            + sign3 * (x[3] - x3)
            <= r*y[i] + M*(1 - y[i])
          )

  model.optimize()
  return (
    round(model.getVal(x[1])),
    round(model.getVal(x[2])),
    round(model.getVal(x[3])),
  )

time_start = time.time()

# parse input
nanobots = []
for line in open("input/23.txt", "r").read().splitlines():
  x1, x2, x3, r = re.findall(r'([\-0-9]+)', line)
  nanobots.append(
    (int(x1), int(x2), int(x3), int(r))
  )

def in_range_of(nanobot1, nanobot2):
  x1, x2, x3, _ = nanobot1
  y1, y2, y3, r = nanobot2
  return manhattan((x1,x2,x3), (y1,y2,y3)) <= r

max_range_nanobot = max(nanobots, key = lambda nanobot: nanobot[3])

part1 = sum( 1 for nanobot in nanobots if in_range_of(nanobot, max_range_nanobot) )
part2 = manhattan((0,0,0), solve(nanobots))

print("Part 1 :", part1)
print("Part 2 :", part2)

time_end = time.time()
print("Total Time: ", (time_end - time_start))
