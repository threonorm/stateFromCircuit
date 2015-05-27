import sys
from gurobipy import *


model = read(sys.argv[1]+".lp")
model.setParam('TimeLimit', 2*60)
model.optimize()
model.write(sys.argv[1] + ".sol")
