import sys
from gurobipy import *


model = read(sys.argv[1]+".lp")
model.setParam('TimeLimit', 200*60)
model.optimize()
#model.computeIIS()
#model.write(sys.argv[1] + ".ilp")
model.write(sys.argv[1] + ".sol")

