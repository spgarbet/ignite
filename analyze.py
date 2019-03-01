from varsens import *
import numpy
import csv

sample    = Sample(48, 100, loadFile="sample.txt",delimiter=",")

file=csv.reader(open("output/nmb100s2.csv"))
n=[]
for row in file:
  n.append(float(row[0]))
  
objective = Objective(48, 100, sample=sample, obj_vals=numpy.array(n).reshape(9800,1))

v = Varsens(objective)
v.sens
v.sens_t

# With sufficient resolution this would not be required, but it is what it is
sens   = numpy.clip(v.sens, 0, 1)   # Clip
if(sum(sens) > 1):
  sens   = sens / sum(sens)           # Normalize

sens_t = [x - numpy.min(v.sens_t) for x in v.sens_t]
sens_t = numpy.clip(sens_t, 0, 1) # Clip


with open('saltelli-single-2.txt', 'w') as f:
    for item in map(lambda x: round(x, 2), 100*sens.reshape(48)):
        f.write("%s\n" % item)

with open('saltelli-total-2.txt', 'w') as f:
    for item in map(lambda x: round(x, 2), 100*sens_t.reshape(48)):
        f.write("%s\n" % item)
