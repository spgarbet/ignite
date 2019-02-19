from varsens import *
import numpy
import csv

sample    = Sample(48, 100, loadFile="sample.txt",delimiter=",")

file=csv.reader(open("output/nmb100s3.csv"))
n=[]
for row in file:
  n.append(float(row[0]))
  
objective = Objective(48, 100, sample=sample, obj_vals=numpy.array(n).reshape(9800,1))

v = Varsens(objective)
v.sens
v.sens_t
