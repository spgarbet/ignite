from varsens import *
import numpy
import csv

vn = []
lb = []
ub = []

with open('psa-ranges.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    next(csv_reader, None)
    for row in csv_reader:
        vn.append(row[0])
        lb.append(float(row[2]))
        ub.append(float(row[3]))

lb = numpy.array(lb)
ub = numpy.array(ub)

def g_scaling(x):
  return scale.linear(numpy.array(x), lb, ub)
  
sample = Sample(len(lb), 100, g_scaling)

sample.export(delimiter=",")
