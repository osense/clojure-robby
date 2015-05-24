#!/usr/bin/python2

import sys
import os
from matplotlib import pyplot as plt

def read_file(fname):
    v = []
    with open(fname, 'r') as f:
        for line in f:
            v.append(float(line))
    return v

legend_title = None
for arg in sys.argv[1:]:
    try:
        vals = read_file(arg)
        plt.plot(vals, label = arg)
    except:
        if arg[0] != '-':
            legend_title = arg
        print('skipping ' + arg)
        pass

plt.xlabel('Generation')
plt.ylabel('Fitness')
plt.legend(loc = 4, title = legend_title)

if '-s' in sys.argv:
    plt.show()
else:
    plt.savefig('out-graph.png', bbox_inches='tight')

