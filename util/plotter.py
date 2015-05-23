#!/usr/bin/python2

import sys
import os
from matplotlib import pyplot as plt

infile = sys.argv[1]
vals = []
with open(infile, 'r') as f:
    for line in f:
        vals.append(float(line))

plt.plot([i for i in range(len(vals))], vals)
plt.xlabel('Generation')
plt.ylabel('Fitness')

(out, _) = os.path.splitext(infile)
plt.savefig(out + '.png', bbox_inches='tight')

