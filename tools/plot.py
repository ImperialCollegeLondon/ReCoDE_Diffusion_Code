#! /usr/bin/env python3

import numpy
import matplotlib.pyplot as plt

x, y = numpy.loadtxt('OutputFile.txt', unpack=True)
fig, ax = plt.subplots()
ax.plot(x, y, label='Flux Profile', marker='o')
ax.set_xlabel('Distance x')
ax.set_ylabel('Flux')
ax.legend()
plt.show()
