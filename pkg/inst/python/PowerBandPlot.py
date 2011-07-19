#!/usr/bin/env python


from matplotlib.pyplot import *
import numpy
import sys


# Usage: PowerBandPlot verbose HR

verbose=sys.argv[1]
plothr=sys.argv[2]

if (verbose=='TRUE'):
	print('      Plotting power bands')
	sys.stdout.flush()


