#!/usr/bin/env python


from matplotlib.pyplot import *
import numpy
import sys

# Usage: SinglePlot verbose xvectorfile yvectorfile figuretitle xtitle ylabel

verbose=sys.argv[1]
xvectorfile=sys.argv[2]
yvectorfile=sys.argv[3]

figuretitle=sys.argv[4]
figuretitle = figuretitle.replace('_',' ')

xtitle=sys.argv[5]
xtitle = xtitle.replace('_',' ')

ytitle=sys.argv[6]
ytitle = ytitle.replace('_',' ')


f=open(xvectorfile,'r')
xvector = f.readlines()
f.close()

f=open(yvectorfile,'r')
yvector = f.readlines()
f.close()


if (verbose=='TRUE'):
	print '      xvectorfile:',xvectorfile,' (',len(xvector),' samples )'
	print '      yvectorfile:',yvectorfile,' (',len(yvector),' samples )'
	sys.stdout.flush()

plot(xvector,yvector,'-k')
xlim(float(xvector[0]),float(xvector[-1]))
xlabel(xtitle)
ylabel(ytitle)
title(figuretitle)
grid()
show()

