#!/usr/bin/env python


from matplotlib.pyplot import *
import numpy
import sys


# Usage: SinglePlot verbose xvectorfile yvectorfile figuretitle xtitle ylabel numoftags [tag1 starts1 durations1 tag2 ....]

colors='green','red','blue','yellow','grey','pink','purple','maroon'

verbose=sys.argv[1]
xvectorfile=sys.argv[2]
yvectorfile=sys.argv[3]

figuretitle=sys.argv[4]
figuretitle = figuretitle.replace('_',' ')

xtitle=sys.argv[5]
xtitle = xtitle.replace('_',' ')

ytitle=sys.argv[6]
ytitle = ytitle.replace('_',' ')

numoftags=int(sys.argv[7])

f=open(xvectorfile,'r')
xvector = f.readlines()
f.close()

f=open(yvectorfile,'r')
yvector = f.readlines()
f.close()



if (verbose=='TRUE'):
	print '      xvectorfile: '+xvectorfile+' ('+str(len(xvector))+' samples)'
	print '      yvectorfile: '+yvectorfile+' ('+str(len(yvector))+' samples)'
	sys.stdout.flush()

plot(xvector,yvector,'-k')
xlim(float(xvector[0]),float(xvector[-1]))

tags=list()
indarg=8
for i in range(numoftags):
	tag=sys.argv[indarg]
	tags.append(tag)
	indarg=indarg+1

	startsvectorfile=sys.argv[indarg]
	f=open(startsvectorfile,'r')
	startsvector = f.readlines()
	f.close()
	startsvector=[float(x) for x in startsvector]
	indarg=indarg+1

	durationsvectorfile=sys.argv[indarg]
	f=open(durationsvectorfile,'r')
	durationsvector = f.readlines()
	f.close()
	durationsvector=[float(x) for x in durationsvector]
	indarg=indarg+1
	endsvector=[sum(pair) for pair in zip(startsvector, durationsvector)] 

	#axhline(y=53,xmin=startsvector[0],xmax=endsvector[0])
	for j in range(len(startsvector)):
		axvspan(startsvector[j], endsvector[j], ymin=0.04+i*0.03, ymax=0.06+i*0.03, facecolor=colors[i], alpha=0.5)
		axvline(startsvector[j],ymin=0.02+i*0.03, ymax=0.98-i*0.03,alpha=0.5,color=colors[i],ls='-',)
		if j==0:
			axvline(endsvector[j],ymin=0.02+i*0.03, ymax=0.98-i*0.03,alpha=0.5,color=colors[i],ls='-',label=tags[i])
		else:
			axvline(endsvector[j],ymin=0.02+i*0.03, ymax=0.98-i*0.03,alpha=0.5,color=colors[i],ls='-')


if (verbose=='TRUE' and numoftags!=0):
	print '      Types of episodes to plot: '+str(numoftags)+' ('+(', '.join(tags))+')'
	sys.stdout.flush()


leg=legend(fancybox=True,shadow=True)
for t in leg.get_texts():
    t.set_fontsize('small')
xlabel(xtitle)
ylabel(ytitle)
title(figuretitle)
grid()
show()

