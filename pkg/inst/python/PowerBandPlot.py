#!/usr/bin/env python


from matplotlib.pyplot import *
import numpy
import sys

matplotlib.rc('xtick', labelsize=10) 
matplotlib.rc('ytick', labelsize=10) 
matplotlib.rc('legend', fontsize=10) 

colors='green','red','blue','yellow','grey','pink','purple','maroon'




# Usage: PowerBandPlot verbose HR xvector lfhfvector ulfvector vlfvector lfvector hfvector [ xvector2 hvector ] plottitle numoftags

n=1
verbose=sys.argv[n]
n=n+1
plothr=sys.argv[n]
n=n+1
xvectorfile=sys.argv[n]
n=n+1
lfhfvectorfile=sys.argv[n]
n=n+1
ulfvectorfile=sys.argv[n]
n=n+1
vlfvectorfile=sys.argv[n]
n=n+1
lfvectorfile=sys.argv[n]
n=n+1
hfvectorfile=sys.argv[n]
n=n+1

if (plothr=='TRUE'):
	numfilas=6
	xvector2file=sys.argv[n]
	n=n+1
	hrvectorfile=sys.argv[n]
	n=n+1
else:
	numfilas=5

stringtitle=sys.argv[n]
n=n+1
stringtitle = stringtitle.replace('_',' ')

numoftags=int(sys.argv[n])
n=n+1

tags=list()
startsframe=list()
endsframe=list()
startstime=list()
endstime=list()
for i in range(numoftags):
	tag=sys.argv[n]
	tags.append(tag)
	n=n+1

	startsframevectorfile=sys.argv[n]
	f=open(startsframevectorfile,'r')
	startsframevector = f.readlines()
	f.close()
	#startsframevector=[float(x) for x in startsframevector]
	startsframe.append([float(x) for x in startsframevector])
	n=n+1

	endsframevectorfile=sys.argv[n]
	f=open(endsframevectorfile,'r')
	endsframevector = f.readlines()
	f.close()
	endsframe.append([float(x) for x in endsframevector])
	#endsframevector=[float(x) for x in endsframevector]
	n=n+1

	startstimevectorfile=sys.argv[n]
	f=open(startstimevectorfile,'r')
	startstimevector = f.readlines()
	f.close()
	#startstimevector=[float(x) for x in startstimevector]
	startstime.append([float(x) for x in startstimevector])
	n=n+1

	endstimevectorfile=sys.argv[n]
	f=open(endstimevectorfile,'r')
	endstimevector = f.readlines()
	f.close()
	#endstimevector=[float(x) for x in endstimevector]
	endstime.append([float(x) for x in endstimevector])
	n=n+1



# ----------------------------------

if (verbose=='TRUE'):
	print('      Plotting power bands')
	sys.stdout.flush()

if (plothr=='TRUE'):
	numfilas=6
else:
	numfilas=5

if (verbose=='TRUE' and numoftags!=0):
	print ('      Types of episodes to plot: '+str(numoftags)+' ('+(', '.join(tags))+')')
	sys.stdout.flush()


f=open(xvectorfile,'r')
xvector = f.readlines()
f.close()

f=open(lfhfvectorfile,'r')
lfhfvector = f.readlines()
f.close()

f=open(ulfvectorfile,'r')
ulfvector = f.readlines()
f.close()

f=open(vlfvectorfile,'r')
vlfvector = f.readlines()
f.close()

f=open(lfvectorfile,'r')
lfvector = f.readlines()
f.close()

f=open(hfvectorfile,'r')
hfvector = f.readlines()
f.close()


if (plothr=='TRUE'):
	f=open(xvector2file,'r')
	xvector2 = f.readlines()
	f.close()

	f=open(hrvectorfile,'r')
	hrvector = f.readlines()
	f.close()


# ----------------------------------

fig=figure()
fig.subplots_adjust(left=0.12, right=0.95, bottom=0.08, top=0.79, hspace=0.20, wspace=0.20)

ax1 = fig.add_subplot(numfilas,1,1)
ax1.plot(xvector,lfhfvector,'-k')
xlabel('No. of frames',size=10)
ylabel('LF/HF')
ylim(ymin=0)
autoscale(axis='x',tight='TRUE')
tick_params(axis='x',labelbottom='off')
tick_params(axis='x',labeltop='on')
ax1.yaxis.set_major_locator(MaxNLocator(5))
ax1.xaxis.set_label_position("top")
if (numoftags!=0):
	for i in range(numoftags):
		for j in range(len(startsframe[i])):
			if j==0:
				axvspan(startsframe[i][j], endsframe[i][j], facecolor=colors[i], alpha=0.3,label=tags[i])
			else:
				axvspan(startsframe[i][j], endsframe[i][j], facecolor=colors[i], alpha=0.3)
grid()


legend(bbox_to_anchor=(.5, 1.7), loc='lower center', borderaxespad=0.,fancybox=True,shadow=True,ncol=3)

ax2 = fig.add_subplot(numfilas,1,2,sharex=ax1)
ax2.plot(xvector,ulfvector,'-k')
ylabel('ULF')
ylim(ymin=0)
autoscale(axis='x',tight='TRUE')
tick_params(axis='x',labelbottom='off')
ax2.yaxis.set_major_locator(MaxNLocator(5))
if (numoftags!=0):
	for i in range(numoftags):
		for j in range(len(startsframe[i])):
			axvspan(startsframe[i][j], endsframe[i][j], facecolor=colors[i], alpha=0.3)
grid()

ax3 = fig.add_subplot(numfilas,1,3,sharex=ax1)
ax3.plot(xvector,vlfvector,'-k')
ylabel('VLF')
ylim(ymin=0)
autoscale(axis='x',tight='TRUE')
tick_params(axis='x',labelbottom='off')
ax3.yaxis.set_major_locator(MaxNLocator(5))
if (numoftags!=0):
	for i in range(numoftags):
		for j in range(len(startsframe[i])):
			axvspan(startsframe[i][j], endsframe[i][j], facecolor=colors[i], alpha=0.3)
grid()

ax4 = fig.add_subplot(numfilas,1,4,sharex=ax1)
ax4.plot(xvector,lfvector,'-k')
ylabel('LF')
ylim(ymin=0)
autoscale(axis='x',tight='TRUE')
tick_params(axis='x',labelbottom='off')
ax4.yaxis.set_major_locator(MaxNLocator(5))
if (numoftags!=0):
	for i in range(numoftags):
		for j in range(len(startsframe[i])):
			axvspan(startsframe[i][j], endsframe[i][j], facecolor=colors[i], alpha=0.3)
grid()

ax5 = fig.add_subplot(numfilas,1,5,sharex=ax1)
ax5.plot(xvector,hfvector,'-k')
ylabel('HF')
ylim(ymin=0)
autoscale(axis='x',tight='TRUE')
tick_params(axis='x',labelbottom='off')
ax5.yaxis.set_major_locator(MaxNLocator(5))
if (numoftags!=0):
	for i in range(numoftags):
		for j in range(len(startsframe[i])):
			axvspan(startsframe[i][j], endsframe[i][j], facecolor=colors[i], alpha=0.3)
grid()

if (plothr=='TRUE'):
	ax6 = fig.add_subplot(numfilas,1,6)
	ax6.plot(xvector2,hrvector,'-k')
	ylabel('HRV')
	xlabel('Time [sec.]',fontsize=10)
	#ylim(ymin=0)
	autoscale(axis='x',tight='TRUE')
	ax6.yaxis.set_major_locator(MaxNLocator(5))
	grid()

	if (numoftags!=0):
		for i in range(numoftags):
			for j in range(len(startstime[i])):
				axvspan(startstime[i][j], endstime[i][j], facecolor=colors[i], alpha=0.3)

	

suptitle(stringtitle, fontsize=16)



show()


