#!/usr/bin/env python


from matplotlib.pyplot import *
import numpy
import sys

matplotlib.rc('xtick', labelsize=10) 
matplotlib.rc('ytick', labelsize=10) 



# Usage: PowerBandPlot verbose HR xvector lfhfvector ulfvector

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




if (verbose=='TRUE'):
	print('      Plotting power bands')
	sys.stdout.flush()

if (plothr=='TRUE'):
	numfilas=6
else:
	numfilas=5

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


fig=figure()
ax1 = fig.add_subplot(numfilas,1,1)
ax1.plot(xvector,lfhfvector,'-k')
xlabel('No. of frames')
ylabel('LF/HF')
ylim(ymin=0)
tick_params(axis='x',labelbottom='off')
tick_params(axis='x',labeltop='on')
ax1.yaxis.set_major_locator(MaxNLocator(5))
ax1.xaxis.set_label_position("top")
grid()


ax2 = fig.add_subplot(numfilas,1,2,sharex=ax1)
ax2.plot(xvector,ulfvector,'-k')
ylabel('ULF')
ylim(ymin=0)
tick_params(axis='x',labelbottom='off')
#ax2.yaxis.major.formatter.set_powerlimits((0,0))
ax2.yaxis.set_major_locator(MaxNLocator(5))
grid()

ax3 = fig.add_subplot(numfilas,1,3,sharex=ax1)
ax3.plot(xvector,vlfvector,'-k')
ylabel('VLF')
ylim(ymin=0)
tick_params(axis='x',labelbottom='off')
ax3.yaxis.set_major_locator(MaxNLocator(5))
grid()

ax4 = fig.add_subplot(numfilas,1,4,sharex=ax1)
ax4.plot(xvector,lfvector,'-k')
ylabel('LF')
ylim(ymin=0)
tick_params(axis='x',labelbottom='off')
ax4.yaxis.set_major_locator(MaxNLocator(5))
grid()

ax5 = fig.add_subplot(numfilas,1,5,sharex=ax1)
ax5.plot(xvector,hfvector,'-k')
ylabel('HF')
ylim(ymin=0)
tick_params(axis='x',labelbottom='off')
ax5.yaxis.set_major_locator(MaxNLocator(5))
grid()

if (plothr=='TRUE'):
	ax6 = fig.add_subplot(numfilas,1,6)
	ax6.plot(xvector2,hrvector,'-k')
	ylabel('HRV')
	xlabel('Time [sec.]',fontsize=10)
	#ylim(ymin=0)
	ax6.yaxis.set_major_locator(MaxNLocator(5))
	grid()

suptitle(stringtitle, fontsize=16)


show()


