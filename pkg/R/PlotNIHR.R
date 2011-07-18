PlotNIHR <-
function(HRVData,Tag=NULL, verbose=NULL) {
#------------------------------------------------
# Plots non-interpolated instantaneous heart rate
#------------------------------------------------
#	Tag -> Tags of episodes to include in the plot
#    "all" includes all types


	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Plotting non-interpolated instantaneous heart rate **\n");
		if (HRVData$Matplotlib) {
			cat("   Matplotlib mode enabled\n")
		}
	}


   if (!is.null(Tag) & is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present ---\n    --- Quitting now!! ---\n")
   }
	
	if (is.null(HRVData$Beat$Time)) { 
      stop("  --- Beats not present ---\n    --- Quitting now!! ---\n")
	}
	
	if (is.null(HRVData$Beat$niHR)) { 
      stop("  --- Non-interpolated heart rate not present ---\n    --- Quitting now!! ---\n")
	}
	
	if (HRVData$Verbose) {
		cat("   Number of points:",length(HRVData$Beat$Time),"\n");
	}
	
	HRMin=min(HRVData$Beat$niHR)
	HRMax=max(HRVData$Beat$niHR)
	HRDiff=HRMax-HRMin

	if (!is.null(Tag)) {
		if (Tag[1]=="all") {
			Tag=levels(HRVData$Episodes$Type)
		}

		if (HRVData$Verbose) {
			cat("   Episodes in plot:",Tag,"\n")
		}
	}

	if (!HRVData$Matplotlib) {
		plot(HRVData$Beat$Time,HRVData$Beat$niHR,type="l",xlab="time (sec.)",ylab="HR (beats/min.)",ylim=c(HRMin-0.1*HRDiff,HRMax))

		grid()
		
		if (!is.null(Tag)) {

			# Data for representing episodes
			EpisodesAuxLeft=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag]
			EpisodesAuxBottom=c(HRMin-0.09*HRDiff,HRMin-0.04*HRDiff)
			EpisodesAuxRight=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag] + 
				HRVData$Episodes$Duration[HRVData$Episodes$Type %in% Tag]
			EpisodesAuxTop=c(HRMin-0.07*HRDiff,HRMin-0.02*HRDiff)
			EpisodesAuxType=HRVData$Episodes$Type[HRVData$Episodes$Type %in% Tag]

			Pal=rainbow(length(Tag))
			Bor=Pal[match(EpisodesAuxType,Tag)]

			cat("   No of episodes:",length(EpisodesAuxLeft),"\n")
			cat("   No of classes of episodes:",length(Pal),"\n")

			rect(EpisodesAuxLeft,EpisodesAuxBottom,EpisodesAuxRight,EpisodesAuxTop,border=Bor,col=Bor)

			for (i in 1:length(EpisodesAuxLeft)) {
				lines(rep(EpisodesAuxLeft[i],times=2),c(HRMin-0.1*HRDiff,HRMax),lty=2,col=Bor[i])
				lines(rep(EpisodesAuxRight[i],times=2),c(HRMin-0.1*HRDiff,HRMax),lty=2,col=Bor[i])
			}

			legend("topright",inset=0.01,legend=Tag,fill=Pal,cex=0.6,horiz=FALSE,bg='white')
		}

		title(main="Non-interpolated instantaneous heart rate")

	} else {

		xvector=tempfile(pattern = "RHRV", tmpdir = tempdir())
		write(HRVData$Beat$Time,file=xvector,ncolumns=1)

		yvector=tempfile(pattern = "RHRV", tmpdir = tempdir())
		write(HRVData$Beat$niHR,file=yvector,ncolumns=1)


		pythonscript=paste(system.file(package="RHRV"),"python","SinglePlot.py",sep=.Platform$file.sep)
		if (HRVData$Verbose) {
			cat("   Invoking python script:",pythonscript,"\n")
		}

		figuretitle="Non-interpolated_instantaneous_heart_rate"
		xtitle="time_[sec.]"
		ytitle="HR_[beats/min.]"

		pythonscript=paste(pythonscript,HRVData$Verbose,xvector,yvector,figuretitle,xtitle,ytitle,length(Tag))

		for (i in 1:length(Tag)) {
			startsvector=tempfile(pattern = "RHRV", tmpdir = tempdir())
			write(HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag[i]],file=startsvector,ncolumns=1)
			durationsvector=tempfile(pattern = "RHRV", tmpdir = tempdir())
			write(HRVData$Episodes$Duration[HRVData$Episodes$Type %in% Tag[i]],file=durationsvector,ncolumns=1)
			pythonscript=paste(pythonscript,Tag[i],startsvector,durationsvector)
		}

		#cat("Calling:\n",pythonscript,"\n")

		res=system(pythonscript,intern=FALSE)
		if (res!=0) {
		   cat("   --- ERROR: invocation returned an error code ---\n   --- Check python and matplotlib installation ---\n")
		}

	}

}

