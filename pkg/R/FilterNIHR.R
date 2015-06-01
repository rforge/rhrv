FilterNIHR <-
function(HRVData, long=50, last=13, minbpm=25, maxbpm=200, mini=NULL, maxi=NULL, fixed=NULL, verbose=NULL) {
# ----------------------------------------------------------------------------------------
# Filters non-interpolated heart rate
# Filtering is based on comparisons with previous and last values and with an updated mean
# ----------------------------------------------------------------------------------------
	
	if (!is.null(mini)) {
		cat("  --- Warning: deprecated argument ignored ---\n    --- See help for more information!! ---\n")
	}
	
	if (!is.null(maxi)) {
		cat("  --- Warning: deprecated argument ignored ---\n    --- See help for more information!! ---\n")
	}
	
	if (!is.null(fixed)) {
		cat("  --- Warning: deprecated argument ignored ---\n    --- See help for more information!! ---\n")
	}
	
	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}

	if (is.null(HRVData$Beat$niHR)) { 
      stop("  --- Non-interpolated heart rate not present ---\n    --- Quitting now!! ---\n")
	}

	if (is.null(HRVData$Beat$Time)) { 
      stop("  --- Heart beats not present ---\n    --- Quitting now!! ---\n")
	}

	if (is.null(HRVData$Beat$RR)) { 
      stop("  --- RR series not present ---\n    --- Quitting now!! ---\n")
	}

	
	if (HRVData$Verbose) {
		cat("** Filtering non-interpolated Heart Rate **\n")
		cat("   Number of original beats:",length(HRVData$Beat$niHR),"\n")
	}
		
	n=length(HRVData$Beat$niHR)
	lon=50
	last=13
	ind=seq(from=0,to=0,length.out=n)
	minbpm=25
	maxbpm=200
	# call C function to load the results in out
	out <-.C("filterhr",hr=as.double(HRVData$Beat$niHR),as.integer(n),as.integer(lon),as.integer(last),as.integer(minbpm),as.integer(maxbpm),ind=as.integer(ind))
	#copiei a maneira que implmentou Leandro para actualizar a estructura HRVData$Beat
	hr=HRVData$Beat$niHR[out$ind==1]
	beat=HRVData$Beat$Time[out$ind==1]
	rr=HRVData$Beat$RR[out$ind==1]
	

	if (HRVData$Verbose) {
		cat("   Number of accepted beats:",length(hr),"\n")
	}
	
	HRVData$Beat = data.frame (Time=beat, niHR=hr, RR=rr)
	return(HRVData)
}

