LoadBeatAmbit <- function(HRVData, RecordName, RecordPath=".", verbose = NULL) {
#-------------------------------
# Loads beats from a Suunto Ambit XML file
#-------------------------------
#	RecordName -> record containing RR values
#	RecordPath -> path
#-------------------------------

	dir=getwd()

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData, verbose)
	    }
	if (HRVData$Verbose) {
		cat("** Loading beats positions for record:", RecordName,"**\n")
		cat("   Path:",RecordPath,"\n")
	}

	setwd(RecordPath)

	#Date and time information
	aux=scan(RecordName,what=character(0),strip.white=TRUE,quiet=TRUE)
	date=aux[grepl('DateTime',aux)]
	dateAux=substr(date,11,20)

    time <- substr(date,22,29)
		
		
	if (HRVData$Verbose) {
		
		cat("   Date: ",dateAux, "\n")
		cat("   Time: ",time, "\n")
	}
	datetimeinfo = paste(dateAux,time,sep = " ")
	HRVData$datetime=datetimeinfo

	BeatPosition=grep('IBI',aux)
    rawIBI=aux[BeatPosition[1]:BeatPosition[2]]
    rawIBI[1]=gsub('<IBI>','',rawIBI[1])
    rawIBI[length(rawIBI)]=gsub('</IBI>','',rawIBI[length(rawIBI)])
	HRVData$Beat$RR=as.numeric(rawIBI)

	HRVData$Beat$Time=cumsum(HRVData$Beat$RR)/1000
	if (HRVData$Verbose) {
        	cat("   Number of beats:", length(HRVData$Beat$Time), "\n")
    	}
	setwd(dir)
	return(HRVData)
}
