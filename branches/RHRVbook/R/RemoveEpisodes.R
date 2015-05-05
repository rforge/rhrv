RemoveEpisodes <-
function(HRVData, Tags=NULL, Indexes=NULL) {	
#-----------------------------
# Removes Episodes from record
#-----------------------------
#	Tags -> Vector containing types of episodes
#	Indexes -> Vector containing indexes of episodes


	
   	if (HRVData$Verbose) {
		cat("** Removing episodes **\n")
	}

	noEpBefore <- length(HRVData$Episodes$InitTime)
	if (HRVData$Verbose) {
      	cat("   Number of episodes before removal: ",noEpBefore,"\n")
   	}

   	

	HRVData$Episodes <- selectEpisodes(HRVData$Episodes,Tags,Indexes)
	HRVData$Episodes <- HRVData$Episodes[!HRVData$Episodes$selected,]  # Removal happens here
	HRVData$Episodes$selected <- NULL

	HRVData$Episodes <- HRVData$Episodes[order(HRVData$Episodes$InitTime),]  # Sort episodes by InitTime


	noEpAfter <- length(HRVData$Episodes$InitTime)
	if (HRVData$Verbose) {
   	cat("   Number of episodes after removal: ",noEpAfter,"\n")
	}

	if (HRVData$Verbose) {
		if (noEpAfter == noEpBefore) {
			cat("   No episode was removed\n")
		} else {
			cat("   Number of episodes removed: ",noEpBefore-noEpAfter,"\n")
		}
	}

	if (length(HRVData$Episodes$InitTime) == 0) {
		HRVData$Episodes <- NULL
		if (HRVData$Verbose) {
			cat("   All episodes were removed from data\n")
		}
	}

	return(HRVData)
}

