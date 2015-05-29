ModifyEpisodes <-
function(HRVData, Tags=NULL, Indexes=NULL, NewInitTimes=NULL, NewTags=NULL, NewDurations=NULL ,NewValues=NULL) {	
#-----------------
# Changes episodes 
#-----------------
#   Tags -> Vector containing types of episodes to change
#   Indexes -> Vector containing indexes of episodes to change
#	NewInitTimes -> Vector containing new initial times in seconds
#	NewTags -> Vector containing new types of episodes
#	NewDurations -> Vector containing new durations in seconds
#	NewValues -> Vector containing new numerical values for episodes


	if (HRVData$Verbose) {
		cat("** Modifying episodes **\n")
	}

	if (is.null(NewInitTimes) & is.null(NewTags) & is.null(NewDurations)
		& is.null(NewValues)) {
		if (HRVData$Verbose) {
			cat("   No change made\n")
		}
		return(HRVData)
	}

	HRVData$Episodes <- selectEpisodes(HRVData$Episodes,Tags,Indexes)

	epToMod <- length(HRVData$Episodes$InitTime[HRVData$Episodes$selected])

	if (HRVData$Verbose) {
		cat("   Number of episodes to modify:",epToMod,"\n")
	}

	if (epToMod == 0) {
		HRVData$Episodes$selected <- NULL
		return(HRVData)
	}

	if (!is.null(NewTags)) {
		HRVData$Episodes$Type <- as.character(HRVData$Episodes$Type)
		HRVData$Episodes$Type[HRVData$Episodes$selected] <- NewTags
		HRVData$Episodes$Type <- factor(HRVData$Episodes$Type)
	}

	if (!is.null(NewInitTimes)) {
		HRVData$Episodes$InitTime[HRVData$Episodes$selected] <- NewInitTimes
	}

	if (!is.null(NewDurations)) {
		HRVData$Episodes$Duration[HRVData$Episodes$selected] <- NewDurations
	}


	if (!is.null(NewValues)) {
		HRVData$Episodes$Value[HRVData$Episodes$selected] <- NewValues
	}

	HRVData$Episodes$selected <- NULL

	HRVData$Episodes <- HRVData$Episodes[order(HRVData$Episodes$InitTime),]  # Sort episodes by InitTime

	EpBeforeMod <- length(HRVData$Episodes$InitTime)
	HRVData$Episodes <- HRVData$Episodes[!duplicated(HRVData$Episodes),]  # Remove duplicated episodes
	EpAfterMod <- length(HRVData$Episodes$InitTime)

	if (EpBeforeMod != EpAfterMod) {
		if (HRVData$Verbose) {
			cat("   Removing",EpBeforeMod-EpAfterMod,"duplicated episodes\n")
			cat("   Number of episodes:",EpAfterMod,"\n")
		}
	}

	return(HRVData)
}

