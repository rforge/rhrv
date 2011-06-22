SetMatplotlib <-
function(HRVData,Matplotlib) {
# ------------------------------
# Sets matplotlib mode on or off
# ------------------------------
	if (HRVData$Verbose) {
		if (Matplotlib == TRUE) {
                	cat("** Matplotlib mode enabled **\n")
		}

		if (Matplotlib == FALSE) {
                	cat("** Matplotlib mode disabled **\n")
		}
        }

   	HRVData$Matplotlib=Matplotlib

   	return(HRVData)
}

