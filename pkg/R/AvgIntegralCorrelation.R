AvgIntegralCorrelation <-
function(HRVData, Data, m, tau, r) {
# -------------------------------------
# Averages Integral Correlation
# -------------------------------------

	Cmr = IntegralCorrelation(HRVData,Data,m=m,tau=tau,r=r)

	if (HRVData$Verbose) {
		cat("** Averaging Integral Correlation **\n")
	}

	Phi=log(sum(Cmr)/length(Cmr))

	if (HRVData$Verbose) {
		cat("  Average Integral Correlation: ", sum(Cmr)/length(Cmr), "\n", sep="")
	}
	
	return(Phi)
}

