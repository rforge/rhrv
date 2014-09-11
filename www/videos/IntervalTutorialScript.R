setwd("C:/RHRV")
library(RHRV)

hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)
hrv.data = LoadBeatAscii(hrv.data, RecordName="example.beats", RecordPath=".")

#we add the info about the episodes
hrv.data = AddEpisodes(hrv.data, InitTimes = c(700,2000,5000),
                       Tags = c("Before","During","After"), Durations = c(900,2000,600), 
                       Values = c(0,0,0))

hrv.data = BuildNIHR(hrv.data)
hrv.data=FilterNIHR(hrv.data)
# plot all tags
PlotNIHR(hrv.data, Tag="all")
hrv.data = InterpolateNIHR(hrv.data, freqhr = 4)
# Plot only the "After" episodic information
PlotHR(hrv.data , Tag=c("After"))

#Perform frequency analysys
hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis= 1,
                              type = "wavelet", wavelet = "la8", 
                               bandtolerance = 0.01, relative = FALSE)
# plot episodic information
PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 5000, ymaxratio = 50, Tag = "all")
PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 5000, ymaxratio = 50, Tag = "After")

#We divide the heart rate by tag "before"
splitting.data = SplitHRbyEpisodes(hrv.data, Tag = c("Before"))
cat("Apnea mean: ",mean(splitting.data$InEpisodes),"\n")
cat("Apnea mean: ",mean(splitting.data$OutEpisodes),"\n")

#We divide the heart rate by tag "During"
splitting.data = SplitHRbyEpisodes(hrv.data, Tag = c("During"))
cat("Apnea mean: ",mean(splitting.data$InEpisodes),"\n")

#We divide the spectral analysis for tag "Before"
splitting.data = SplitPowerBandByEpisodes(hrv.data,
                                          indexFreqAnalysis = 1, Tag = c("Before"))

cat("Apnea mean: ",mean(splitting.data$InEpisodes$ULF),"\n")

#We divide the spectral analysis for tag "After"
splitting.data = SplitPowerBandByEpisodes(hrv.data,
                                          indexFreqAnalysis = 1, Tag = c("After"))
cat("Normal mean: ",mean(splitting.data$OutEpisodes$ULF),"\n")
