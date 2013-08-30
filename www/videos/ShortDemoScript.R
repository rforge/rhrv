library(RHRV)
hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)

hrv.data = LoadBeatAscii(hrv.data, "example.beats", RecordPath = ".")

hrv.data = BuildNIHR(hrv.data)
PlotNIHR(hrv.data)

hrv.data = FilterNIHR(hrv.data)
PlotNIHR(hrv.data)

hrv.data = EditNIHR(hrv.data)

hrv.data = AddEpisodes(hrv.data, InitTimes = c(200,2000), Tags = c("Not drug","Drug"), Durations = c(1400,2500), Values = c(0,0))
PlotNIHR(hrv.data, Tag="all")

hrv.data = InterpolateNIHR (hrv.data, freqhr = 4)

hrv.data = CreateFreqAnalysis(hrv.data)

hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis= 1, type = "wavelet", wavelet = "la8", bandtolerance = 0.01, relative = FALSE, ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )
PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 200, ymaxratio = 50)
PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 300, ymaxratio = 50, Tag="all")

splitting.data = SplitPowerBandByEpisodes(hrv.data, indexFreqAnalysis = 1, Tag = c("Drug"))

cat("ULF power during drug administration: ",mean(splitting.data$InEpisodes$ULF),"\n")
cat("ULF power before drug administration: ",mean(splitting.data$OutEpisodes$ULF),"\n")
cat("VLF power during drug administration: ",mean(splitting.data$InEpisodes$VLF),"\n")
cat("VLF power before drug administration: ",mean(splitting.data$OutEpisodes$VLF),"\n")
cat("LF power during drug administration: ",mean(splitting.data$InEpisodes$LF),"\n")
cat("LF power before drug administration: ",mean(splitting.data$OutEpisodes$LF),"\n")
cat("HF power during drug administration: ",mean(splitting.data$InEpisodes$HF),"\n")
cat("HF power before drug administration: ",mean(splitting.data$OutEpisodes$HF),"\n")
