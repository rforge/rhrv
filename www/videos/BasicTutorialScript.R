install.packages("RHRV")
library(RHRV)

hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)

setwd("C:/RHRV")
hrv.data = LoadBeatAscii(hrv.data, "example.beats", RecordPath = ".")
plot(hrv.data$Beat$Time)

hrv.data = BuildNIHR(hrv.data)
PlotNIHR(hrv.data)

hrv.data = FilterNIHR(hrv.data)
PlotNIHR(hrv.data)
hrv.data.bak=hrv.data
hrv.data = FilterNIHR(hrv.data, long=50, last=10, minbpm=25, maxbpm=180)
PlotNIHR(hrv.data)
hrv.data=hrv.data.bak
hrv.data = EditNIHR(hrv.data)
PlotNIHR(hrv.data)

hrv.data = InterpolateNIHR (hrv.data, freqhr = 4)
PlotHR(hrv.data)

hrv.data = CreateTimeAnalysis(hrv.data, size = 300, interval = 7.8125)

hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis= 1, size = 300, shift = 30, sizesp = 2048, type = "fourier", ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )

PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 2500, ymaxratio = 1.4)


hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = CalculatePowerBand( hrv.data , indexFreqAnalysis= 2, type = "wavelet", wavelet = "la8", bandtolerance = 0.01, relative = FALSE, ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )
PlotPowerBand(hrv.data, indexFreqAnalysis = 2, ymax = 200, ymaxratio = 50)