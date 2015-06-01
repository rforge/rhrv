setwd("C:/RHRV")
library(RHRV)

hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)
hrv.data = LoadBeatAscii(hrv.data, RecordName="nonlinearHB.beats", RecordPath=".")
hrv.data = BuildNIHR(hrv.data)

#We create the data structure to store the nonlinear analysis
hrv.data = CreateNonLinearAnalysis(hrv.data)

#We check that the RR series is nonlinear
hrv.data = NonlinearityTests(hrv.data)
hrv.data = SurrogateTest(hrv.data, significance = 0.05,
                         useFunction = timeAsymmetry2, tau=4, doPlot = TRUE)

#We estimate the time lag to be used in phase space reconstruction
kTimeLag=CalculateTimeLag(hrv.data,method="first.e.decay",lagMax=100,doPlot=TRUE)

#We estimate the embedding dimension of the phase space reconstruction
kEmbeddingDim = CalculateEmbeddingDim(hrv.data, numberPoints = 10000,
                                      timeLag = kTimeLag, maxEmbeddingDim = 15)
kEmbeddingDim

#We calculate the correlation dimension
hrv.data = CalculateCorrDim(hrv.data, indexNonLinearAnalysis = 1,
                            minEmbeddingDim = kEmbeddingDim - 1, 
                            maxEmbeddingDim = kEmbeddingDim + 2,
                            timeLag = kTimeLag, 
                            minRadius=1, maxRadius=100, pointsRadius = 100,
                            theilerWindow = 20, doPlot = FALSE)
PlotCorrDim(hrv.data,indexNonLinearAnalysis=1)
hrv.data = EstimateCorrDim(hrv.data, indexNonLinearAnalysis=1, regressionRange=c(1.5,10),
                            useEmbeddings=(kEmbeddingDim-1):(kEmbeddingDim+2), 
                           doPlot=FALSE)

#We calculate the Sample entropy
hrv.data = CalculateCorrDim(hrv.data, indexNonLinearAnalysis = 1,
                            minEmbeddingDim = 4*kEmbeddingDim,
                            maxEmbeddingDim = 4*kEmbeddingDim+5,
                            timeLag = kTimeLag, minRadius = 1,
                            maxRadius = 100, pointsRadius = 100,
                            theilerWindow = 20, doPlot = FALSE)
hrv.data = CalculateSampleEntropy(hrv.data, indexNonLinearAnalysis= 1, doPlot = FALSE)
PlotSampleEntropy(hrv.data, indexNonLinearAnalysis=1)
hrv.data = EstimateSampleEntropy(hrv.data, indexNonLinearAnalysis=1,
                                 regressionRange=c(10,20), useEmbeddings = 20:24,
                                 doPlot = TRUE)

par(mfrow = c(1,1))
#We calculate the Maximum Lyapunov exponent

hrv.data = CalculateMaxLyapunov(hrv.data, indexNonLinearAnalysis = 1,
                            minEmbeddingDim= kEmbeddingDim, 
                            maxEmbeddingDim= kEmbeddingDim+2,
                            timeLag = kTimeLag,radius = 3, theilerWindow = 20,
                            doPlot = TRUE)
hrv.data = EstimateMaxLyapunov(hrv.data, indexNonLinearAnalysis = 1,
                               regressionRange = c(1,6),
                               useEmbeddings = (kEmbeddingDim):(kEmbeddingDim+2),
                               doPlot = TRUE)

#We calculate the Detrended Fluctuation Analysis alpha1 and alpha 2 parameters

hrv.data = CalculateDFA(hrv.data, indexNonLinearAnalysis = 1,
                         windowSizeRange = c(6, 300), npoints = 25, doPlot = TRUE)
hrv.data = EstimateDFA(hrv.data, indexNonLinearAnalysis = 1,
                       regressionRange = c(20,100), doPlot = TRUE)

#We perform Recurrence Quantifcation Analysis
hrv.data = RQA(hrv.data, indexNonLinearAnalysis = 1, embeddingDim=kEmbeddingDim, 
              timeLag = kTimeLag, radius = 2, doPlot=TRUE)

names(hrv.data$NonLinearAnalysis[[1]]$rqa)
cat("Entropy of the diagonal lines: ", hrv.data$NonLinearAnalysis[[1]]$rqa$ENTR, "\n")

#Rear RR series example
hrv.data = CreateHRVData( )
hrv.data = LoadBeatAscii(hrv.data,  RecordName="example2.beats", RecordPath=".")
hrv.data = BuildNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)
hrv.data = InterpolateNIHR (hrv.data, freqhr = 4)
hrv.data = CreateNonLinearAnalysis(hrv.data)
hrv.data = SetVerbose(hrv.data,TRUE)

#We check that the RR series is nonlinear
hrv.data = NonlinearityTests(hrv.data)
hrv.data = SurrogateTest(hrv.data, significance = 0.05,
                         useFunction = timeAsymmetry2, tau=4, doPlot = TRUE)

kTimeLag = CalculateTimeLag(hrv.data, method = "first.minimum", lagMax = 300)
kEmbeddingDim = CalculateEmbeddingDim(hrv.data, numberPoints = 10000,
                                      timeLag = kTimeLag, maxEmbeddingDim = 18)
kEmbeddingDim
#We apply nonlinear noise reduction
hrv.data = NonLinearNoiseReduction(hrv.data, embeddingDim = kEmbeddingDim)

#We check that the RR series is nonlinear
hrv.data = NonlinearityTests(hrv.data)
hrv.data = SurrogateTest(hrv.data, significance = 0.05,
                         useFunction = timeAsymmetry2, tau=4, doPlot = TRUE)

hrv.data = PoincarePlot(hrv.data, indexNonLinearAnalysis=1, timeLag=1, 
                        confidenceEstimation = TRUE,confidence = 0.9, doPlot=TRUE)


#We calculate the correlation dimension
hrv.data = CalculateCorrDim(hrv.data, indexNonLinearAnalysis = 1,
                            minEmbeddingDim = kEmbeddingDim - 1, 
                            maxEmbeddingDim = kEmbeddingDim + 2,
                            timeLag = kTimeLag, 
                            minRadius=1, maxRadius=100, pointsRadius = 100,
                            theilerWindow = 20, doPlot = FALSE)
PlotCorrDim(hrv.data,indexNonLinearAnalysis=1)
hrv.data = EstimateCorrDim(hrv.data, indexNonLinearAnalysis=1, regressionRange=c(60,90),
                           useEmbeddings=(kEmbeddingDim-1):(kEmbeddingDim+2), 
                           doPlot=TRUE)
