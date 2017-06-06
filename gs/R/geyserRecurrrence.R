# 06.06.2017, 16:46:50
# 26.05.2017, 20:03:15 added in svn rhrv/gs/R/geyserRecurrrence.R 
# 25.05.2017, 17:24:33

setwd("/Users/gs/projects/rforge/rhrv/gs/Rnw_out"); Sweave(file= "../Rnw/recurrence.Rnw", output="recurrence.tex", debug=TRUE, eps=FALSE, figs.only=FALSE); 

# test set for bivariate recurrence plot

library(MASS)
library(nonlinearTseries)


local.recurrencePlotAux=function(neighs, dim=NULL, lag=NULL, radius=NULL){

	# just for reference. This function is inlined
	neighbourListNeighbourMatrix  = function(){
	  #neighs.matrix = Diagonal(ntakens)
	  for (i in 1:ntakens){
	    if (length(neighs[[i]])>0){
	      for (j in neighs[[i]]){
	        neighs.matrix[i,j] = 1
	      }
	    }
	  }
	  return (neighs.matrix)
	}

  ntakens=length(neighs)
  neighs.matrix <- matrix(nrow=ntakens,ncol=ntakens)
  #neighbourListNeighbourMatrix()
    #neighs.matrix = Diagonal(ntakens)
	  for (i in 1:ntakens){
	  	neighs.matrix[i,i] = 1 # do we want the diagonal fixed to 1
	    if (length(neighs[[i]])>0){
	      for (j in neighs[[i]]){
	        neighs.matrix[i,j] = 1
	      }
	    }
	  }

	main <- paste("Recurrence Plot: ", 
			deparse(substitute(neighs))
			)
	more <- NULL
	
	#use compones of neights if available
	if (!is.null(dim)) more <- paste(more," dim:",dim)
	if (!is.null(lag)) more <- paste(more," lag:",lag)
	if (!is.null(radius)) more <- paste(more," radius:",radius)
	
	if (!is.null(more)) main <- paste(main,"\n",more)

  # need no print because it is not a trellis object!!
  #print(
  	image(x=1:ntakens, y=1:ntakens,
		 z=neighs.matrix,xlab="i", ylab="j", 
		col="black",
		#xlim=c(1,ntakens), ylim=c(1,ntakens),
		useRaster=TRUE,  #? is this safe??
		main=main
		)
 #	)
 
}

# univariate variant, assuming attributes
local.recurrencePlot1=function(neighs){
    dim <- attr(neighs,"embedding.dim")
    lag <- attr(neighs,"time.lag")
    radius <- attr(neighs,"radius")
	# just for reference. This function is inlined
	neighbourListNeighbourMatrix  = function(){
	  #neighs.matrix = Diagonal(ntakens)
	  for (i in 1:ntakens){
	    if (length(neighs[[i]])>0){
	      for (j in neighs[[i]]){
	        neighs.matrix[i,j] = 1
	      }
	    }
	  }
	  return (neighs.matrix)
	}

  ntakens=length(neighs)
  neighs.matrix <- matrix(nrow=ntakens,ncol=ntakens)
  #neighbourListNeighbourMatrix()
    #neighs.matrix = Diagonal(ntakens)
	  for (i in 1:ntakens){
	  	neighs.matrix[i,i] = 1 # do we want the diagonal fixed to 1
	    if (length(neighs[[i]])>0){
	      for (j in neighs[[i]]){
	        neighs.matrix[i,j] = 1
	      }
	    }
	  }

	main <- paste("Recurrence Plot: ", 
			deparse(substitute(neighs))
			)
	more <- NULL
	
	#use compones of neights if available
	if (!is.null(dim)) more <- paste(more," dim:",dim)
	if (!is.null(lag)) more <- paste(more," lag:",lag)
	if (!is.null(radius)) more <- paste(more," radius:",radius)
	
	if (!is.null(more)) main <- paste(main,"\n",more)

  # need no print because it is not a trellis object!!
  #print(
  	image(x=1:ntakens, y=1:ntakens,
		 z=neighs.matrix,xlab="i", ylab="j", 
		col="black",
		#xlim=c(1,ntakens), ylim=c(1,ntakens),
		useRaster=TRUE,  #? is this safe??
		main=main
		)
 #	)
 
}


# bivariate variant, assuming attributes
# same dimension and size required
local.recurrencePlot2=function(neighs1, neighs2){
    dim1 <- attr(neighs1,"embedding.dim")
    lag1 <- attr(neighs1,"time.lag")
    radius1 <- attr(neighs1,"radius")
 
    dim2 <- attr(neighs2,"embedding.dim")
    lag2 <- attr(neighs2,"time.lag")
    radius2 <- attr(neighs2,"radius")
    
	# just for reference. This function is inlined
	neighbourListNeighbourMatrix  = function(){
	  #neighs.matrix = Diagonal(ntakens)
	  for (i in 1:ntakens){
	    if (length(neighs[[i]])>0){
	      for (j in neighs[[i]]){
	        neighs.matrix[i,j] = 1
	      }
	    }
	  }
	  return (neighs.matrix)
	}

#1
  ntakens1=length(neighs1)
  neighs1.matrix <- matrix(nrow=ntakens1,ncol=ntakens1)
  #neighbourListNeighbourMatrix()
    #neighs.matrix = Diagonal(ntakens)
	  for (i in 1:ntakens1){
	  	neighs1.matrix[i,i] = 1 # do we want the diagonal fixed to 1
	    if (length(neighs1[[i]])>0){
	      for (j in neighs1[[i]]){
	        neighs1.matrix[i,j] = 1
	      }
	    }
	  }
#2
  ntakens2=length(neighs2)
  neighs2.matrix <- matrix(nrow=ntakens2,ncol=ntakens2)
  #neighbourListNeighbourMatrix()
    #neighs.matrix = Diagonal(ntakens)
	  for (i in 1:ntakens2){
	  	neighs2.matrix[i,i] = 1 # do we want the diagonal fixed to 1
	    if (length(neighs2[[i]])>0){
	      for (j in neighs2[[i]]){
	        neighs2.matrix[i,j] = 1
	      }
	    }
	  }
# merge
neighs.matrix <- neighs1.matrix
# replace upper triangle by neighs2.matrix
for (i in 1:ntakens2){
	for (j in i:ntakens2)
	neighs.matrix[i,j] <- - neighs2.matrix[i,j]  #for colour
}


	main <- paste("Recurrence Plot: ", 
			deparse(substitute(neighs1)),
			deparse(substitute(neighs2))
			)
	more <- NULL
	
	#use compones of neights if available
	if (!is.null(dim1)) more <- paste(more," dim:",dim1, dim2)
	if (!is.null(lag1)) more <- paste(more," lag:",lag1, lag2)
	if (!is.null(radius1)) more <- paste(more," radius:",radius1, radius2)
	
	if (!is.null(more)) main <- paste(main,"\n",more)
#

ntakens <- ntakens1

  # need no print because it is not a trellis object!!
  #print(
  	image(x=1:ntakens, y=1:ntakens,
		 z=neighs.matrix,xlab="i", ylab="j", 
		col=c("red","blue"),
		#xlim=c(1,ntakens), ylim=c(1,ntakens),
		useRaster=TRUE,  #? is this safe??
		main=main
		)
 #	)
 
}



geyseradj <- geyser 

attach(geyseradj)

takens.duration4 <- buildTakens( time.series=geyseradj$duration[-1], embedding.dim=4, time.lag=1)
takens.waiting4 <- buildTakens( time.series=geyseradj$waiting[-1], embedding.dim=4, time.lag=1)

durationneighs4<- findAllNeighbours(takens.duration4, radius=2.0)

waitingneighs4<-findAllNeighbours(takens.waiting4, radius=20.0)
local.recurrencePlotAux(durationneighs4)
local.recurrencePlotAux(waitingneighs4)

local.recurrencePlot1(durationneighs4)
local.recurrencePlot1(waitingneighs4)

local.recurrencePlot2(durationneighs4, waitingneighs4)



#
# covariance 
# see \cite{marwan2002nonlinear}
# CR[i,j] = \Theta( \epsilon - \norm( xv[i] - yv[j] ))

# Brute force

# signed distances may be used for experiments.
maxdist <- function (x){ max(abs(x))} # works on delta
cordist <- function (x, y){ suppressWarnings(cor(x,y))} # using signed. Warnings for zero #
# variances are suppressed

# epsilon/radius and heaviside not use here - may be added in image rendering.

# propagate names from takens

CR0 <- function (xtakens, ytakens, sdist= maxdist) {
	xl <- nrow(xtakens); yl <- nrow(ytakens)
	xname <- deparse(substitute(xtakens))
	yname <- deparse(substitute(ytakens))
	cr <- matrix(nrow=xl, ncol=yl)
	for (i in 1:xl) 
	 for (j in 1:yl)
	{
	cr[i,j] <- sdist(xtakens[i,]- ytakens[j,])
	}
	return(cr)
}


CR2 <- function (xtakens, ytakens, cdist= cordist) {
	xl <- nrow(xtakens); yl <- nrow(ytakens)
	xname <- deparse(substitute(xtakens))
	yname <- deparse(substitute(ytakens))
	cr <- matrix(nrow=xl, ncol=yl)
	for (i in 1:xl) 
	 for (j in 1:yl)
	{
	cr[i,j] <- cdist(xtakens[i,], ytakens[j,])
	}
	return(cr)
}


## for experiments only. do not copy large matrices
crossrecurrencePlotFromMatrix <- function(neighs.matrix,
									zlim= range(neighs.matrix, na.rm=TRUE), 
									main="Cross Recurrence plot",
                                  xlab="x Takens vector's index",
                                  ylab="y Takens vector's index",...){
  # need a print because it is (possibly) a trellis object!!
  rec.plot = image(neighs.matrix,	
  					zlim= zlim,
  					x = 1: ncol(neighs.matrix),
  					y = 1: nrow(neighs.matrix),
                   main = main, xlab = xlab, ylab = ylab, 
                   ...)
  print(rec.plot)
  rec.plot
}

# raw data may give a poor impression. Adjust e.g. for scale and location
cr4 <- CR0(takens.duration4,takens.waiting4)
cr4C <- CR2(takens.duration4,takens.waiting4)
image(cr4)
# neighs.matrix <- cr4;range(cr4) #  70.5500 107.1667


# a max distance
crossrecurrencePlotFromMatrix(cr4, 
main="Cross Recurrence plot\nmax",
xlab="duration4 index", ylab="waiting4 index")  # ugly heat matrix

crossrecurrencePlotFromMatrix(cr4, zlim=c(85,108), col=grey((1:10)/10),
main="Cross Recurrence plot\nmax",
xlab="duration4 index", ylab="waiting4 index") 
# near conventional bw, 
# introducing radius/cut by zlim

# a correlation distance
crossrecurrencePlotFromMatrix(cr4C,
main="Cross Recurrence plot\n cor",
xlab="duration4 index", ylab="waiting4 index")  # ugly heat matrix

quantile(cr4C, na.rm=TRUE)
crossrecurrencePlotFromMatrix(cr4C, zlim=c(-0.7,0.7), col=grey((1:10)/10),
main="Cross Recurrence plot\n cor",
xlab="duration4 index", ylab="waiting4 index")
# near conventional 
# introducing radius/cut by zlim
