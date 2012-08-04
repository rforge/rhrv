BoundModwpt <- function(x, wf="la8", n.levels=4, boundary="periodic",targets)
{
 
  N <- length(x); storage.mode(N) <- "integer"
  J <- n.levels
  if(2^J > N) stop("wavelet transform exceeds sample size in modwt")

  dict <- wave.filter(wf)
  L <- dict$length
  storage.mode(L) <- "integer"
  ht <- dict$hpf/sqrt(2)
  storage.mode(ht) <- "double"
  gt <- dict$lpf/sqrt(2)
  storage.mode(gt) <- "double"

  y <- vector("list", sum(2^(1:J)))
  yn <- length(y)
  crystals1 <- rep(1:J, 2^(1:J))
  crystals2 <- unlist(apply(as.matrix(2^(1:J) - 1), 1, seq, from=0))
  names(y) <- paste("w", crystals1, ".", crystals2, sep="")

  W <- V <- numeric(N)
  storage.mode(W) <- storage.mode(V) <- "double"
  for(j in 1:J) {
    index <- 0
    jj <- min((1:yn)[crystals1 == j])
    for(n in 0:(2^j / 2 - 1)) {
        index <- index + 1
        #should filter parent node j-1,n
        if(shouldCompute(c(j-1,n),targets)){
          if(j > 1)
            x <- y[[(1:yn)[crystals1 == j-1][index]]]
          if(n %% 2 == 0) {
            z <- .C("modwt", as.double(x), N, as.integer(j), L, ht, gt,
                    W = W, V = V, PACKAGE = "waveslim")[7:8]
            y[[jj + 2*n + 1]] <- z$W
            y[[jj + 2*n]] <- z$V
          }
          else {
            z <- .C("modwt", as.double(x), N, as.integer(j), L, ht, gt,
                    W = W, V = V, PACKAGE = "waveslim")[7:8]
            y[[jj + 2*n]] <- z$W
            y[[jj + 2*n + 1 ]] <- z$V
        }
      }

    }
  }

  return(y)
}

shouldCompute <- function(node,targets){

 compute=FALSE;
 # node c(0,0) must be computed
 if (nodeEquals(node,c(0,0))){
    compute=TRUE;
 }else{
   numberTargets=length(targets)/2;
   for (n in 1:numberTargets){


          currentTarget=targets[(2*n-1):(2*n)];
          if (  nodeEquals(node,currentTarget)||isFather(node,currentTarget)  )
          {
            compute=TRUE;
            break;
          }
   }
}

 return(compute);
}

isFather <- function(father,pson){
isfather=FALSE;

if (father[1]<pson[1]){
  deltaLevels=pson[1]-father[1];
  # father can generate, at pson level, nodes n1 to n2
  n2=(father[2]+1)*2^deltaLevels-1;
  n1=(father[2])*2^deltaLevels;
  isfather= ((n1<=pson[2])&&(n2>=pson[2]));

}

return (isfather);

}


nodeEquals<-function(n1,n2){
return ((n1[1]==n2[1])&&(n1[2]==n2[2]));

}