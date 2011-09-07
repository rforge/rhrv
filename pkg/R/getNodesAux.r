
getNodesAux=function(f,sampling,error,type,deltaOriginalInterval,relative)
{   # auxliary variables
    found=FALSE;
    i=1;
    bj=0;
    # Iterate until the node is found
    while(found==FALSE)
    {
          # width of the interval
          delta = sampling/(2^(i+1));
          # searching
          for (j in (bj*2):(bj*2+1))
          {
            interval= c(j*delta,(j+1)*delta);
            if (f %in% interval)
            {
                  bj=j;
                  found =  getError(f,interval,type,deltaOriginalInterval,relative)<error;
                  if ((found)||(f!=interval[1]&&f!=interval[2]))
                    break
            }

          }
          if (found==FALSE)
              i=i+1;

    }
    nodes=c(i,j)

}