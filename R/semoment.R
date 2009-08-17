semoment <-
function(x,r=4){
 n <- length(x)
 semoment <- sqrt((mr(x,n,2*r)-mr(x,n,r)^2+r^2*mr(x,n,2)*mr(x,n,r-1)^2 -
      2*r*mr(x,n,r-1)*mr(x,n,r+1))/n)
 return(semoment)}

