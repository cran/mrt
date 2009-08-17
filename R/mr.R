mr <-
function(x,n,r) {
   mr <- sum((x-mean(x))^r)/n
   if (r == 0) mr <- 1
   if (r == 1) mr <- 0
   return(mr)}

