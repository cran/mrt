g2se2 <-
function(x) {n <- length(x)
 beta1 <- mr(x,n,3)^2/mr(x,n,2)^3 # sqrt(beta1) = g1
 beta2 <- mr(x,n,4)/mr(x,n,2)^2 # = g2 + 3
 sqrt((mr(x,n,8)/mr(x,n,2)^4 - 4*mr(x,n,6)*mr(x,n,4)/mr(x,n,2)^5 +
   4*beta2^3 - beta2^2+16*beta2*beta1 - 8*mr(x,n,5)*mr(x,n,3)/mr(x,n,2)^4
   + 16+beta1)/(n))}

