g1se2 <-
function(x) {n <- length(x)
 beta1 <- mr(x,n,3)^2/mr(x,n,2)^3 # sqrt(beta1) = g1
 beta2 <- mr(x,n,4)/mr(x,n,2)^2 # = g2 + 3
 sqrt((mr(x,n,6)/mr(x,n,2)^4 - 6*beta2 + 9 + (beta1/4)*(9*beta2+35)
    - 3*mr(x,n,5)*mr(x,n,3)/mr(x,n,2)^4)/n)}

