sk <-
function(varx,table=TRUE,SW = TRUE, KS=FALSE,rough=FALSE, ...){
  if (table) print(sktable(varx))
  if (SW) print(shapiro.test(varx))
  if (KS) print(ks.test(varx,"pnorm"))
  if (rough){
    print(paste("Rough skewness SE =",format(sk_approx(varx),digits=4)))
    print(paste("Rough kurtosis SE =",format(k_approx(varx),digits=4)))}
    invisible()}

