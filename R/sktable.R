sktable <-
function(varx, reps = 2000, bootm = TRUE, ...){
 if (bootm){
 sktable <- matrix(nrow=6,ncol=6)
 dimnames(sktable)[[1]] <- list("estimate", "trad SE", "w/mom SE","boot SE","lb BCa","ub BCa")
 g1boot <- boot(varx,function(x,i) g1(x[i]),R=reps)
 gg1boot <- boot(varx,function(x,i) gg1(x[i]),R=reps)
 b1boot <- boot(varx,function(x,i) b1(x[i]),R=reps)
 g2boot <- boot(varx,function(x,i) g2(x[i]),R=reps)
 gg2boot <- boot(varx,function(x,i) gg2(x[i]),R=reps)
 b2boot <- boot(varx,function(x,i) b2(x[i]),R=reps)
  sktable[,1] <- c(g1(varx),g1se(varx),g1se2(varx),
  sd(g1boot$t,na.rm=TRUE),boot.ci(g1boot,type="bca")$bca[4:5])
 sktable[,2] <- c(gg1(varx),gg1se(varx),gg1se2(varx),
  sd(gg1boot$t,na.rm=TRUE),boot.ci(gg1boot,type="bca")$bca[4:5])
 sktable[,3] <- c(b1(varx), b1se(varx),b1se2(varx),
  sd(b1boot$t,na.rm=TRUE),boot.ci(b1boot,type="bca")$bca[4:5])
  sktable[,4] <- c(g2(varx), g2se(varx),g2se2(varx),
   sd(g2boot$t,na.rm=TRUE),boot.ci(g2boot,type="bca")$bca[4:5])
  sktable[,5] <- c(gg2(varx), gg2se(varx),gg2se2(varx),
  sd(gg2boot$t,na.rm=TRUE),boot.ci(gg2boot,type="bca")$bca[4:5])
  sktable[,6] <- c(b2(varx), b2se(varx),b2se2(varx),
  sd(b2boot$t,na.rm=TRUE),boot.ci(b2boot,type="bca")$bca[4:5])}
 if (bootm == FALSE){
 sktable <- matrix(nrow=3,ncol=6)
 dimnames(sktable)[[1]] <- list("estimate", "trad SE", "w/mom SE")
 sktable[,1] <- c(g1(varx),g1se(varx),g1se2(varx))
 sktable[,2] <- c(gg1(varx),gg1se(varx),gg1se2(varx))
 sktable[,3] <- c(b1(varx), b1se(varx),b1se2(varx))
  sktable[,4] <- c(g2(varx), g2se(varx),g2se2(varx))
  sktable[,5] <- c(gg2(varx), gg2se(varx),gg2se2(varx))
  sktable[,6] <- c(b2(varx), b2se(varx),b2se2(varx))}
dimnames(sktable)[[2]] <- list("g1","G1","b1","g2","G2","b2")
return(sktable)}

