wsci <-
function(mat,meth=0,interval=95,boots=1000,diff=0,...) 
{warnst <- options("warn")$warn
 options(warn = -1)
 n <- nrow(mat);  k <- ncol(mat); gm <- sum(mat)/(n*k) 
 cilimits <- c((1-interval/100)/2,.5+interval/200) 
 forgraph <- {};  adjvalue <- {} 
 for (i in 1:n) 
  {adjvalue <- c(adjvalue, gm - sum(mat[i,]/k))} 
 for (j in 1:k) 
  {cols <- {} 
   for (i in 1:n) 
    {cols <- c(cols, mat[i,j] + adjvalue[i])} 
   if (meth==0) 
    {x <- c(mean(cols),mean(cols)+qt(cilimits,n-1)*sd(cols)/sqrt(n))
     forgraph <- cbind(forgraph,x)}
    if (meth==1) 
    {b <- boot(cols,function(x,i) mean(x[i]),boots) 
     x <- c(mean(cols),boot.ci(b,conf=interval/100)$bca[c(4,5)])
     forgraph <- cbind(forgraph,x) } 
  } 
   if (meth==2)
    {values <- c(as.matrix(mat))
     cond <- as.factor(rep(1:k,each=n))
     part <- as.factor(rep(1:n,k))
     x <- lm(values~cond*part)
     adjvalue <- sqrt(anova(x)[3,3]/n)*qt(.5*(1-interval/100),(n-1)*(k-1),lower.tail=FALSE)
     forgraph <- rbind(mean(mat),mean(mat)-adjvalue,mean(mat)+adjvalue)
    }
 dimnames(forgraph) <- list(c("mean", "lower", "upper"),names(mat)) 
 if (diff==0){
   errbar(names(mat),forgraph[1,],forgraph[3,],forgraph[2,],ylab="Means and 95% WSCIs")}
 if (diff==1){
     barwidth <- abs(qt(cilimits,n-1)*sd(mat)/sqrt(n))
     fbs <- t(cbind(mean(mat),mean(mat)+barwidth,mean(mat)-barwidth))
     f2 <- cbind(forgraph,fbs) 
     errbar(rep(names(mat),2),f2[1,],f2[2,],f2[3,],ylab="Means and 95% WSCIs",
       Type=rep(c(1,2),each=k),ylim=c(min(f2),max(f2)))
     title("Means and 95% BSCIs",cex.main=.8,line=3)
     dimnames(fbs) <- dimnames(forgraph)
     print(fbs)
   } 
options(warn = warnst)
return(forgraph) 
}

