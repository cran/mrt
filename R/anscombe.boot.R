anscombe.boot <-
function(x,reps=2000, printtest=FALSE,clevel=.95,method="bca",...){
 if (length(x) < 5) 
   warning("The n is probably too small to work", immediate. = TRUE)
 anscstat <- function(x) anscombe.test(x)$statistic                                              
 if (printtest){
 print("This is Pearson's g2 statistic and is 3 higher than in sktable,") 
 print("which is Fisher's adjustment to make the Normal have g2=0.")
 print(moments:::anscombe.test(x))}
 ansboot <- boot(x, function(x,i) anscstat(x[i]),R=reps)
 if (method=="bca") 
 bootout <- c(ansboot$t0,
   boot.ci(ansboot,type="bca",index=2,conf=clevel)$bca[4:5])
 else                                                        
 bootout <- c(ansboot$t0,
   boot.ci(ansboot,type="perc",index=2,conf=clevel)$perc[4:5])
 names(bootout) <- c("Kurtosis", "z", "lb of z","ub of z")
 return(bootout)
}

