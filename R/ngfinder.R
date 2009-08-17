ngfinder <-
function(nomg,number = 2, right=1, replic = 1000,
  moments=NULL,distmeth = "ks", ...){
n <- dim(nomg)[1]
k <- dim(nomg)[2]
forgroups <- matrix(nrow=n,ncol=replic)
values <- matrix(nrow= trunc(n/number),ncol=replic)
for (i in 1:replic){
  forgroups[,i] <- sample(n)
  for (j in 1:trunc(n/number)){
   x <- number*(j - 1) + 1; y <- number*j
   value <- 0
   for (m in 1:k)
     value <- value + any(nomg[forgroups[x:y,i],m] == right)
   values[j,i] <- value
    }}
if (is.null(moments)){
ksvals <- {}
options(warn=-1)
for (i in 1:replic)
  if (distmeth == "ks")
  ksvals <- c(ksvals,ks.test(values[,i],c(values))$statistic)
  if (distmeth == "pearson")
  ksvals <- c(ksvals,chisq.test(values[,i],c(values))$statistic)
options(warn=0)
best <- which.min(ksvals)}
if (is.numeric(moments)){
  momentmat <- matrix(nrow=moments,ncol=replic)
  for (i in 1:replic){
    for (m in 1:moments){
      x <- values[,i] 
      momentmat[m,i] <- sum(x^m)/length(x)}}
  for (m in 1:moments) 
     momentmat[m,] <- scale(momentmat[m,])    
  best <- which.min(colMeans(abs(momentmat)))
  }
return(list(forgroups=forgroups[,best],values=values[,best]))
}

