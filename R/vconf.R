vconf <-
function(ctab,clevel=.95){
 if (clevel < .5) 
   warning("With low confidence values interval may not include sample value")
 # This function ignores the warning if any E_ij < 5
 suppressWarnings(chsq <- chisq.test(ctab,correct=FALSE))
 df <- chsq$parameter
 stat <- chsq$statistic
 N <- sum(ctab)
 cls <- conf.limits.nc.chisq(stat,clevel,df)
 K <- min(dim(ctab)[1]-1,dim(ctab)[2]-1)
 V <- sqrt(stat/(N*K)); names(V) <- "Cramer's V"
 Vlb <- sqrt((cls$Lower.Limit+df)/(N*K)); names(Vlb) <- "lower bound"
 Vub <- sqrt((cls$Upper.Limit+df)/(N*K)); names(Vub) <- "upper bound"
 outv <- list(V,Vlb,Vub)
 names(outv) <- c("V","Vlb","Vub")
 return(outv)}

