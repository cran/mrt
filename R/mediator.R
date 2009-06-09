`mediator` <-
function(x,y,m, ...){                       
  # Put in experimental variable x, outcome variable y, then mediator m.
  # Some of this is written so that it works in a mediator
  # function for more complex problems.
  reg0 <- lm(y~x)
  reg1 <- lm(m~x)
  reg2 <- lm(y~m+x)
  c <- summary(reg0)$coefficients[2,1]
  sc <- summary(reg0)$coefficients[2,2]
  a <- summary(reg1)$coefficients[2,1]
  sa <- summary(reg1)$coefficients[2,2]
  b <- summary(reg2)$coefficients[2,1]
  sb <- summary(reg2)$coefficients[2,2]
  cp <- summary(reg2)$coefficients[3,1]
  scp <- summary(reg2)$coefficients[3,2]
  sobel <- (a*b)/sqrt(b^2*sa^2 + a^2*sb^2 + sa^2*sb^2)
  psobel <- format(2*(1-pnorm(abs(sobel))),digits=2,nsmall=2)
  plot(c(0,100),c(0,110),col="white",ann=FALSE,tck=0,col.axis="white")
  rect(c(10,10,70,70,40),c(10,50,10,50,80),c(30,30,90,90,60),c(30,70,30,70,100))
  arrows(c(30,30,20,60),c(20,60,70,90),c(70,70,40,80),c(20,60,90,70),length=.15)
  text(c(20,20,80,80,50),c(20,60,20,60,90),c("X","X","Y","Y","M"),cex=2)
  text(30,80,paste(format(a,digits=2,nsmall=2)),pos=2,cex=1.3)
  text(70,80,paste(format(b,digits=2,nsmall=2)),pos=4,cex=1.3)
  text(50,20,paste(format(c,digits=2,nsmall=2)),pos=3,cex=1.3)
  text(50,60,paste(format(cp,digits=2,nsmall=2)),pos=3,cex=1.3)
  text(50,2,paste("Sobel z = ",format(sobel,digits=2,nsmall=2),"; p = ",psobel),cex=1.3)
  print(paste("Sobel z =", format(sobel,digits=3,nsmall=2),", p =", psobel))
  invisible(c(sobel,psobel)) 
}

