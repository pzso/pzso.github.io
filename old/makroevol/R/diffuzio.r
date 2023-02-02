###############################################
## Forrás:                                   ##
## Soetaert and Herman (2008)                ##
## A practical guide to ecological modelling ##
## 5. fejezet                                ##
## Dispersion_reaction1D.r aéapján           ##
## 1D reakció-diffúziós egyenlet alkalmazása ##
##  N(x,t): denzitás változása               ##
##    (analitikus megoldás ábrázolása)       ##
###############################################

Ds  <- 1    # diffúziós koefficiens
N0  <- 1    # kezdeti egyedszám
k   <- 0.05 # növekedési ráta

grow1D <- outer(xx<-seq(-5,5,length=50),tt<-seq(0.1,5,by=0.025),
 FUN = function (x,tt) N0/(2*sqrt(pi*Ds*tt))*exp(k*tt-x^2/(4*Ds*tt)))

persp(xx,tt,z=grow1D,theta=150,box=TRUE,axes=TRUE,col="gray",
xlab="x",ylab="t",zlab="N",border=NA,shade=0.75)  

#Numerikus eljárásokhoz lásd deSolve csomag
#animáció példa: Soetaert and Herman 6. fejezet, Aphids.r
