###############################################
## Forr�s:                                   ##
## Soetaert and Herman (2008)                ##
## A practical guide to ecological modelling ##
## 5. fejezet                                ##
## Dispersion_reaction1D.r a�apj�n           ##
## 1D reakci�-diff�zi�s egyenlet alkalmaz�sa ##
##  N(x,t): denzit�s v�ltoz�sa               ##
##    (analitikus megold�s �br�zol�sa)       ##
###############################################

Ds  <- 1    # diff�zi�s koefficiens
N0  <- 1    # kezdeti egyedsz�m
k   <- 0.05 # n�veked�si r�ta

grow1D <- outer(xx<-seq(-5,5,length=50),tt<-seq(0.1,5,by=0.025),
 FUN = function (x,tt) N0/(2*sqrt(pi*Ds*tt))*exp(k*tt-x^2/(4*Ds*tt)))

persp(xx,tt,z=grow1D,theta=150,box=TRUE,axes=TRUE,col="gray",
xlab="x",ylab="t",zlab="N",border=NA,shade=0.75)  

#Numerikus elj�r�sokhoz l�sd deSolve csomag
#anim�ci� p�lda: Soetaert and Herman 6. fejezet, Aphids.r
