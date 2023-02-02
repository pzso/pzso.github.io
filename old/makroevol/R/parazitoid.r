###############################################
## Forr�s:                                   ##
## Soetaert and Herman (2009)                ##
## A practical guide to ecological modelling ##
## Chapter 9. parasitoid.r                   ##
## l�sd ecolMod csomag, demo(chap9)          ##
###############################################

rH <- 2.82        # n�veked�si r�ta
tS <- 100         # keres�si id�
tH <- 1           # kezel�si id�
A  <- tS/tH       # attack rate
ks <- 20          # 1/tH*a
maxt <-100        # gener�ci�k sz�ma
burnin <- 100     # bemeleg�t�s, eldobjuk az elej�r�l
P_H <- c(0.5,0.5) # kezdeti �llapot (vektor), parazitoid el�l

Parasite <- function(P_H,ks){                # k�vetkez� gener�ci� (diszkr�t logisztikus modell)
 P <- P_H[1]
 H <- P_H[2]
 f <- A*P/(ks+H)
 c(H*(1-exp(-f)),H*exp(rH*(1-H)-f))
}

#----------- MAIN
record <- matrix(nrow=maxt,ncol=2)           # grafikus �br�zol�shoz, parazita �s gazda oszlopok
if(burnin>0) 
  for(i in 1:burnin) P_H <-Parasite(P_H,ks)  # bemeleg�t�s, eldobjuk
for (i in 1:maxt) {
  P_H <- Parasite(P_H,ks);                   # dinamika, diszkr�t gener�ci�k 
  record[i,]<-P_H
}

#----------- �BR�ZOL�S
plot(record[1:i,1],type="l",xlim=c(1,maxt),ylim=range(record),lwd=2,
  xlab="Id� (gener�ci�)",ylab="Popul�ci� m�rete",col="red")
lines(record[1:i,2],lty=2,col="blue")
legend("topright",c("Parazitoid","Gazda"),lty=c(1,2),lwd=c(2,1),col=c("red","blue"))
            