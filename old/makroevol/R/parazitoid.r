###############################################
## Forrás:                                   ##
## Soetaert and Herman (2009)                ##
## A practical guide to ecological modelling ##
## Chapter 9. parasitoid.r                   ##
## lásd ecolMod csomag, demo(chap9)          ##
###############################################

rH <- 2.82        # növekedési ráta
tS <- 100         # keresési idõ
tH <- 1           # kezelési idõ
A  <- tS/tH       # attack rate
ks <- 20          # 1/tH*a
maxt <-100        # generációk száma
burnin <- 100     # bemelegítés, eldobjuk az elejérõl
P_H <- c(0.5,0.5) # kezdeti állapot (vektor), parazitoid elõl

Parasite <- function(P_H,ks){                # következõ generáció (diszkrét logisztikus modell)
 P <- P_H[1]
 H <- P_H[2]
 f <- A*P/(ks+H)
 c(H*(1-exp(-f)),H*exp(rH*(1-H)-f))
}

#----------- MAIN
record <- matrix(nrow=maxt,ncol=2)           # grafikus ábrázoláshoz, parazita és gazda oszlopok
if(burnin>0) 
  for(i in 1:burnin) P_H <-Parasite(P_H,ks)  # bemelegítés, eldobjuk
for (i in 1:maxt) {
  P_H <- Parasite(P_H,ks);                   # dinamika, diszkrét generációk 
  record[i,]<-P_H
}

#----------- ÁBRÁZOLÁS
plot(record[1:i,1],type="l",xlim=c(1,maxt),ylim=range(record),lwd=2,
  xlab="Idõ (generáció)",ylab="Populáció mérete",col="red")
lines(record[1:i,2],lty=2,col="blue")
legend("topright",c("Parazitoid","Gazda"),lty=c(1,2),lwd=c(2,1),col=c("red","blue"))
            