###############################################
## Forrás:                                   ##
## branching random diffusion                ##
## Liam Revell 2011 alapján                  ##
## lásd phylotools R csomag                  ##
###############################################

sig2 <- 1      # mintázott normális eloszlás varianciája (a folyamat) 
b <- 0.0023    # születési ráta (az elágazási valószínûség)
maxt <- 1000   # generációk száma 

n<-1; y<-matrix(0,1,n) # kiindulás: egy folyamat (n) 0 állapottal (y)

for(i in 2:maxt){
 y<-rbind(y,y[i-1,])
 for(j in 1:n){
  if(b>runif(n=1)){    # elágazás? 
    y<-cbind(y,y[,j])  # új folyamat
    n<-n+1
    y[i,n]<-y[i,j]+rnorm(n=1,sd=sqrt(sig2)) 
   }
   y[i,j]<-y[i,j]+rnorm(n=1,sd=sqrt(sig2))
  }
}

time<-seq(1,maxt,1)    # simítás, pl. minden 5. generáció
matplot(time,y[time,],xlab="Idõ",ylab="Karakter",type="l",col="black",lty=1)
