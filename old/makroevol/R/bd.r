###############################################
## Forr�s:                                   ##
## branching random diffusion                ##
## Liam Revell 2011 alapj�n                  ##
## l�sd phylotools R csomag                  ##
###############################################

sig2 <- 1      # mint�zott norm�lis eloszl�s varianci�ja (a folyamat) 
b <- 0.0023    # sz�let�si r�ta (az el�gaz�si val�sz�n�s�g)
maxt <- 1000   # gener�ci�k sz�ma 

n<-1; y<-matrix(0,1,n) # kiindul�s: egy folyamat (n) 0 �llapottal (y)

for(i in 2:maxt){
 y<-rbind(y,y[i-1,])
 for(j in 1:n){
  if(b>runif(n=1)){    # el�gaz�s? 
    y<-cbind(y,y[,j])  # �j folyamat
    n<-n+1
    y[i,n]<-y[i,j]+rnorm(n=1,sd=sqrt(sig2)) 
   }
   y[i,j]<-y[i,j]+rnorm(n=1,sd=sqrt(sig2))
  }
}

time<-seq(1,maxt,1)    # sim�t�s, pl. minden 5. gener�ci�
matplot(time,y[time,],xlab="Id�",ylab="Karakter",type="l",col="black",lty=1)
