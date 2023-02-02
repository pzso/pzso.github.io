## All�l gyakoris�g v�ltoz�sa szelekci� �s drift hat�s�ra, szimul�ci�
## Egy lokusz - k�t all�l modell, diszkr�t dinamika, random p�rv�laszt�ssal
## Drift: binomi�lis modell az all�l gyakoris�gra, a sztochasztikus komponens
## Szelekci�: determinisztikus komponens (a binomi�lis eloszl�s p param�tere)

n<-50      # G�nk�szlet m�rete (diploid popul�ci� m�rete n/2)
s<-0.15    # Szelekci�s koefficiens
p0<-0.2    # Kezdeti all�lgyakoris�g
h<-0       # szelekci� a heterozig�t�ra (s�lyoz�s)
maxt<-100  # gener�ci�k sz�ma
nsim<-50   # ism�tl�sek sz�ma

#---- All�l gyakoris�g v�ltoz�sa szelekci� hat�s�ra
dp<-function(p,s=0,h=0) {
 s*p*(1-p)*(h*(1-2*(1-p))+1-p)/(1-2*p*(1-p)*h*s-s*(1-p)^2)
};

#---- Dinamika
p<-matrix(rep(p0,nsim*maxt),ncol=nsim)
for(i in 1:nsim) 
 for(j in 2:maxt) 
   p[j,i] <- rbinom(1,n,p[j-1,i]+dp(p[j-1,i],s,h))/n

#---- �br�zol�s
matplot(p,type="l",lty=1,col="gray",xlab="Gener�ci�",ylab="All�l gyakoris�g",ylim=c(0,1))
matlines(rowMeans(p),col="red",lty=2,lwd=4) #az �tlag

hist(p[maxt,],xlab="All�l gyakoris�g",ylab="Gyakoris�g",
  main= paste("All�l gyakoris�g eloszl�sa, t = ",maxt, ", n = ",n,sep=""))
