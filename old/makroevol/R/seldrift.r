## Allél gyakoriság változása szelekció és drift hatására, szimuláció
## Egy lokusz - két allél modell, diszkrét dinamika, random párválasztással
## Drift: binomiális modell az allél gyakoriságra, a sztochasztikus komponens
## Szelekció: determinisztikus komponens (a binomiális eloszlás p paramétere)

n<-50      # Génkészlet mérete (diploid populáció mérete n/2)
s<-0.15    # Szelekciós koefficiens
p0<-0.2    # Kezdeti allélgyakoriság
h<-0       # szelekció a heterozigótára (súlyozás)
maxt<-100  # generációk száma
nsim<-50   # ismétlések száma

#---- Allél gyakoriság változása szelekció hatására
dp<-function(p,s=0,h=0) {
 s*p*(1-p)*(h*(1-2*(1-p))+1-p)/(1-2*p*(1-p)*h*s-s*(1-p)^2)
};

#---- Dinamika
p<-matrix(rep(p0,nsim*maxt),ncol=nsim)
for(i in 1:nsim) 
 for(j in 2:maxt) 
   p[j,i] <- rbinom(1,n,p[j-1,i]+dp(p[j-1,i],s,h))/n

#---- Ábrázolás
matplot(p,type="l",lty=1,col="gray",xlab="Generáció",ylab="Allél gyakoriság",ylim=c(0,1))
matlines(rowMeans(p),col="red",lty=2,lwd=4) #az átlag

hist(p[maxt,],xlab="Allél gyakoriság",ylab="Gyakoriság",
  main= paste("Allél gyakoriság eloszlása, t = ",maxt, ", n = ",n,sep=""))
