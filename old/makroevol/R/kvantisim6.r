##---- Kvantitatív jellegek, 6. szimuláció: környezeti hatás ----##
## Diszkrét, átfedés nélküli generáció                           ##
## Diploid populáció                                             ##
## Függetlenül szegregálódó lokuszok                             ##
## Paraméterek:                                                  ##
##  n: populáció mérete (hím+nõstény, azonos genotípus eloszlás) ##
##  m: függetlenül szegregálódó lokuszok száma ("kromoszómák")   ##
##     lokuszonként két allél a populációban, '+' és '-'         ##
##     fenotípus érték a '+' allélok száma (0, 1, ..., 2m)       ##
##  np: szaporodó egyedek száma,                                 ##
##      ha az egyedszámmal azonos: panmixis                      ##
##      ha kisebb: küszöbérték (irányító) szelekció,             ##
##       csak az np darab legnagyobb x értékû szaporodik         ##
##      ha kisebb és negatív: küszöbérték (irányító) szelekció,  ##
##       csak az np darab legkisebb x értékû szaporodik          ##
##      wprob használata esetén a kiválasztott egyedszám         ##
##  wprob: karakter állapotokhoz tartozó túlélés (súlyozás), a   ##
##      kiválasztási esély (de lásd ?sample prob paraméterét)    ##
##      a karakter állapot (x értéke) függvényében               ##
##  nm: helyettesítõ egyedek száma a fúzió során (egy generáció) ##
##      genotípusuk a másik szülõ populációból vett random       ##
##      minta (helyettesítés véletlenszerû, szelekció elõtt)     ##
##  sig2: környezeti varianca (normális eloszlás, átlag a        ##
##      genotípus érték), a karakter érték valószínûségét        ##
##      határozza meg                                            ##
##  maxt: generációk száma                                       ##
##  tdiv: 1:maxt felbontása (pl. csak szelekció)                 ##
## Változók:                                                     ##
##  genot: az egyedek a lista elemei, most a genotípus mátrix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
##  genotv: genotípus értékek (genot sorrendjében)               ##
##  x: az egyedek karakter értékei (genot sorrendjében)          ##
##  record: lista, x tárolása generációként az ábrázoláshoz      ##
## Szimuláció lépései:                                           ##
##  1. inicializálás: P populáció létrehozása (genot, x)         ##
##  2. szimuláció (generációk)                                   ##
##  3. eredmény ábrázolása                                       ##
## Megjegyzés: lásd kvantisim1.r                                 ##
##---------------------------------------------------------------##

m <- 10           #lokuszok száma
n <- 1000         #populáció egyedszáma
maxt <- 50        #generációk száma az F2 generációtól
np <- 750         #szülõk, egyedszám (np <= n) 
sig2 <- 0.1       #környezeti variancia (átlag: genotípus érték) 

genot<-vector("list",n) #egyed: a genotípus lista egy eleme
record<-vector("list",maxt) 
x<-vector("numeric",n)

source("kvantifunct.r") #függvények betöltése

#-------- Küszöbérték (Irányító) szelekció
dsel<-function(agenot,anp,ax) {
 parent<-agenot[order(sign(anp)*ax)[(n-abs(anp)+1):n]]
 for(i in 1:n) agenot[[i]]<-panmixia(parent,1:abs(anp))
 return(agenot)
}

#-------- Karakter érték környezeti zajjal
charval2<-function(a,sig=sqrt(sig2)) {rnorm(1,a,sig)} #genotípus értékbõl

#--- Inicializálás: random populáció összetétel
genot<-randompop(n,FALSE)
genotv<-sapply(genot,charval)
for(i in 1:n) x[i]<-charval2(genotv[i])
record[[1]]<-x
#tapply(x,genotv,mean) #karakter átlag
#hist(x)

#--- További generációk: Szelekció
for(ss in 2:maxt) {
 genot<-dsel(genot,np,x);
 genotv<-sapply(genot,charval)
 for(i in 1:n) x[i]<-charval2(genotv[i])
 record[[ss]]<-x
}

#--- Ábrázolás
op<-par(mfrow=c(2,1),mar=c(4,4,3,2)+0.1)
hist(x,ylab="Gyakoriság",xlab="Karakter",main="Karakter eloszlás (szelekció után)")
mean1<-sapply(record,mean)
time<-1:maxt
plot(time,mean1[time],type="l",lty=1,lwd=2,
  ylab="Karakter átlag",xlab="Idõ (generációk száma)",main="Karakter átlag változása")
par(op)
