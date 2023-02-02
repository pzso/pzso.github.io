##---- Kvantitatív jellegek, 4. szimuláció: izoláció és fúzió ---##
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
##  maxt: generációk száma F2-tõl                                ##
##  tdiv: 1:maxt felbontása (izoláció, majd fúzió)               ##
## Változók:                                                     ##
##  genot: az egyedek a lista elemei, most a genotípus mátrix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
##  x: az egyedek karakter értékei (genot sorrendjében)          ##
##  record: lista, x tárolása generációként az ábrázoláshoz      ##
## Szimuláció lépései:                                           ##
##  1. inicializálás: P és F1 populáció létrehozása (genot, x)   ##
##  2. szimuláció (generációk)                                   ##
##  3. eredmény ábrázolása                                       ##
## Megjegyzés: lásd kvantisim1.r                                 ##
##---------------------------------------------------------------##

m <- 10           #lokuszok száma
n <- 1000         #populáció egyedszáma
maxt <- 50        #generációk száma az F2 generációtól
tdiv <- c(20,33)  #izoláció és fúzió (<maxt, generáció sorszám)
#tdiv <- c(20,maxt) #izoláció fúzió nélkül
np <- 750         #szülõk, egyedszám (np <= n) 
np2 <- 750        #szülõk a második populációban
nm <- 500         #helyettesitettek száma a fúzió során
wprob <-dnorm(    #fitnesz (túlélési valószinûség, pl stabilizáló szelekcióra,
 0:(2*m),mean=10,sd=2) #a karakter értékekhez tartozó súly 
wprob2 <-dnorm(   #fitnesz a 2. populációra
 0:(2*m),mean=17,sd=2) 

genot<-vector("list",n) #egyed: a genotípus lista egy eleme
record<-vector("list",maxt+2) #record[[1]] a P, record[[2]] az F1 stb

source("kvantifunct.r") #függvények betöltése

#-------- Küszöbérték (Irányító) szelekció
dsel<-function(agenot,anp,ax) {
 parent<-agenot[order(sign(anp)*ax)[(n-abs(anp)+1):n]]
 for(i in 1:n) agenot[[i]]<-panmixia(parent,1:abs(anp))
 return(agenot)
}

#-------- Szelekció karakter állapottól függõ súlyozással
wsel<-function(agenot,anp,ax,w) {
 parent<-agenot[sample(n,abs(np),replace=FALSE,prob=w[ax])]
 for(i in 1:n) agenot[[i]]<-panmixia(parent,1:abs(anp))
 return(agenot)
}

#-------- Inicializálás, P generáció
#-- Random populáció összetételhez: genot<-randompop(n) 
#-- Két eltérõ eloszlású kiindulási populáció, mérete:n/2
# 1:nh az elsõ, (nh+1):n a második populáció
# az utódok már egy populációt alkotnak
nh<-n/2
for(i in 1:nh) 
 genot[[i]]<-matrix(sample(c("+","-"),2*m,replace=TRUE,prob=c(.25,.75)),nrow=2)
for(i in (nh+1):n) 
 genot[[i]]<-matrix(sample(c("+","-"),2*m,replace=TRUE,prob=c(.75,.25)),nrow=2)
x<-sapply(genot,charval)
#plot.chardistr(list(x[1:nh],x[(nh+1):n]),col=c("red","blue"))
record[[1]]<-x

#--- F1 generáció
parent<-genot; 
for(i in 1:n) genot[[i]]<-matingtype(parent,1:nh,(nh+1):n)
x<-sapply(genot,charval)
#plot.chardistr(list(record[[1]][1:nh],record[[1]][(nh+1):n],x),col=c("red","blue","green"),cex.names=0.9)
record[[2]]<-x

#--- További generációk (1 populáció)

#--- 1. Egy populáció
for(ss in 1:tdiv[1]) {
 genot<-wsel(genot,np,x,wprob)
 x<-sapply(genot,charval)
 record[[ss+2]]<-x
}

#--- 2. teljes izoláció, két populáció
genot2<-genot                                       #2. populáció létrehozása
record2<-record
x2<-x
for(ss in (tdiv[1]+1):tdiv[2]) {
 genot<-wsel(genot,np,x,wprob); x<-sapply(genot,charval); record[[ss+2]]<-x
 genot2<-wsel(genot2,np2,x2,wprob2); x2<-sapply(genot2,charval); record2[[ss+2]]<-x2
}

#--- 3. izoláció megszûnik, fúzió
if(tdiv[2]<maxt) {
 immigr<-sample(n,nm,FALSE)                          
 for(i in 1:nm) 
  genot[[immigr[i]]]<-genot2[[immigr[i]]]
 x<-sapply(genot,charval)
 for(ss in (tdiv[2]+1):maxt) {
  genot<-wsel(genot,np,x,wprob)
  x<-sapply(genot,charval)
  record[[ss+2]]<-x
  record2[[ss+2]]<-x
 }
}

#--- Átlagok ábrázolása
mean1<-sapply(record,mean)
mean2<-sapply(record2,mean)
time<-7:(maxt+1) #eleje burnin, eldobva...
matplot(time,cbind(mean2[time],mean1[time]),type="l",lty=1,lwd=2,col=c("blue","black"),
  ylab="Karakter átlag",xlab="Idõ (generációk száma)")
