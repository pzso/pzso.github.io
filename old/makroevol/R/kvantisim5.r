##---- Kvantitatív jellegek, 5. szimuláció: szelekció-génáramlás ##
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
##  maxt: generációk száma                                       ##
##  tdiv: 1:maxt felbontása (pl. csak szelekció)                 ##
## Változók:                                                     ##
##  genot: az egyedek a lista elemei, most a genotípus mátrix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
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
maxt <- 50        #generációk száma
tdiv<-c(9,30)     #génáramlás kezdete
np <- 750         #szülõk, egyedszám (np <= n) 
np2 <- 750        #szülõk a második populációban
nm <- 400         #helyettesitettek száma (cserélt egyed)
wprob <-dnorm(    #fitnesz (túlélési valószinûség, pl stabilizáló szelekcióra,
 0:(2*m),mean=10,sd=1) #a karakter értékekhez tartozó súly 
wprob2 <-dnorm(   #fitnesz a 2. populációra
 0:(2*m),mean=17,sd=1) 

genot<-vector("list",n) #egyed: a genotípus lista egy eleme
record<-vector("list",maxt) 
record2<-vector("list",maxt) 

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

#--- Inicializálás: random populáció összetétel, két azonos méretû populáció 
genot<-randompop(n,FALSE)
x<-sapply(genot,charval)
record[[1]]<-x
genot2<-randompop(n,FALSE)
x2<-sapply(genot2,charval)
record2[[1]]<-x2

#--- További generációk 1. Szelekció mindkét populációban
for(ss in 2:tdiv[1]) {
 genot<-wsel(genot,np,x,wprob); x<-sapply(genot,charval); record[[ss]]<-x
 genot2<-wsel(genot2,np2,x2,wprob2); x2<-sapply(genot2,charval); record2[[ss]]<-x2
}

#--- További generációk 2. Csak génáramlás és panmixis
for(ss in (tdiv[1]+1):tdiv[2]) {
 parent<-genot                  # genot mentése
 immigr<-sample(n,nm,FALSE)     # génáramlás (genotípus csere)
 for(i in 1:nm) {
  genot[[immigr[i]]]<-genot2[[immigr[i]]]
  genot2[[immigr[i]]]<-parent[[immigr[i]]]
 }
 x<-sapply(genot,charval); x2<-sapply(genot2,charval);

 parent<-genot[sample(n,np,FALSE)] # panmixis mindkét populációban
 for(i in 1:n) genot[[i]]<-panmixia(parent,1:np)
 x<-sapply(genot,charval);
 parent<-genot2[sample(n,np2,FALSE)]
 for(i in 1:n) genot2[[i]]<-panmixia(parent,1:np2)
 x2<-sapply(genot2,charval);
 record[[ss]]<-x; record2[[ss]]<-x2
}

#--- További generációk 3. Szelekció és génáramlás
for(ss in (tdiv[2]+1):maxt) {
 parent<-genot                  # genot mentése
 immigr<-sample(n,nm,FALSE)     # génáramlás (genotípus csere)
 for(i in 1:nm) {
  genot[[immigr[i]]]<-genot2[[immigr[i]]]
  genot2[[immigr[i]]]<-parent[[immigr[i]]]
 }
 x<-sapply(genot,charval); x2<-sapply(genot2,charval)

 genot<-wsel(genot,np,x,wprob); x<-sapply(genot,charval)  # szelekció
 genot2<-wsel(genot2,np2,x2,wprob2); x2<-sapply(genot2,charval)
 record[[ss]]<-x; record2[[ss]]<-x2
}

#--- Átlagok ábrázolás
mean1<-sapply(record,mean)
mean2<-sapply(record2,mean)
time<-tdiv[1]:maxt
matplot(time,cbind(mean2[time],mean1[time]),type="l",lty=1,lwd=2,col=c("blue","black"),
  ylab="Karakter átlag",xlab="Idõ (generációk száma)")
