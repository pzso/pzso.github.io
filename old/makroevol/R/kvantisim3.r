##---- Kvantitatív jellegek, 3. szimuláció: szelekció-migráció --##
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
##  nm: bevándorló (helyettesítõ) egyedek száma generációnként   ##
##      genotípusuk a hátrányos szülõ populációból vett random   ##
##      minta (helyettesítés véletlenszerû, szelekció elõtt)     ##
##  maxt: generációk száma F2-tõl                                ##
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

m<-10    #lokuszok száma
n<-1000  #populáció egyedszáma
maxt<-20 #generációk száma az F2 generációtól
np<-400  #szülõk, egyedszám (np <= n) 
nm<-300  #helyettesitett egyedek száma

genot<-vector("list",n) #egyed: a genotípus lista egy eleme
record<-vector("list",maxt+2) #record[[1]] a P, record[[2]] az F1 stb

source("kvantifunct.r") # függvények betöltése

#-------- Inicializálás, P generáció:
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
plot.chardistr(list(x[1:nh],x[(nh+1):n]),col=c("red","blue"))
record[[1]]<-x

#--- F1 generáció
parent<-genot; 
for(i in 1:n) genot[[i]]<-matingtype(parent,1:nh,(nh+1):n)
x<-sapply(genot,charval)
plot.chardistr(list(record[[1]][1:nh],record[[1]][(nh+1):n],x),col=c("red","blue","green"),cex.names=0.9)
record[[2]]<-x

#--- További generációk (1 populáció): elõbb helyettesítés majd szelekció
for(ss in 1:maxt) {
 immigr<-sample(n,nm,FALSE)
 for(i in 1:nm) 
  genot[[immigr[i]]]<-matrix(sample(c("+","-"),2*m,replace=TRUE,prob=c(.25,.75)),nrow=2)
 parent<-genot[order(x)[(n-np+1):n]]
 for(i in 1:n) genot[[i]]<-panmixia(parent,1:np)
 x<-sapply(genot,charval)
 record[[ss+2]]<-x
}

plot.chardistr2(record[-1],F)

