##---- Kvantitatív jellegek, 1. szimuláció: panmixis ------------##
## Diszkrét, átfedés nélküli generáció                           ##
## Diploid populáció                                             ##
## Függetlenül szegregálódó lokuszok                             ##
## Paraméterek:                                                  ##
##  n: populáció mérete (hím+nõstény, azonos genotípus eloszlás) ##
##     bármely egyed azonos eséllyel szaporodhat                 ##
##  m: függetlenül szegregálódó lokuszok száma ("kromoszómák")   ##
##     lokuszonként két allél a populációban, '+' és '-'         ##
##     fenotípus érték a '+' allélok száma (0, 1, ..., 2m)       ##
##  maxt: generációk száma F2-tõl                                ##
## Változók:                                                     ##
##  genot: az egyedek a lista elemei, most a genotípus mátrix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
##  x: az egyedek karakter értékei (genot sorrendjében)          ##
##  record: lista, x tárolása generációként az ábrázoláshoz      ##
## Szaporodás példa (4. és 5. egyed):                            ##
##  print.genot(reprod(genot[[4]],genot[[5]]))                   ##
##  lásd még a kvantifunct.r végén a példákat                    ##
## Szimuláció lépései:                                           ##
##  1. inicializálás: P és F1 populáció létrehozása (genot, x)   ##
##  2. szimuláció (generációk)                                   ##
##  3. eredmény ábrázolása                                       ##
## Megjegyzés: ismételhetõ futtatásokhoz lásd ?set.seed          ##
##---------------------------------------------------------------##

m<-10    #lokuszok száma
n<-1000  #populáció egyedszáma
maxt<-30 #generációk száma az F2 generációtól

genot<-vector("list",n) #egyed: a genotípus lista egy eleme
record<-vector("list",maxt+2) #record[[1]] a P, record[[2]] az F1 stb

source("kvantifunct.r") #függvények betöltése

#-------- Inicializálás, P generáció:
#-- Random populáció összetételhez: genot<-randompop(n) 
#-- Most két eltérõ eloszlású kiindulási populáció, méretük:n/2
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

#--- További generációk (1 populáció)
# véletlenszerû a párválasztás minden generációban
for(ss in 1:maxt) {
 parent<-genot
 for(i in 1:n) genot[[i]]<-panmixia(parent,1:n)
 x<-sapply(genot,charval)
 record[[ss+2]]<-x
}

#--- Ábrázoljuk:
plot.chardistr2(record[-1],FALSE)
