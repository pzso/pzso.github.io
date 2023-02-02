##---- Kvantitat�v jellegek, 1. szimul�ci�: panmixis ------------##
## Diszkr�t, �tfed�s n�lk�li gener�ci�                           ##
## Diploid popul�ci�                                             ##
## F�ggetlen�l szegreg�l�d� lokuszok                             ##
## Param�terek:                                                  ##
##  n: popul�ci� m�rete (h�m+n�st�ny, azonos genot�pus eloszl�s) ##
##     b�rmely egyed azonos es�llyel szaporodhat                 ##
##  m: f�ggetlen�l szegreg�l�d� lokuszok sz�ma ("kromosz�m�k")   ##
##     lokuszonk�nt k�t all�l a popul�ci�ban, '+' �s '-'         ##
##     fenot�pus �rt�k a '+' all�lok sz�ma (0, 1, ..., 2m)       ##
##  maxt: gener�ci�k sz�ma F2-t�l                                ##
## V�ltoz�k:                                                     ##
##  genot: az egyedek a lista elemei, most a genot�pus m�trix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
##  x: az egyedek karakter �rt�kei (genot sorrendj�ben)          ##
##  record: lista, x t�rol�sa gener�ci�k�nt az �br�zol�shoz      ##
## Szaporod�s p�lda (4. �s 5. egyed):                            ##
##  print.genot(reprod(genot[[4]],genot[[5]]))                   ##
##  l�sd m�g a kvantifunct.r v�g�n a p�ld�kat                    ##
## Szimul�ci� l�p�sei:                                           ##
##  1. inicializ�l�s: P �s F1 popul�ci� l�trehoz�sa (genot, x)   ##
##  2. szimul�ci� (gener�ci�k)                                   ##
##  3. eredm�ny �br�zol�sa                                       ##
## Megjegyz�s: ism�telhet� futtat�sokhoz l�sd ?set.seed          ##
##---------------------------------------------------------------##

m<-10    #lokuszok sz�ma
n<-1000  #popul�ci� egyedsz�ma
maxt<-30 #gener�ci�k sz�ma az F2 gener�ci�t�l

genot<-vector("list",n) #egyed: a genot�pus lista egy eleme
record<-vector("list",maxt+2) #record[[1]] a P, record[[2]] az F1 stb

source("kvantifunct.r") #f�ggv�nyek bet�lt�se

#-------- Inicializ�l�s, P gener�ci�:
#-- Random popul�ci� �sszet�telhez: genot<-randompop(n) 
#-- Most k�t elt�r� eloszl�s� kiindul�si popul�ci�, m�ret�k:n/2
# 1:nh az els�, (nh+1):n a m�sodik popul�ci�
# az ut�dok m�r egy popul�ci�t alkotnak
nh<-n/2
for(i in 1:nh) 
 genot[[i]]<-matrix(sample(c("+","-"),2*m,replace=TRUE,prob=c(.25,.75)),nrow=2)
for(i in (nh+1):n) 
 genot[[i]]<-matrix(sample(c("+","-"),2*m,replace=TRUE,prob=c(.75,.25)),nrow=2)
x<-sapply(genot,charval)
plot.chardistr(list(x[1:nh],x[(nh+1):n]),col=c("red","blue"))
record[[1]]<-x

#--- F1 gener�ci�
parent<-genot; 
for(i in 1:n) genot[[i]]<-matingtype(parent,1:nh,(nh+1):n)
x<-sapply(genot,charval)
plot.chardistr(list(record[[1]][1:nh],record[[1]][(nh+1):n],x),col=c("red","blue","green"),cex.names=0.9)
record[[2]]<-x

#--- Tov�bbi gener�ci�k (1 popul�ci�)
# v�letlenszer� a p�rv�laszt�s minden gener�ci�ban
for(ss in 1:maxt) {
 parent<-genot
 for(i in 1:n) genot[[i]]<-panmixia(parent,1:n)
 x<-sapply(genot,charval)
 record[[ss+2]]<-x
}

#--- �br�zoljuk:
plot.chardistr2(record[-1],FALSE)
