##---- Kvantitat�v jellegek, 3. szimul�ci�: szelekci�-migr�ci� --##
## Diszkr�t, �tfed�s n�lk�li gener�ci�                           ##
## Diploid popul�ci�                                             ##
## F�ggetlen�l szegreg�l�d� lokuszok                             ##
## Param�terek:                                                  ##
##  n: popul�ci� m�rete (h�m+n�st�ny, azonos genot�pus eloszl�s) ##
##  m: f�ggetlen�l szegreg�l�d� lokuszok sz�ma ("kromosz�m�k")   ##
##     lokuszonk�nt k�t all�l a popul�ci�ban, '+' �s '-'         ##
##     fenot�pus �rt�k a '+' all�lok sz�ma (0, 1, ..., 2m)       ##
##  np: szaporod� egyedek sz�ma,                                 ##
##      ha az egyedsz�mmal azonos: panmixis                      ##
##      ha kisebb: k�sz�b�rt�k (ir�ny�t�) szelekci�,             ##
##       csak az np darab legnagyobb x �rt�k� szaporodik         ##
##  nm: bev�ndorl� (helyettes�t�) egyedek sz�ma gener�ci�nk�nt   ##
##      genot�pusuk a h�tr�nyos sz�l� popul�ci�b�l vett random   ##
##      minta (helyettes�t�s v�letlenszer�, szelekci� el�tt)     ##
##  maxt: gener�ci�k sz�ma F2-t�l                                ##
## V�ltoz�k:                                                     ##
##  genot: az egyedek a lista elemei, most a genot�pus m�trix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
##  x: az egyedek karakter �rt�kei (genot sorrendj�ben)          ##
##  record: lista, x t�rol�sa gener�ci�k�nt az �br�zol�shoz      ##
## Szimul�ci� l�p�sei:                                           ##
##  1. inicializ�l�s: P �s F1 popul�ci� l�trehoz�sa (genot, x)   ##
##  2. szimul�ci� (gener�ci�k)                                   ##
##  3. eredm�ny �br�zol�sa                                       ##
## Megjegyz�s: l�sd kvantisim1.r                                 ##
##---------------------------------------------------------------##

m<-10    #lokuszok sz�ma
n<-1000  #popul�ci� egyedsz�ma
maxt<-20 #gener�ci�k sz�ma az F2 gener�ci�t�l
np<-400  #sz�l�k, egyedsz�m (np <= n) 
nm<-300  #helyettesitett egyedek sz�ma

genot<-vector("list",n) #egyed: a genot�pus lista egy eleme
record<-vector("list",maxt+2) #record[[1]] a P, record[[2]] az F1 stb

source("kvantifunct.r") # f�ggv�nyek bet�lt�se

#-------- Inicializ�l�s, P gener�ci�:
#-- Random popul�ci� �sszet�telhez: genot<-randompop(n) 
#-- K�t elt�r� eloszl�s� kiindul�si popul�ci�, m�rete:n/2
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

#--- Tov�bbi gener�ci�k (1 popul�ci�): el�bb helyettes�t�s majd szelekci�
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

