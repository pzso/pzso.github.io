##---- Kvantitat�v jellegek, 4. szimul�ci�: izol�ci� �s f�zi� ---##
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
##      ha kisebb �s negat�v: k�sz�b�rt�k (ir�ny�t�) szelekci�,  ##
##       csak az np darab legkisebb x �rt�k� szaporodik          ##
##      wprob haszn�lata eset�n a kiv�lasztott egyedsz�m         ##
##  wprob: karakter �llapotokhoz tartoz� t�l�l�s (s�lyoz�s), a   ##
##      kiv�laszt�si es�ly (de l�sd ?sample prob param�ter�t)    ##
##      a karakter �llapot (x �rt�ke) f�ggv�ny�ben               ##
##  nm: helyettes�t� egyedek sz�ma a f�zi� sor�n (egy gener�ci�) ##
##      genot�pusuk a m�sik sz�l� popul�ci�b�l vett random       ##
##      minta (helyettes�t�s v�letlenszer�, szelekci� el�tt)     ##
##  maxt: gener�ci�k sz�ma F2-t�l                                ##
##  tdiv: 1:maxt felbont�sa (izol�ci�, majd f�zi�)               ##
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

m <- 10           #lokuszok sz�ma
n <- 1000         #popul�ci� egyedsz�ma
maxt <- 50        #gener�ci�k sz�ma az F2 gener�ci�t�l
tdiv <- c(20,33)  #izol�ci� �s f�zi� (<maxt, gener�ci� sorsz�m)
#tdiv <- c(20,maxt) #izol�ci� f�zi� n�lk�l
np <- 750         #sz�l�k, egyedsz�m (np <= n) 
np2 <- 750        #sz�l�k a m�sodik popul�ci�ban
nm <- 500         #helyettesitettek sz�ma a f�zi� sor�n
wprob <-dnorm(    #fitnesz (t�l�l�si val�szin�s�g, pl stabiliz�l� szelekci�ra,
 0:(2*m),mean=10,sd=2) #a karakter �rt�kekhez tartoz� s�ly 
wprob2 <-dnorm(   #fitnesz a 2. popul�ci�ra
 0:(2*m),mean=17,sd=2) 

genot<-vector("list",n) #egyed: a genot�pus lista egy eleme
record<-vector("list",maxt+2) #record[[1]] a P, record[[2]] az F1 stb

source("kvantifunct.r") #f�ggv�nyek bet�lt�se

#-------- K�sz�b�rt�k (Ir�ny�t�) szelekci�
dsel<-function(agenot,anp,ax) {
 parent<-agenot[order(sign(anp)*ax)[(n-abs(anp)+1):n]]
 for(i in 1:n) agenot[[i]]<-panmixia(parent,1:abs(anp))
 return(agenot)
}

#-------- Szelekci� karakter �llapott�l f�gg� s�lyoz�ssal
wsel<-function(agenot,anp,ax,w) {
 parent<-agenot[sample(n,abs(np),replace=FALSE,prob=w[ax])]
 for(i in 1:n) agenot[[i]]<-panmixia(parent,1:abs(anp))
 return(agenot)
}

#-------- Inicializ�l�s, P gener�ci�
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
#plot.chardistr(list(x[1:nh],x[(nh+1):n]),col=c("red","blue"))
record[[1]]<-x

#--- F1 gener�ci�
parent<-genot; 
for(i in 1:n) genot[[i]]<-matingtype(parent,1:nh,(nh+1):n)
x<-sapply(genot,charval)
#plot.chardistr(list(record[[1]][1:nh],record[[1]][(nh+1):n],x),col=c("red","blue","green"),cex.names=0.9)
record[[2]]<-x

#--- Tov�bbi gener�ci�k (1 popul�ci�)

#--- 1. Egy popul�ci�
for(ss in 1:tdiv[1]) {
 genot<-wsel(genot,np,x,wprob)
 x<-sapply(genot,charval)
 record[[ss+2]]<-x
}

#--- 2. teljes izol�ci�, k�t popul�ci�
genot2<-genot                                       #2. popul�ci� l�trehoz�sa
record2<-record
x2<-x
for(ss in (tdiv[1]+1):tdiv[2]) {
 genot<-wsel(genot,np,x,wprob); x<-sapply(genot,charval); record[[ss+2]]<-x
 genot2<-wsel(genot2,np2,x2,wprob2); x2<-sapply(genot2,charval); record2[[ss+2]]<-x2
}

#--- 3. izol�ci� megsz�nik, f�zi�
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

#--- �tlagok �br�zol�sa
mean1<-sapply(record,mean)
mean2<-sapply(record2,mean)
time<-7:(maxt+1) #eleje burnin, eldobva...
matplot(time,cbind(mean2[time],mean1[time]),type="l",lty=1,lwd=2,col=c("blue","black"),
  ylab="Karakter �tlag",xlab="Id� (gener�ci�k sz�ma)")
