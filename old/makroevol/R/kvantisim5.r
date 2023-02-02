##---- Kvantitat�v jellegek, 5. szimul�ci�: szelekci�-g�n�raml�s ##
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
##  maxt: gener�ci�k sz�ma                                       ##
##  tdiv: 1:maxt felbont�sa (pl. csak szelekci�)                 ##
## V�ltoz�k:                                                     ##
##  genot: az egyedek a lista elemei, most a genot�pus m�trix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
##  x: az egyedek karakter �rt�kei (genot sorrendj�ben)          ##
##  record: lista, x t�rol�sa gener�ci�k�nt az �br�zol�shoz      ##
## Szimul�ci� l�p�sei:                                           ##
##  1. inicializ�l�s: P popul�ci� l�trehoz�sa (genot, x)         ##
##  2. szimul�ci� (gener�ci�k)                                   ##
##  3. eredm�ny �br�zol�sa                                       ##
## Megjegyz�s: l�sd kvantisim1.r                                 ##
##---------------------------------------------------------------##

m <- 10           #lokuszok sz�ma
n <- 1000         #popul�ci� egyedsz�ma
maxt <- 50        #gener�ci�k sz�ma
tdiv<-c(9,30)     #g�n�raml�s kezdete
np <- 750         #sz�l�k, egyedsz�m (np <= n) 
np2 <- 750        #sz�l�k a m�sodik popul�ci�ban
nm <- 400         #helyettesitettek sz�ma (cser�lt egyed)
wprob <-dnorm(    #fitnesz (t�l�l�si val�szin�s�g, pl stabiliz�l� szelekci�ra,
 0:(2*m),mean=10,sd=1) #a karakter �rt�kekhez tartoz� s�ly 
wprob2 <-dnorm(   #fitnesz a 2. popul�ci�ra
 0:(2*m),mean=17,sd=1) 

genot<-vector("list",n) #egyed: a genot�pus lista egy eleme
record<-vector("list",maxt) 
record2<-vector("list",maxt) 

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

#--- Inicializ�l�s: random popul�ci� �sszet�tel, k�t azonos m�ret� popul�ci� 
genot<-randompop(n,FALSE)
x<-sapply(genot,charval)
record[[1]]<-x
genot2<-randompop(n,FALSE)
x2<-sapply(genot2,charval)
record2[[1]]<-x2

#--- Tov�bbi gener�ci�k 1. Szelekci� mindk�t popul�ci�ban
for(ss in 2:tdiv[1]) {
 genot<-wsel(genot,np,x,wprob); x<-sapply(genot,charval); record[[ss]]<-x
 genot2<-wsel(genot2,np2,x2,wprob2); x2<-sapply(genot2,charval); record2[[ss]]<-x2
}

#--- Tov�bbi gener�ci�k 2. Csak g�n�raml�s �s panmixis
for(ss in (tdiv[1]+1):tdiv[2]) {
 parent<-genot                  # genot ment�se
 immigr<-sample(n,nm,FALSE)     # g�n�raml�s (genot�pus csere)
 for(i in 1:nm) {
  genot[[immigr[i]]]<-genot2[[immigr[i]]]
  genot2[[immigr[i]]]<-parent[[immigr[i]]]
 }
 x<-sapply(genot,charval); x2<-sapply(genot2,charval);

 parent<-genot[sample(n,np,FALSE)] # panmixis mindk�t popul�ci�ban
 for(i in 1:n) genot[[i]]<-panmixia(parent,1:np)
 x<-sapply(genot,charval);
 parent<-genot2[sample(n,np2,FALSE)]
 for(i in 1:n) genot2[[i]]<-panmixia(parent,1:np2)
 x2<-sapply(genot2,charval);
 record[[ss]]<-x; record2[[ss]]<-x2
}

#--- Tov�bbi gener�ci�k 3. Szelekci� �s g�n�raml�s
for(ss in (tdiv[2]+1):maxt) {
 parent<-genot                  # genot ment�se
 immigr<-sample(n,nm,FALSE)     # g�n�raml�s (genot�pus csere)
 for(i in 1:nm) {
  genot[[immigr[i]]]<-genot2[[immigr[i]]]
  genot2[[immigr[i]]]<-parent[[immigr[i]]]
 }
 x<-sapply(genot,charval); x2<-sapply(genot2,charval)

 genot<-wsel(genot,np,x,wprob); x<-sapply(genot,charval)  # szelekci�
 genot2<-wsel(genot2,np2,x2,wprob2); x2<-sapply(genot2,charval)
 record[[ss]]<-x; record2[[ss]]<-x2
}

#--- �tlagok �br�zol�s
mean1<-sapply(record,mean)
mean2<-sapply(record2,mean)
time<-tdiv[1]:maxt
matplot(time,cbind(mean2[time],mean1[time]),type="l",lty=1,lwd=2,col=c("blue","black"),
  ylab="Karakter �tlag",xlab="Id� (gener�ci�k sz�ma)")
