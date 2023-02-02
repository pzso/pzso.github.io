##---- Kvantitat�v jellegek, 6. szimul�ci�: k�rnyezeti hat�s ----##
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
##  sig2: k�rnyezeti varianca (norm�lis eloszl�s, �tlag a        ##
##      genot�pus �rt�k), a karakter �rt�k val�sz�n�s�g�t        ##
##      hat�rozza meg                                            ##
##  maxt: gener�ci�k sz�ma                                       ##
##  tdiv: 1:maxt felbont�sa (pl. csak szelekci�)                 ##
## V�ltoz�k:                                                     ##
##  genot: az egyedek a lista elemei, most a genot�pus m�trix,   ##
##         ahol az ivarsejt tipus a matrix egy sora              ##
##  genotv: genot�pus �rt�kek (genot sorrendj�ben)               ##
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
maxt <- 50        #gener�ci�k sz�ma az F2 gener�ci�t�l
np <- 750         #sz�l�k, egyedsz�m (np <= n) 
sig2 <- 0.1       #k�rnyezeti variancia (�tlag: genot�pus �rt�k) 

genot<-vector("list",n) #egyed: a genot�pus lista egy eleme
record<-vector("list",maxt) 
x<-vector("numeric",n)

source("kvantifunct.r") #f�ggv�nyek bet�lt�se

#-------- K�sz�b�rt�k (Ir�ny�t�) szelekci�
dsel<-function(agenot,anp,ax) {
 parent<-agenot[order(sign(anp)*ax)[(n-abs(anp)+1):n]]
 for(i in 1:n) agenot[[i]]<-panmixia(parent,1:abs(anp))
 return(agenot)
}

#-------- Karakter �rt�k k�rnyezeti zajjal
charval2<-function(a,sig=sqrt(sig2)) {rnorm(1,a,sig)} #genot�pus �rt�kb�l

#--- Inicializ�l�s: random popul�ci� �sszet�tel
genot<-randompop(n,FALSE)
genotv<-sapply(genot,charval)
for(i in 1:n) x[i]<-charval2(genotv[i])
record[[1]]<-x
#tapply(x,genotv,mean) #karakter �tlag
#hist(x)

#--- Tov�bbi gener�ci�k: Szelekci�
for(ss in 2:maxt) {
 genot<-dsel(genot,np,x);
 genotv<-sapply(genot,charval)
 for(i in 1:n) x[i]<-charval2(genotv[i])
 record[[ss]]<-x
}

#--- �br�zol�s
op<-par(mfrow=c(2,1),mar=c(4,4,3,2)+0.1)
hist(x,ylab="Gyakoris�g",xlab="Karakter",main="Karakter eloszl�s (szelekci� ut�n)")
mean1<-sapply(record,mean)
time<-1:maxt
plot(time,mean1[time],type="l",lty=1,lwd=2,
  ylab="Karakter �tlag",xlab="Id� (gener�ci�k sz�ma)",main="Karakter �tlag v�ltoz�sa")
par(op)
