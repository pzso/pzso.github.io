#------------- F�GGV�NYEK A SZIMUL�CI�KHOZ -----------------------#
# Bet�lt�se a mukak�nyvt�rb�l: source("kvantifunct.r")            #
# Genot�pus: m�trix, egy sora az ivarsejt t�pus, oszlopa az all�l #
# �ltal�nos felt�telek:                                           #
#  - diploid egyedek                                              #
#  - k�t all�l lokuszonk�nt a popul�ci�ban, jele '+' �s '-'       #
#  - f�ggetlen szegreg�l�d�s �s v�letlenszer� rekombin�l�d�s      #
# Glob�lis param�terek (f�ggv�nyek haszn�lj�k):                   #
#  m: lokuszok sz�ma                                              #
#  n: popul�ci� m�rete                                            #
#-----------------------------------------------------------------#

#-- Az 'a' genot�pus� egyed karakter �llapota (karakter �rt�ke)
charval<-function(a) {sum(a=="+")}

reprod<-function(a,b=NULL) {
#-- Szaporod�s (a, b genot�pusok, m all�l)
#-- a �s b genot�pus m�trix, vagy
#-- ha b NULL, akkor a 2 elem� lista
 if(is.null(b)) {b<- a[[2]]; a<-a[[1]]}
 rbind(
   diag(a[sample(1:2,m,replace=TRUE),]),
   diag(b[sample(1:2,m,replace=TRUE),])
 )
}

#-- V�letlenszer� p�rv�laszt�s a popul�ci�ban (gi index vektor): 
panmixia<-function(g,gi=1:n) {reprod(g[sample(gi,2,replace=FALSE)])}

#-- P�rok k�t k�l�nb�z� halmazb�l (g1,g2 index vektor):
matingtype<-function(g,g1,g2) {reprod(g[[sample(g1,1)]],g[[sample(g2,1)]])}

#---- Form�zott megjelen�t�s:

print.genot<-function(a) {
 print(data.frame(genotipus=apply(a,1,paste,collapse="")),row.names=F)
 print(paste("Karakter �rt�k:",charval(a)))
}

plot.chardistr<-function(x,...) {
 if(!is.list(x)) x<-list(x)
 nb<-length(x)
 temp2<-matrix(rep(0,nb*(2*m+1)),nrow=nb)
 for(i in 1:nb) {
  temp<-table(x[[i]])
  temp2[i,as.numeric(names(temp))+1]<-temp
 }
 barplot(temp2,beside=T,names=as.character(0:(2*m)),ylab="Gyakoris�g",xlab="Karakter",...)
}

plot.chardistr2<-function(l,use.col=TRUE) {
 rtable<-lapply(l,table); nr<-length(rtable)
 lwd<-rep(1,nr)
 if(use.col) col<-1:nr
 else {
  col<-rep("gray",nr) 
  col[1]<-"green"; col[nr]<-"blue" 
  lwd[1]<-2; lwd[nr]<-2
 }
 plot(0,xlim=c(0,(2*m+1)),ylim=c(0,max(sapply(rtable,max))),type="n",ylab="Gyakoris�g",xlab="Karakter")
 for(i in 1:nr)
  lines(as.numeric(names(rtable[[i]])),rtable[[i]],col=col[i],lwd=lwd[i])
 if(use.col) legend("topleft",legend=1:nr,fill=1:nr,title="Id�")
 else legend("topleft",legend=c("1",paste("2-",nr-1,sep=""),paste(nr,sep="")),fill=c("green","gray","blue"),title="Gener�ci�",cex=0.9)
}

#-------- P�LDA: Inicializ�l�s minta, tiszta lesz�rmaz�si sorok
pelda<-function() {
#--- Sz�l� gener�ci� (P): homozig�t�k ('+' �s '-' minden lokuszra)
 p1<-matrix(rep("+",2*m),nrow=2)
 p2<-matrix(rep("-",2*m),nrow=2)
 print.genot(p1); 
 print.genot(p2);
#--- Az F1 gener�ci� egy�rtelm�, azonos genot�pus� heterozig�t�k
 f1<-reprod(p1,p2)
 print.genot(f1)
#--- Popul�ci� l�trehoz�sa (n egyed)
 genot<-rep(list(f1),n) #n db azonos genot�pus� egyed
 x<-sapply(genot,charval) #karakter �rt�keik (genot sorendj�ben)
}

randompop<-function(n,do.plot=TRUE) {
 genot<-vector("list",n)
 for(i in 1:n) 
   genot[[i]]<-matrix(sample(c("+","-"),2*m,replace=TRUE),nrow=2)
 if(do.plot) 
   plot.chardistr(sapply(genot,charval)) #vagy pl. egyszer�en hist()
 return(genot)
}