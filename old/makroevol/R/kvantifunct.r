#------------- FÜGGVÉNYEK A SZIMULÁCIÓKHOZ -----------------------#
# Betöltése a mukakönyvtárból: source("kvantifunct.r")            #
# Genotípus: mátrix, egy sora az ivarsejt típus, oszlopa az allél #
# Általános feltételek:                                           #
#  - diploid egyedek                                              #
#  - két allél lokuszonként a populációban, jele '+' és '-'       #
#  - független szegregálódás és véletlenszerû rekombinálódás      #
# Globális paraméterek (függvények használják):                   #
#  m: lokuszok száma                                              #
#  n: populáció mérete                                            #
#-----------------------------------------------------------------#

#-- Az 'a' genotípusú egyed karakter állapota (karakter értéke)
charval<-function(a) {sum(a=="+")}

reprod<-function(a,b=NULL) {
#-- Szaporodás (a, b genotípusok, m allél)
#-- a és b genotípus mátrix, vagy
#-- ha b NULL, akkor a 2 elemû lista
 if(is.null(b)) {b<- a[[2]]; a<-a[[1]]}
 rbind(
   diag(a[sample(1:2,m,replace=TRUE),]),
   diag(b[sample(1:2,m,replace=TRUE),])
 )
}

#-- Véletlenszerû párválasztás a populációban (gi index vektor): 
panmixia<-function(g,gi=1:n) {reprod(g[sample(gi,2,replace=FALSE)])}

#-- Párok két különbözõ halmazból (g1,g2 index vektor):
matingtype<-function(g,g1,g2) {reprod(g[[sample(g1,1)]],g[[sample(g2,1)]])}

#---- Formázott megjelenítés:

print.genot<-function(a) {
 print(data.frame(genotipus=apply(a,1,paste,collapse="")),row.names=F)
 print(paste("Karakter érték:",charval(a)))
}

plot.chardistr<-function(x,...) {
 if(!is.list(x)) x<-list(x)
 nb<-length(x)
 temp2<-matrix(rep(0,nb*(2*m+1)),nrow=nb)
 for(i in 1:nb) {
  temp<-table(x[[i]])
  temp2[i,as.numeric(names(temp))+1]<-temp
 }
 barplot(temp2,beside=T,names=as.character(0:(2*m)),ylab="Gyakoriság",xlab="Karakter",...)
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
 plot(0,xlim=c(0,(2*m+1)),ylim=c(0,max(sapply(rtable,max))),type="n",ylab="Gyakoriság",xlab="Karakter")
 for(i in 1:nr)
  lines(as.numeric(names(rtable[[i]])),rtable[[i]],col=col[i],lwd=lwd[i])
 if(use.col) legend("topleft",legend=1:nr,fill=1:nr,title="Idõ")
 else legend("topleft",legend=c("1",paste("2-",nr-1,sep=""),paste(nr,sep="")),fill=c("green","gray","blue"),title="Generáció",cex=0.9)
}

#-------- PÉLDA: Inicializálás minta, tiszta leszármazási sorok
pelda<-function() {
#--- Szülõ generáció (P): homozigóták ('+' és '-' minden lokuszra)
 p1<-matrix(rep("+",2*m),nrow=2)
 p2<-matrix(rep("-",2*m),nrow=2)
 print.genot(p1); 
 print.genot(p2);
#--- Az F1 generáció egyértelmû, azonos genotípusú heterozigóták
 f1<-reprod(p1,p2)
 print.genot(f1)
#--- Populáció létrehozása (n egyed)
 genot<-rep(list(f1),n) #n db azonos genotípusú egyed
 x<-sapply(genot,charval) #karakter értékeik (genot sorendjében)
}

randompop<-function(n,do.plot=TRUE) {
 genot<-vector("list",n)
 for(i in 1:n) 
   genot[[i]]<-matrix(sample(c("+","-"),2*m,replace=TRUE),nrow=2)
 if(do.plot) 
   plot.chardistr(sapply(genot,charval)) #vagy pl. egyszerûen hist()
 return(genot)
}