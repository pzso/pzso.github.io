###############################################
## FORR�S:                                   ##
## Soetaert and Herman (2008)                ##
## A practical guide to ecological modelling ##
## Chapter 3.6.4.                            ##
## Competition in a lattice grid -R-VERSION  ##
###############################################

species <- c("A","B","C","D","E")                     # fajok �s szink�djaik
col<-c("grey","lightblue","blue","darkblue","black")

replacement <- matrix(ncol=5,byrow=TRUE,data=c(       # �tmenetek a szomsz�dok f�ggv�ny�ben
1.0,0.02,0.06,0.05,0.03,
0.23,1.0,0.09,0.32,0.37,
0.06,0.08,1.0,0.16,0.09,
0.44,0.06,0.06,1.0,0.11,
0.03,0.02,0.03,0.05,1.0 )
)

#--------- A szimul�ci�
competition <- function(cells,nstep=100,do.plot=10) {
 nind  <- nrow(replacement)
 ncell <- nrow(cells)
 dens  <- matrix(nrow=nstep,ncol=nind)
 for (ss in 1:nstep) {
   dn  <- rbind(cells[ncell,]  ,cells[1:(ncell-1),])
   up  <- rbind(cells[2:ncell,],cells[1,]   )
   le  <- cbind(cells[,2:ncell],cells[,1]   )
   ri  <- cbind(cells[,ncell]  ,cells[,1:(ncell-1)])
   rnd <- matrix(nr=ncell,nc=ncell,runif(ncell*ncell))
   for (i in 1:ncell) {
     for (j in 1:ncell) {
        ii    <- cells[i,j]
        neigb <- c(up[i,j],ri[i,j],dn[i,j],le[i,j])
        p     <- replacement[neigb,ii]
        cump  <- c(cumsum(p/4),1)
        rep   <- min(which(cump>=rnd[i,j] ))
        if (rep<5) cells[i,j] <- neigb[rep]
      }
   }
   for (i in 1:nind) dens[ss,i]<-sum(cells == i)
   if((ss%%do.plot)==0) {
     image(cells,col=col,zlim=c(1,5),axes=FALSE, main=paste(ss,". l�p�s",sep=""))
     box()
     for(i in 1:length(species)) mtext(species[i],side=1,col=col[i],adj=0,at=(i-1)/length(species))
   }
 }
return(list(cells=cells,density=dens))
}

#----- Kezdeti �llapot (t=0)
cells <- matrix(40,40,data=0)                 # sejtek 40x40 m�trixa
cells[,1:8]<-4; cells[,9:16]<-5;              # felt�lt�s a fajokkal
cells[,17:24]<-1; cells[,25:32]<-3; 
cells[,33:40]<-2
image(cells,col=col,zlim=c(1,5),              # �br�zol�s
  axes=FALSE,main="Kiindul�si �llapot")
box()
for(i in 1:length(species)) mtext(species[i],side=1,col=col[i],adj=0,at=(i-1)/length(species))
#text(x=rep(0.1,5),y=seq(0.1,0.9,length.out=5),labels=species[ini],col=c("black",rep("white",4)),adj=0,font=2)

#---- Szimul�ci�
temp<-competition(cells,nstep=200)

#---- �BR�ZOL�S (denzit�s)
#matplot(temp$density,type="l",lwd=2,lty=1,col=col,xlab="Id�",ylab="Denzit�s")
#legend("topleft",species,,lwd=2,col=col)

