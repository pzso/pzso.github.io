##################################################
## Forr�s:                                      ##
## Soetaert and Herman (2008)                   ##
## A practical guide to ecological modelling    ##
## 10. fejezet, Dinamikus programoz�s           ##
## Folt szelekci�s modell (Clark & Mangel 1999) ##
##################################################

#--- Param�terek (�llapot �s id�):
x_crit  <- 0                 # kritikus t�meg a t�l�l�shez
x_max   <- 30                # maxim�lis t�meg
x_rep   <- 4                 # kritikus t�meg a szaporod�shoz
x_cat   <- x_crit:x_max      # t�meg kateg�ri�k (lehets�ges �llapotok)
nmass   <- length(x_cat)     # t�meg kateg�ri�k sz�ma
maxt    <- 20                # id� (peri�dusok sz�ma) 
times   <- 1:(maxt-1)        # peri�dusok

#--- Foltok:
npatch  <- 3                    # foltok sz�ma (habitat)
psurvive <- c(0.99,0.95,0.98)   # t�l�l�si val�sz�n�s�g a foltban
pfood    <- c(0.2 ,0.5 ,0 )     # t�pl�lkoz�si val�sz�n�s�g
cost     <- c(1 ,1 ,1 )         # folt k�lts�g (metabolikus)
feedgain <- c(2 ,4 ,0 )         # nyeres�g (t�pl�lkoz�s)
repr     <- c(0 ,0 ,4 )         # szaporod�si nyeres�g

#--- Eredm�nyek (v�ltoz�k):
f         <- matrix(nrow=maxt,ncol=nmass ,0)    # optimal fitnesz �rt�k
bestpatch <- matrix(nrow=maxt-1,ncol=nmass-1,0) # optim�lis folt v�laszt�s
V         <- vector(length=npatch)              # aktu�lis fitnesz a foltokra

#--- Az utols� peri�dus v�g�n (maxt, Mo)
fend      <- 60
kx        <- 0.25*x_max
f[maxt,]  <- fend*(x_cat-x_crit)/(x_cat-x_crit+kx)

#--- Fitnesz adott x �s t �rt�kre
fitness <- function(x,t) {
 xx <- pmin(x ,x_max) 
 xx <- pmax(xx,x_crit) 
 fitness <- f[t,xx-x_crit+1]
}

#--- Optimaliz�ci�s ciklus
for (t in rev(times)) {              # id�ben visszafel�
 for (x in x_cat[-1]) {              # minden �llapotra (kiv�ve x-crit)
  dfit <- pmax(0,pmin(x-x_rep,repr)) # fitnesz nyeres�g a szaporod�sb�l
  expectgain <- psurvive*(pfood*fitness(x-cost+feedgain-dfit,t+1) + 
                          (1-pfood)*fitness(x-cost-dfit,t+1))
  V <- dfit + expectgain  
  V[expectgain == 0] <- 0           # elpusztul
  f[t,x-x_crit+1]    <- max(V)       # optim�lis fitnesz
  bestpatch[t,x] <- which.max(V)    # optim�lis folt
 }
}

#--- �br�zol�s
cols <- c("black","darkblue","lightblue","white")
image(x=times,y=x_cat[-1],z=bestpatch,ylab="T�meg",xlab="Id�",zlim=c(0,3),
      main="Optim�lis folt",col=cols)
box()
legend("topleft",fill=cols,legend=c("elpusztult","1","2","3"),cex=0.8)
contour(x=1:maxt,y=x_cat,z=f,add=TRUE)
legend("topright",legend="fitnesz",lty=1,cex=0.8)
